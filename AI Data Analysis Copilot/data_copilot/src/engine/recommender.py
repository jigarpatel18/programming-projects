from __future__ import annotations

from src.engine import gemini_client

CATEGORY_DESCRIPTIONS = {
    "eda": "Provide practical exploratory data analysis recommendations for the dataset. Focus on data quality checks, summary statistics, feature distributions, relationships between variables, potential outliers, missing values, and visualizations that should be created before modeling. Explain why each recommendation is useful.",
    "models": "Recommend appropriate machine learning models based on the dataset's characteristics. Explain why each model is suitable, discuss important preprocessing steps, evaluation metrics, and any assumptions or limitations the user should consider before training the models.",
    "insights": "Recommend appropriate machine learning models based on the dataset's characteristics. Explain why each model is suitable, discuss important preprocessing steps, evaluation metrics, and any assumptions or limitations the user should consider before training the models.",

}


def suggest_eda(profile: dict | None, topic: str) -> dict:
    return _suggest("eda", profile, topic)


def suggest_models(profile: dict | None, topic: str) -> dict:
    return _suggest("models", profile, topic)


def suggest_insights(profile: dict | None, topic: str) -> dict:
    return _suggest("insights", profile, topic)


def _suggest(category: str, profile: dict | None, topic: str) -> dict:
    mode = "grounded" if profile else "general"
    raw_suggestions = _generate_raw(category, profile, topic, mode)
    kept, _dropped = reflect_on_suggestions(raw_suggestions, profile, mode)
    return {"status": "success", "data": {"mode": mode, "suggestions": kept}}


# build prompt for gemeini call 
def _generate_raw(category: str, profile: dict | None, topic: str, mode: str) -> list[dict]:
    description = CATEGORY_DESCRIPTIONS[category]

    if mode == "grounded":
        prompt = f"""You are a data analysis copilot. A dataset has been loaded with this profile:
{profile}

The user's topic/goal: "{topic}"

Suggest 3-5 {description} specific to this dataset.
Only cite columns and values that actually appear in the profile above — do not invent statistics.

Return ONLY a JSON list, each item shaped exactly like:
{{"text": "...", "cited_columns": ["..."], "cited_values": [...]}}
cited_columns must be real column names from the profile.
cited_values must be real numbers copied from the profile (means, stds, min/max, etc)."""
    else:
        prompt = f"""You are a data analysis copilot. No dataset has been loaded yet.

The user's topic/goal: "{topic}"

Suggest 3-5 general {description} appropriate for this kind of topic, based on domain
knowledge only. Do NOT reference specific column names or statistics, and do not imply
you have seen real data — you have not.

Return ONLY a JSON list, each item shaped exactly like:
{{"text": "...", "cited_columns": [], "cited_values": []}}
cited_columns and cited_values must always be empty in this mode."""

    return gemini_client.generate_json(prompt)


# look for all numeric vals so it can be validated against suggestions
def _flatten_profile_values(profile: dict) -> set[str]:
    found = set()

    def walk(node):
        if isinstance(node, dict):
            for v in node.values():
                walk(v)
        elif isinstance(node, list):
            for v in node:
                walk(v)
        elif isinstance(node, (int, float)):
            found.add(str(node))

    walk(profile)
    return found


# cross check to make sure any values not found in profile are removed for the suggestions
def reflect_on_suggestions(raw_suggestions: list[dict], profile: dict | None, mode: str) -> tuple[list[dict], list[dict]]:
    kept, dropped = [], []

    if mode == "general":
        for s in raw_suggestions:
            if s.get("cited_columns") or s.get("cited_values"):
                dropped.append(s)
            else:
                kept.append({"text": s.get("text", ""), "cited_columns": [], "cited_values": []})
        return kept, dropped

    # grounded mode
    real_columns = set((profile or {}).get("columns", []))
    real_values = _flatten_profile_values(profile or {})

    for s in raw_suggestions:
        cited_columns = s.get("cited_columns", [])
        cited_values = s.get("cited_values", [])

        if not cited_columns and not cited_values:
            dropped.append(s)
            continue

        columns_ok = all(c in real_columns for c in cited_columns)
        values_ok = all(str(v) in real_values for v in cited_values)

        if columns_ok and values_ok:
            kept.append({
                "text": s.get("text", ""),
                "cited_columns": cited_columns,
                "cited_values": cited_values,
            })
        else:
            dropped.append(s)

    return kept, dropped