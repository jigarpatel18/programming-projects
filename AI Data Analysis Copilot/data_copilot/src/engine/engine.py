from __future__ import annotations
import re


from src.engine.profiler import (
    load_dataset as profiler_load_dataset,
    build_profile,
)

from src.engine.recommender import (
    suggest_eda,
    suggest_models,
    suggest_insights,
)

from src.engine.gemini_client import (
    generate_json,
    generate_text,
)

from src.storage.storage_handler import (
    create_session,
    save_message,
    load_history,
    list_sessions,
    get_sessions,
)

_TITLE_PROMPT = """
Create a short title for this data analysis chat.

Rules:
- Maximum 5 words
- Be descriptive
- No quotes
- No punctuation

User message:
{text}

Return only the title.
""".strip()


_EXTRACTION_PROMPT = """
You are a routing engine for a data analysis assistant.

Determine what the user wants.

Return ONLY JSON:

{
    "intent": "eda",
    "dataset": ""
}

Possible intents:
- load_dataset
- eda
- models
- insights
- general_qa
- unclear

Rules:
- If the user mentions a dataset filename (example: heart.csv), extract it into "dataset".
- If the user wants analysis, EDA, modeling, or insights, classify accordingly.
- If no dataset is mentioned, leave dataset as an empty string.
""".strip()


def load_dataset_profile(path: str) -> dict:
    try:
        df = profiler_load_dataset(path)
        profile = build_profile(df)

        return {
            "status": "success",
            "data": profile,
            "message": f"Dataset '{path}' loaded successfully.",
        }

    except Exception as exc:
        return {
            "status": "error",
            "message": str(exc),
        }


def _load_profile_if_needed(dataset: str, current_profile: dict | None):

    if not dataset:
        return current_profile

    try:
        df = profiler_load_dataset(dataset)
        return build_profile(df)

    except Exception:
        return current_profile


def set_topic(topic: str) -> dict:
    return {
        "status": "success",
        "data": {"topic": topic.strip()},
    }

 # using manual classificatio for intent to reduce api calls to gemini
def _classify_intent(user_input: str) -> dict:

    text = user_input.lower().strip()

    # Look for a CSV filename
    match = re.search(r"([\w\-]+\.csv)", text)
    dataset = match.group(1) if match else ""

    # Dataset loading
    if any(word in text for word in [
        "load",
        "open",
        "read",
        "import",
        "use"
    ]) and dataset:
        return {
            "intent": "load_dataset",
            "dataset": dataset,
        }

    # EDA
    if any(word in text for word in [
        "eda",
        "exploratory",
        "explore",
        "distribution",
        "missing",
        "summary",
        "visualize",
        "visualization",
        "correlation",
    ]):
        return {
            "intent": "eda",
            "dataset": dataset,
        }

    # Models
    if any(word in text for word in [
        "model",
        "models",
        "algorithm",
        "algorithms",
        "regression",
        "classification",
        "predict",
        "prediction",
        "random forest",
        "xgboost",
    ]):
        return {
            "intent": "models",
            "dataset": dataset,
        }

    # Insights
    if any(word in text for word in [
        "insight",
        "insights",
        "analyze",
        "analysis",
        "look for",
        "patterns",
        "interesting",
        "investigate",
        "findings",
    ]):
        return {
            "intent": "insights",
            "dataset": dataset,
        }

    # Default
    return {
        "intent": "general_qa",
        "dataset": dataset,
    }

# takes user input, classifies it with proper intent and returns a response for the input, and saves messsage as well
def handle_message(
    session_id: str,
    user_input: str,
    profile: dict | None,
    topic: str,
) -> dict:
    try:
        classification = _classify_intent(user_input)

        intent = classification["intent"]

        dataset = classification.get("dataset", "")

        profile = _load_profile_if_needed(
            dataset,
            profile
        )

        # look for the intent of the user prompt 
        if intent == "eda":
            result = suggest_eda(profile, topic)

        elif intent == "models":
            result = suggest_models(profile, topic)

        elif intent == "insights":
            result = suggest_insights(profile, topic)

        elif intent == "general_qa":
            history = load_history(session_id)
            augmented_prompt = user_input
            if profile:
                augmented_prompt = (
                    f"The user has a dataset loaded with this profile: \n{profile}\n Only reference specific columns or values from this profile if relevant and accurate - do not invent dataset facts that aren't here. \n\nUser question: {user_input}"
                )
            answer = generate_text(augmented_prompt, history, profile)
            result = {
                "status": "success",
                "data": answer,
                "profile": profile,
            }
        
        elif intent == "load_dataset":
                    result = load_dataset_profile(dataset)
                    if result["status"] == "success":
                        profile = result["data"]
                        cols = profile.get("columns", [])
                        result = {
                            "status": "success",
                            "data": f"Loaded '{dataset}' — {profile.get('row_count')} rows, "
                                    f"{len(cols)} columns. You can now ask for EDA, models, or insights."
                        }
        else:
            return {
                "status": "unclear",
                "message": "I'm not sure what you're asking. Try asking for EDA suggestions, model recommendations, insights, or a general question.",
                "profile": profile,
                "topic": topic,
            }
        
        sessions = get_sessions()

        if sessions.get("status") == "success":
            current = [
                s for s in sessions["data"]
                if s["session_id"] == session_id
            ]

            if current and not current[0].get("title"):
                title = _generate_session_title(user_input)

                update_session_title(
                    session_id,
                    title
                )

        # save message to the session history 
        save_message(session_id, "user", user_input)

        if intent == "general_qa":
            save_message(session_id, "assistant", result["data"])
        else:
            save_message(session_id, "assistant", str(result["data"]))

        result["profile"] = profile
        result["topic"] = topic
        return result

    except Exception as exc:
        return {
            "status": "error",
            "message": str(exc),
            "profile": profile,
            "topic": topic,
        }


# make a short title for each session, making it easier to track in the google spreadsheet
def _generate_session_title(user_input: str) -> str:
    try:
        title = generate_text(
            _TITLE_PROMPT.format(text=user_input),
            []
        )

        return title.strip()

    except Exception:
        return "Data Analysis Chat"

# start a new session
def new_session() -> dict:
    try:
        session_id = create_session()

        return {
            "status": "success",
            "data": session_id,
        }

    except Exception as exc:
        return {
            "status": "error",
            "message": str(exc),
        }



# called for every user message 
def process_request(
    session_id,
    user_input,
    profile=None,
    topic=""
):
    return handle_message(
        session_id,
        user_input,
        profile,
        topic
    )