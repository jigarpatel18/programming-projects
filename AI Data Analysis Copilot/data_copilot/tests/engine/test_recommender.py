from unittest.mock import patch

from src.engine.recommender import suggest_eda, reflect_on_suggestions

_PROFILE = {"columns": ["income"], "numeric_stats": {"income": {"mean": 52000, "skew": 1.87}}}


# test 1: check whether grounding mode keeps a suggestion that cites real prfoile data  
@patch("src.engine.recommender.gemini_client.generate_json")
def test_suggest_eda_grounded_keeps_valid_suggestion(mock_generate_json):
    mock_generate_json.return_value = [
        {"text": "Income is skewed", "cited_columns": ["income"], "cited_values": [1.87]}
    ]
    result = suggest_eda(_PROFILE, "loan default analysis")
    assert result["data"]["mode"] == "grounded"
    assert len(result["data"]["suggestions"]) == 1


# test 2: make sure that grounded mode also dorps any fabricated_suggestions 
@patch("src.engine.recommender.gemini_client.generate_json")
def test_suggest_eda_grounded_drops_fabricated_suggestion(mock_generate_json):
    mock_generate_json.return_value = [
        {"text": "91% of users churn", "cited_columns": ["age"], "cited_values": [91]}
    ]
    result = suggest_eda(_PROFILE, "loan default analysis")
    assert result["data"]["suggestions"] == []


# test 3: make sure general is the default when no profile is given or loaded in
@patch("src.engine.recommender.gemini_client.generate_json")
def test_suggest_eda_general_mode_when_no_profile(mock_generate_json):
    mock_generate_json.return_value = [
        {"text": "Start with univariate distributions", "cited_columns": [], "cited_values": []}
    ]
    result = suggest_eda(None, "loan default analysis")
    assert result["data"]["mode"] == "general"
    assert len(result["data"]["suggestions"]) == 1


# test 4: ensure general mode drops any suggestions made off of dataest specific citations
@patch("src.engine.recommender.gemini_client.generate_json")
def test_suggest_eda_general_mode_drops_leaked_citation(mock_generate_json):
    mock_generate_json.return_value = [
        {"text": "Your income column has mean 52000", "cited_columns": ["income"], "cited_values": [52000]}
    ]
    result = suggest_eda(None, "loan default analysis")
    assert result["data"]["suggestions"] == []


# test 5: make sure reflect_on_suggestions drops a grounded-mode suggestion with no citations at all
def test_reflect_on_suggestions_drops_uncited_grounded_suggestion():
    raw = [{"text": "Something vague", "cited_columns": [], "cited_values": []}]
    kept, dropped = reflect_on_suggestions(raw, _PROFILE, "grounded")
    assert kept == []
    assert len(dropped) == 1