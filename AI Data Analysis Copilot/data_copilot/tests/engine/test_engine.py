from unittest.mock import patch

from src.engine.engine import (
    handle_message,
    load_dataset_profile,
    set_topic,
)


# test 1: should return error for when the dataset profile is unable to be loaded
def test_load_dataset_profile_reports_error():
    result = load_dataset_profile("nonexistent_file_123.csv")
    assert result["status"] == "error"
    assert result["message"]


# test 2: make sure set_topic should remove unecessary whitespace from topic string
def test_set_topic_strips_whitespace():
    result = set_topic("  loan default prediction  ")
    assert result == {"status": "success", "data": {"topic": "loan default prediction"}}


# test 3: handle_message returns error when load_dataset fails
@patch("src.engine.engine.save_message")
@patch("src.engine.engine.get_sessions")
@patch("src.engine.engine.load_dataset_profile")
@patch("src.engine.engine._classify_intent")
def test_handle_message_load_dataset_intent(mock_classify, mock_load, mock_get_sessions, mock_save):
    mock_classify.return_value = {
        "intent": "load_dataset",
        "dataset": "sales.csv"
    }

    mock_load.return_value = {
        "status": "success",
        "data": {}
    }

    mock_get_sessions.return_value = {
        "status": "success",
        "data": []
    }

    result = handle_message("session-1", "load sales.csv", None, "")

    assert result["status"] == "success"
    mock_load.assert_called_once_with("sales.csv")


# test 4: check to see if profile is returned so it is valid for general_qa intent and is passed to generate_text
@patch("src.engine.engine.save_message")
@patch("src.engine.engine.load_history", return_value=[])
@patch("src.engine.engine.generate_text", return_value="some answer")
@patch("src.engine.engine._classify_intent")
def test_handle_message_returns_profile(mock_classify, mock_generate_text, mock_history, mock_save):
    mock_classify.return_value = {"intent": "general_qa", "dataset": ""}
    existing_profile = {"columns": ["income"], "row_count": 5}

    result = handle_message("session-1", "what's next?", existing_profile, "")

    assert result["profile"] == existing_profile


# test 5: make sure gen_qa intent is correct and passed to Gemini with correct prompt and profile
@patch("src.engine.engine.save_message")
@patch("src.engine.engine.load_history", return_value=[])
@patch("src.engine.engine.generate_text")
@patch("src.engine.engine._classify_intent")
def test_general_qa_receives_profile_context(mock_classify, mock_generate_text, mock_history, mock_save):
    mock_classify.return_value = {"intent": "general_qa", "dataset": ""}
    mock_generate_text.return_value = "answer"
    profile = {"columns": ["income"], "row_count": 100}

    handle_message("session-1", "what about missing income values?", profile, "")

    sent_prompt = mock_generate_text.call_args[0][0]
    assert "income" in sent_prompt


