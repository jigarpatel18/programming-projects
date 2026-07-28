from unittest.mock import patch

from src.interface.cli import format_response, run_session


# test 1: check for success status with a string data
def test_format_response_success_string():
    result = {"status": "success", "data": "Random Forest is a strong baseline here."}
    assert format_response(result) == "Random Forest is a strong baseline here."


# test 2: check for success status with list of suggestions
def test_format_response_success_suggestions_lists_each_one():
    result = {
        "status": "success",
        "data": {"mode": "grounded", "suggestions": [{"text": "Check for skew"}, {"text": "Check for outliers"}]},
    }
    output = format_response(result)
    assert "1. Check for skew" in output
    assert "2. Check for outliers" in output


# test 3: make sure if the status is unclear, a help test is given to user
def test_format_response_unclear_includes_help_text():
    result = {"status": "unclear", "message": "Could you clarify?"}
    output = format_response(result)
    assert "Could you clarify?" in output
    assert "Examples:" in output


# test 4: check to see if the error message is retruend is status is error
def test_format_response_error_returns_message():
    result = {"status": "error", "message": "File not found"}
    assert format_response(result) == "File not found"


# tests 5: make sure run_session prints in the format_response output and handles user input correctly
@patch("src.interface.cli.new_session")
def test_run_session_prints_formatted_response(mock_new_session, monkeypatch, capsys):
    mock_new_session.return_value = {"status": "success", "data": "session-123"}
    mock_process_fn = lambda session_id, user_input, profile, topic: {
        "status": "success", "data": "mocked answer", "profile": None,
    }

    inputs = iter(["what model should I use?", "quit"])
    monkeypatch.setattr("builtins.input", lambda _: next(inputs))

    run_session(process_fn=mock_process_fn)

    assert "mocked answer" in capsys.readouterr().out


# test 7: check to see if the session ends cleany when user prompts quit or exit
@patch("src.interface.cli.new_session")
def test_run_session_exits_cleanly_on_quit(mock_new_session, monkeypatch, capsys):
    mock_new_session.return_value = {"status": "success", "data": "session-123"}

    inputs = iter(["quit"])
    monkeypatch.setattr("builtins.input", lambda _: next(inputs))

    called = []
    run_session(process_fn=lambda *a, **k: called.append(True))

    assert called == []
    assert "session ended" in capsys.readouterr().out.lower()
