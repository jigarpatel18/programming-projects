from unittest.mock import patch, MagicMock

import pytest

import src.engine.gemini_client as gemini_client


# test 1: raise an error for missing API_key
def test_get_client_raises_without_api_key():
    with patch("src.engine.gemini_client._API_KEY", None):
        with pytest.raises(RuntimeError):
            gemini_client._get_client()


# test 2: caches the so only on client is created 
@patch("src.engine.gemini_client.genai.Client")
def test_get_client_caches_client(mock_client_class):
    with patch("src.engine.gemini_client._API_KEY", "fake-key"):
        gemini_client._get_client()
        gemini_client._get_client()
    mock_client_class.assert_called_once()


# test 3: chekc to see if the generate_json parses through the contetn properly
@patch("src.engine.gemini_client._get_client")
def test_generate_json_parses_response(mock_get_client):
    mock_client = MagicMock()
    mock_client.models.generate_content.return_value = MagicMock(text='[{"text": "suggestion"}]')
    mock_get_client.return_value = mock_client

    result = gemini_client.generate_json("some prompt")
    assert result == [{"text": "suggestion"}]


# test 4: make sure generate_json requests JSOn output mode 
@patch("src.engine.gemini_client._get_client")
def test_generate_json_requests_json_mime_type(mock_get_client):
    mock_client = MagicMock()
    mock_client.models.generate_content.return_value = MagicMock(text="{}")
    mock_get_client.return_value = mock_client

    gemini_client.generate_json("some prompt")

    call_kwargs = mock_client.models.generate_content.call_args.kwargs
    assert call_kwargs["config"]["response_mime_type"] == "application/json"


# test 5: check to see if generate_text appends the new prompt onto the exisiting history already saved 
@patch("src.engine.gemini_client._get_client")
def test_generate_text_appends_to_history(mock_get_client):
    mock_client = MagicMock()
    mock_client.models.generate_content.return_value = MagicMock(text="an answer")
    mock_get_client.return_value = mock_client

    history = [{"role": "user", "parts": [{"text": "earlier message"}]}]
    gemini_client.generate_text("new question", history)

    sent_contents = mock_client.models.generate_content.call_args.kwargs["contents"]
    assert len(sent_contents) == 2
    assert sent_contents[-1]["parts"][0]["text"] == "new question"