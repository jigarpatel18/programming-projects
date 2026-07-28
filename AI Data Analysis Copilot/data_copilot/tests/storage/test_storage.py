import pytest
from src.storage.storage_handler import (
    create_session,
    save_message,
    list_sessions,
    load_history,
)


# test 1 crreate a session and check if it returns a valid ID
def test_create_session_returns_uuid():
    session_id = create_session()
    assert len(session_id) == 36


# test 2: check if the message gets saved successfully and returns a success status 
def test_save_message_success():
    session_id = create_session()
    result = save_message(session_id, "user", "What model should I use?")
    assert result == {"status": "success"}


# test 3: check for when a message gets saved with an invalid role and returns an error status 
def test_save_message_invalid_role_error():
    session_id = create_session()
    result = save_message(session_id, "system", "hello")
    assert result["status"] == "error"


# test 4: check that when a message gets saved, it is actually saved and can be retrieved with load_history
def test_save_message_increments_message_number():
    session_id = create_session()
    save_message(session_id, "user", "first")
    save_message(session_id, "assistant", "second")
    assert len(load_history(session_id)) == 2


# test 5: check to see if the history is returned in the correct order and with the correct roles and text
def test_load_history_translates_and_orders():
    session_id = create_session()
    save_message(session_id, "user", "first")
    save_message(session_id, "assistant", "second")

    history = load_history(session_id)

    assert history[0] == {"role": "user", "parts": [{"text": "first"}]}
    assert history[1] == {"role": "model", "parts": [{"text": "second"}]}

