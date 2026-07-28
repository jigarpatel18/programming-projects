# Storage layer 

from __future__ import annotations

import os
import uuid
from datetime import datetime, timezone
from pathlib import Path

try:
    import gspread
    from google.oauth2.service_account import Credentials
except ImportError:  
    gspread = None
    Credentials = None


VALID_ROLES = {"user", "assistant"}
_ROLE_TO_GEMINI = {"user": "user", "assistant": "model"}

_PROJECT_ROOT = Path(__file__).resolve().parent.parent.parent
_SERVICE_ACCOUNT_PATH = _PROJECT_ROOT / "service_account.json"

SCOPES = [
    "https://www.googleapis.com/auth/spreadsheets",
    "https://www.googleapis.com/auth/drive",
]

SPREADSHEET_NAME = os.environ.get("SPREADSHEET_NAME", "data-copilot-memory")

SESSIONS_HEADERS = ["session_id", "title", "created_at", "last_updated"]
MESSAGES_HEADERS = ["session_id", "message_number", "role", "message", "timestamp"]


# opens and connects to the sessions worksheet 
def _get_sessions_sheet():
    creds = Credentials.from_service_account_file(str(_SERVICE_ACCOUNT_PATH), scopes=SCOPES)
    client = gspread.authorize(creds)
    sheet = client.open(SPREADSHEET_NAME).worksheet("Sessions")
    if not sheet.get_all_values():
        sheet.append_row(SESSIONS_HEADERS)
    return sheet

# opens the messages worksheet and conencts to it
def _get_messages_sheet():
    creds = Credentials.from_service_account_file(str(_SERVICE_ACCOUNT_PATH), scopes=SCOPES)
    client = gspread.authorize(creds)
    sheet = client.open(SPREADSHEET_NAME).worksheet("Messages")
    if not sheet.get_all_values():
        sheet.append_row(MESSAGES_HEADERS)
    return sheet


# time for svaing the sessions and messages time, as they are being saved
def _now() -> str:
    return datetime.now(timezone.utc).isoformat()

# stats a new session eveyrtime cli.py is run in a new terminal window
def create_session() -> str:
    session_id = str(uuid.uuid4())
    now = _now()
    sheet = _get_sessions_sheet()
    sheet.append_row([session_id, "", now, now])
    return session_id

#save chat message under the sessions it was prompted in
def save_message(session_id: str, role: str, message: str) -> dict:
    try:
        if role not in VALID_ROLES:
            return {"status": "error", "message": f"invalid role: {role}"}

        if not message or not message.strip():
            return {"status": "error", "message": "message cannot be empty"}

        sessions = _get_sessions_sheet()
        session_row = _find_row_by_session_id(sessions, session_id)
        if session_row is None:
            return {"status": "error", "message": f"no session with id {session_id}"}

        messages = _get_messages_sheet()
        existing = [row for row in messages.get_all_records() if row["session_id"] == session_id]
        next_message_number = len(existing) + 1

        messages.append_row([session_id, next_message_number, role, message, _now()])
        sessions.update_cell(session_row, 4, _now())  # column 4 = last_updated

        return {"status": "success"}
    except Exception as e:
        return {"status": "error", "message": str(e)}

# helper function for save_message to find the row os session id in the worksheet
def _find_row_by_session_id(sheet, session_id):
    records = sheet.get_all_records()

    for i, record in enumerate(records, start=2):  # row 1 is headers
        if record["session_id"] == session_id:
            return i

    return None

# also a helper function for save_message and allows for test making to be easier 
def load_history(session_id: str) -> list[dict]:
    try:
        messages = _get_messages_sheet()
        rows = [row for row in messages.get_all_records() if row["session_id"] == session_id]
        rows.sort(key=lambda r: r["message_number"])
        return [
            {"role": _ROLE_TO_GEMINI[row["role"]], "parts": [{"text": row["message"]}]}
            for row in rows
        ]
    except Exception:
        return []

# fetches all sessions and returns them in a list of dicts
def list_sessions() -> dict:
    try:
        sessions = _get_sessions_sheet()
        rows = sessions.get_all_records()
        rows.sort(key=lambda r: r["last_updated"], reverse=True)
        return {"status": "success", "data": rows}
    except Exception as e:
        return {"status": "error", "message": str(e)}

# recieves all the sessions from spreadsheet and returns them in a list of dicts 
def get_sessions() -> dict:
    try:
        storage = _get_sessions_sheet()  # or whatever function loads your session data   
        # or whatever function loads your session data

        return {
            "status": "success",
            "data": storage.get("sessions", [])
        }

    except Exception as exc:
        return {
            "status": "error",
            "message": str(exc)
        }

