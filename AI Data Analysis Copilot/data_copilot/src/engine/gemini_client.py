
from __future__ import annotations

import json
import os

from dotenv import load_dotenv
from google import genai

load_dotenv()

_API_KEY = os.getenv("GEMINI_API_KEY")
_MODEL = os.getenv("MODEL_NAME") or "gemini-2.5-flash-lite"

_client = None


def _get_client():
    global _client
    if _client is None:
        if not _API_KEY:
            raise RuntimeError("GEMINI_API_KEY is not set. Add it to your .env file.")
        _client = genai.Client(api_key=_API_KEY)
    return _client


def generate_json(prompt: str):
    client = _get_client()
    response = client.models.generate_content(
        model=_MODEL,
        contents=prompt,
        config={"response_mime_type": "application/json"},
    )
    return json.loads(response.text)

def generate_text(prompt: str, history: list[dict] | None = None, profile: dict | None = None, ) -> str:
    client = _get_client()
    if profile:
        prompt = f"""You have access to the following dataset profile.

    {profile}

    Answer the user's question using this dataset whenever it is relevant.

    User Question:
    {prompt}
    """

    contents = list(history or [])
    contents.append(
        {
            "role": "user",
            "parts": [{"text": prompt}],
        }
    )

    response = client.models.generate_content(
        model=_MODEL,
        contents=contents,
    )

    return response.text