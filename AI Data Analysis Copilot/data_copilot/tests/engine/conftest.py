import pytest
import src.engine.gemini_client as gemini_client


@pytest.fixture(autouse=True)
def reset_gemini_client_cache():
    gemini_client._client = None
    yield
    gemini_client._client = None