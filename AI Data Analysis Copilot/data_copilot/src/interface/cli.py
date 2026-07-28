# presentation layer; what the user sees through terminal

from __future__ import annotations

from src.engine.engine import (
    process_request,
    new_session,
)

_WELCOME_BANNER = """
========================================
        Data Copilot
 AI Data Analysis Assistant

Type 'quit' or 'exit' to leave.
========================================
"""

_HELP_TEXT = (
    "Examples:\n"
    "  • Load iris.csv\n"
    "  • Perform EDA on heart.csv\n"
    "  • Recommend machine learning models\n"
    "  • Give me insights to investigate\n"
    "  • Ask a general data science question"
)


#turns the angine response into a readeable text for terminal 
def format_response(result: dict) -> str:
    status = result.get("status")

    if status == "success":
        data = result.get("data")

        if isinstance(data, str):
            return data

        if isinstance(data, dict):

            if "suggestions" in data:
                lines = []

                for i, suggestion in enumerate(data["suggestions"], start=1):
                    lines.append(
                        f"{i}. {suggestion['text']}"
                    )

                return "\n".join(lines)

            return str(data)

        return result.get("message", "Success.")

    if status == "unclear":
        return (
            result.get("message", "I couldn't determine your request.")
            + "\n\n"
            + _HELP_TEXT
        )

    if status == "error":
        return result.get("message", "Something went wrong.")

    return "Unexpected response."


# starts the session and continues run until exit or quit is typed
def run_session(process_fn=None):

    if process_fn is None:
        process_fn = process_request

    print(_WELCOME_BANNER)

    # persistent session state
    session_result = new_session()

    if session_result.get("status") == "success":
        session_id = session_result["data"]
    else:
        session_id = None

    profile = None
    topic = ""

    while True:

        try:
            user_input = input("You: ").strip()

        except EOFError:
            print("\nCoPilot session ended.")
            return


        if not user_input:
            continue

        # read types of user inputs and either quit program or send request to the engine
        if user_input.lower() in {"quit", "exit"}:
            print("CoPilot session ended.")
            return

        if user_input.lower() == "history":
            result = list_sessions()

            if result["status"] == "success":
                print("\nPrevious Sessions:")
                for session in result["data"]:
                    print(f"- {session['title']} ({session['session_id']})")
            else:
                print(result["message"])

            continue

        if user_input.lower() == "save":
            print("Conversation is automatically saved.")
            continue
    

        result = process_fn(
            session_id,
            user_input,
            profile,
            topic,
        )

        # receive the response from the engine and update the session state if profile or topic is returned

        if "profile" in result:
            profile = result["profile"]

        if "topic" in result:
            topic = result["topic"]


        print("Copilot Response:")
        print(format_response(result))
        print()

if __name__ == "__main__":
    run_session()