# AGENT_PROMPTS.md


# Prompt 1: Storage Layer

## Core Implementation Prompt


```text
Before implementing anything, follow these constraints:
1. Respect layer boundaries: interface has no AI calls and no storage access.
   engine never imports storage internals directly except through the
   functions defined in storage_handler.py. Only recommender.py (via its
   Gemini client setup) may call the Gemini API.
2. Do not invent data. Any function generating text about a dataset can only
   state numbers/columns that actually appear in the profile_payload it was
   given. Do not fabricate statistics to make a suggestion sound complete.
3. Match existing function signatures exactly as defined in CONTRACT.md.
   Do not rename parameters or change return shapes to make your
   implementation easier — ask me first if a contract seems wrong.
4. Never hardcode API keys or credentials. Load them via os.getenv() only.
5. Run pytest before and after your change. Do not weaken or delete a test
   to make it pass — tell me if a test seems wrong instead of changing it.
```

```text
I am implementing the Storage layer for data_copilot.
Context: Read FUNCTIONALITY.md, CONTRACT.md, and tests/storage/test_storage.py.
Goal: Implement src/storage/storage_handler.py.
Requirements:
1. Use [Firebase Admin SDK / gspread — pick one] for persistence.
2. Authentication must use a credentials file (service_account.json or
   Firebase equivalent) — no hardcoded secrets.
3. Implement save_session(session_data: dict) -> dict.
   Success: {"status": "success", "id": "session_123"}
   Failure: {"status": "error", "message": "..."}
   No duplicate check needed — sessions aren't unique by any natural key.
4. Implement get_sessions() -> dict.
   Success: {"status": "success", "data": [session_record, ...]}
   Failure: {"status": "error", "message": "..."}
5. If session_data is missing required keys (topic, timestamp), return
   {"status": "error", "message": "..."} rather than saving a partial record.
6. Keep logic modular and easy to test.
```

## Guardrail Prompts

- Hardcoded keys issue:
  - `Ensure key names match CONTRACT.md exactly (for example: session_id, not sessionID).`
- Credential safety issue:
  - `Load credentials from the service account file path via os.getenv() or a config module; do not embed private keys in source code.`
- Wrong return contract:
  - `save_session must return exactly {"status": "success", "id": ...} or {"status": "error", "message": ...} — no other shapes.`
- Saving incomplete sessions:
  - `Validate that session_data contains "topic" and "timestamp" before writing; return "error" if either is missing rather than writing a partial record.`

## Verification Prompt

```text
Now run tests and explain which test validates each required behavior:
- save_session success path
- save_session error path (missing required fields)
- get_sessions returns a list of saved sessions
- get_sessions returns an empty list gracefully when nothing is saved
If any test fails, update the implementation only — do not weaken the tests.
```

---

# Prompt 2: Profiler (engine, no AI)

## Core Implementation Prompt

```text
Before implementing anything, follow these constraints:
1. Respect layer boundaries: interface has no AI calls and no storage access.
   engine never imports storage internals directly except through the
   functions defined in storage_handler.py. Only recommender.py (via its
   Gemini client setup) may call the Gemini API.
2. Do not invent data. Any function generating text about a dataset can only
   state numbers/columns that actually appear in the profile_payload it was
   given. Do not fabricate statistics to make a suggestion sound complete.
3. Match existing function signatures exactly as defined in CONTRACT.md.
   Do not rename parameters or change return shapes to make your
   implementation easier — ask me first if a contract seems wrong.
4. Never hardcode API keys or credentials. Load them via os.getenv() only.
5. Run pytest before and after your change. Do not weaken or delete a test
   to make it pass — tell me if a test seems wrong instead of changing it.
```

```text
I am implementing the Profiler module for data_copilot.
Context: Read CONTRACT.md, FUNCTIONALITY.md, and tests/engine/test_profiler.py.
Goal: Implement src/engine/profiler.py.
Requirements:
1. load_dataset(path: str) -> pandas.DataFrame
   - Support .csv and .xlsx files based on file extension.
   - Raise a clear, specific exception (FileNotFoundError for missing files,
     ValueError for unsupported extensions) — do not silently return None.
2. build_profile(df: pandas.DataFrame) -> dict
   Must return exactly this shape (matches profile_payload in CONTRACT.md):
   {
     "columns": [...],
     "row_count": int,
     "missing_by_column": {col: int, ...},
     "numeric_stats": {col: {"mean": ..., "std": ..., "min": ..., "max": ..., "skew": ...}, ...},
     "categorical_stats": {col: {"top_value": ..., "top_freq": ..., "n_unique": ...}, ...}
   }
3. This module makes NO Gemini/AI calls — it is pure pandas logic and must
   be fully deterministic (same input always produces same output).
4. Handle empty or unreadable files by raising an exception the caller
   (engine.py) can catch — do not crash silently or return an empty dict.
```

## Guardrail Prompts

- Wrong profile shape:
  - `Match the exact key names in profile_payload from CONTRACT.md — do not rename "missing_by_column" to "missing_values" or similar.`
- Silent failure on bad files:
  - `load_dataset must raise FileNotFoundError or ValueError with a clear message — never return None or an empty DataFrame silently.`
- Non-deterministic output:
  - `Do not include any randomness, current timestamps, or AI calls in profiler.py — every function here must be a pure, deterministic function of its input.`

## Verification Prompt

```text
Now run tests and explain which test validates each required behavior:
- load_dataset successfully parses a .csv fixture
- load_dataset successfully parses a .xlsx fixture
- load_dataset raises on a missing file path
- load_dataset raises on an unsupported extension
- build_profile returns correct row_count, missing_by_column, numeric_stats
  and categorical_stats for a small fixture DataFrame
If any test fails, fix the implementation only — do not modify or weaken the tests.
```

---

# Prompt 3: Recommender (engine, Tool Use + Reflection)

## Core Implementation Prompt

```text
Before implementing anything, follow these constraints:
1. Respect layer boundaries: interface has no AI calls and no storage access.
   engine never imports storage internals directly except through the
   functions defined in storage_handler.py. Only recommender.py (via its
   Gemini client setup) may call the Gemini API.
2. Do not invent data. Any function generating text about a dataset can only
   state numbers/columns that actually appear in the profile_payload it was
   given. Do not fabricate statistics to make a suggestion sound complete.
3. Match existing function signatures exactly as defined in CONTRACT.md.
   Do not rename parameters or change return shapes to make your
   implementation easier — ask me first if a contract seems wrong.
4. Never hardcode API keys or credentials. Load them via os.getenv() only.
5. Run pytest before and after your change. Do not weaken or delete a test
   to make it pass — tell me if a test seems wrong instead of changing it.
```

```text
I am implementing the Recommender module for data_copilot.
Context: Read CONTRACT.md (especially Section 5, Grounding Check),
         FUNCTIONALITY.md, and tests/engine/test_recommender.py.
Goal: Implement src/engine/recommender.py with three functions:
      suggest_eda(profile, topic), suggest_models(profile, topic),
      suggest_insights(profile, topic).
Requirements:
1. Each function accepts profile: dict | None and topic: str.
2. If profile is not None ("grounded" mode):
   - Call Gemini with a prompt that includes the real profile_payload data
     and instructs the model to only cite columns/values present in it.
   - The model's raw response must include, per suggestion, the specific
     columns and values it's citing (cited_columns, cited_values).
3. If profile is None ("general" mode):
   - Call Gemini with a prompt for domain-general advice about the topic,
     explicitly instructing it NOT to reference specific columns, stats, or
     any claim that implies it has seen real data.
4. Reflection step (run AFTER the Gemini call, BEFORE returning):
   - Grounded mode: drop any suggestion whose cited_columns or cited_values
     don't actually appear in profile. This must be a real check against
     the profile dict, not just trusting the model.
   - General mode: drop or reject any suggestion containing specific
     numbers, column names, or dataset-specific claims, since none should
     exist when there's no profile.
5. Return contract for all three functions:
   {"status": "success", "data": {"mode": "grounded"|"general", "suggestions": [...]}}
   Each suggestion in the final output: {"text": "...", "cited_columns": [...], "cited_values": [...]}
   (cited_columns/cited_values must be empty lists in general mode.)
6. Enforce JSON-only output from Gemini using the SDK's structured output
   option (response_mime_type or equivalent) so responses are reliably parseable.
7. Load the API key via os.getenv() only — never hardcode it.
```

## Guardrail Prompts

- Ungrounded suggestion slipping through:
  - `The Reflection check must actually compare cited_columns/cited_values against the real profile dict — do not just trust that the model followed instructions in the prompt.`
- General mode leaking fake specifics:
  - `In general mode, scan each suggestion for numbers or column-like terms before returning it. If cited_columns or cited_values is non-empty in general mode, drop that suggestion — it should never happen.`
- Model response not parseable as JSON:
  - `The Gemini call must enforce JSON-only output (response_mime_type or equivalent in the SDK). Do not attempt to regex-parse free text.`
- Hardcoded API key:
  - `Load the API key from os.getenv() only. Never write the key value in source code.`
- Reflection applied inconsistently across the three functions:
  - `suggest_eda, suggest_models, and suggest_insights must all run the same grounding/anti-fabrication logic — consider factoring the Reflection check into one shared helper function they all call.`

## Verification Prompt

```text
Now run tests and explain which test validates each required behavior:
- suggest_eda (grounded) returns suggestions citing real profile columns
- suggest_eda (grounded) drops a mocked suggestion citing a fake column/value
- suggest_eda (general, profile=None) returns suggestions with empty
  cited_columns/cited_values
- suggest_eda (general) drops a mocked suggestion that fabricates dataset specifics
- Same four behaviors verified for suggest_models and suggest_insights
If any test fails, fix the implementation only — do not modify or weaken the tests.
```

---

# Prompt 4: Engine Orchestration

## Core Implementation Prompt

```text
Before implementing anything, follow these constraints:
1. Respect layer boundaries: interface has no AI calls and no storage access.
   engine never imports storage internals directly except through the
   functions defined in storage_handler.py. Only recommender.py (via its
   Gemini client setup) may call the Gemini API.
2. Do not invent data. Any function generating text about a dataset can only
   state numbers/columns that actually appear in the profile_payload it was
   given. Do not fabricate statistics to make a suggestion sound complete.
3. Match existing function signatures exactly as defined in CONTRACT.md.
   Do not rename parameters or change return shapes to make your
   implementation easier — ask me first if a contract seems wrong.
4. Never hardcode API keys or credentials. Load them via os.getenv() only.
5. Run pytest before and after your change. Do not weaken or delete a test
   to make it pass — tell me if a test seems wrong instead of changing it.
```

```text
I am implementing engine.py orchestration for data_copilot.
Context: Read CONTRACT.md, FUNCTIONALITY.md, src/engine/profiler.py,
         src/engine/recommender.py, src/storage/storage_handler.py,
         and tests/engine/test_engine.py.
Goal: Implement src/engine/engine.py with:
      load_dataset(path), set_topic(topic), handle_message(user_input, profile, topic),
      save_session(session_data), get_sessions().
Requirements:
1. load_dataset(path: str) -> dict
   Calls profiler.load_dataset then profiler.build_profile.
   Success: {"status": "success", "data": profile_payload}
   Failure: {"status": "error", "message": "..."} (catch profiler's exceptions here)
2. set_topic(topic: str) -> dict
   Returns {"status": "success", "data": {"topic": topic}}.
3. handle_message(user_input: str, profile: dict | None, topic: str | None) -> dict
   Step 1 — Tool Use: call Gemini to classify user_input into one of
   "eda" | "models" | "insights" | "general_qa" | "unclear". Enforce JSON output.
   Step 2 — Dispatch:
     - "eda"      -> recommender.suggest_eda(profile, topic)
     - "models"   -> recommender.suggest_models(profile, topic)
     - "insights" -> recommender.suggest_insights(profile, topic)
     - "general_qa" -> answer directly via a plain Gemini call, no suggestions list
     - "unclear"  -> return {"status": "unclear", "message": "Could you clarify what you're asking about?"}
   Success: {"status": "success", "data": {"mode": ..., "reply": "...", "suggestions": [...] | None}}
4. save_session(session_data: dict) -> dict — calls storage.save_session, passes through its result.
5. get_sessions() -> dict — calls storage.get_sessions, passes through its result.
6. Import profiler, recommender, and storage functions at the top of engine.py
   (not inside functions) so test patch paths like "src.engine.engine.suggest_eda"
   resolve correctly.
7. Wrap all logic in try/except; return {"status": "error", "message": str(e)} on
   unexpected exceptions rather than crashing.
```

## Guardrail Prompts

- Incorrect patch target (tests fail despite correct logic):
  - `Import recommender/profiler/storage functions at the top of engine.py with explicit imports so tests can patch "src.engine.engine.suggest_eda" etc. correctly — not "src.engine.recommender.suggest_eda".`
- Classification step always guessing instead of asking for clarification:
  - `When the classification call is not confident which of eda/models/insights/general_qa applies, return "unclear" rather than defaulting to one category.`
- Missing "status" key in some paths:
  - `Every return statement in every function must include a "status" key. Check all branches including "unclear" and "error".`
- Hardcoded API key in the classification call:
  - `Load the API key from os.getenv() only, same as recommender.py — do not duplicate a hardcoded key here.`

## Verification Prompt

```text
Now run the engine tests and confirm which test validates each behavior:
- load_dataset success path returns a profile
- load_dataset failure path (bad file) returns an error status
- handle_message classifies and dispatches correctly to suggest_eda
- handle_message classifies and dispatches correctly to suggest_models
- handle_message classifies and dispatches correctly to suggest_insights
- handle_message returns "unclear" without calling any recommender function
  when classification is not confident
- save_session and get_sessions correctly pass through storage results
If any test fails, fix the implementation only — do not modify or weaken the tests.
```

---

# Prompt 5: Interface

## Core Implementation Prompt

```text
Before implementing anything, follow these constraints:
1. Respect layer boundaries: interface has no AI calls and no storage access.
   engine never imports storage internals directly except through the
   functions defined in storage_handler.py. Only recommender.py (via its
   Gemini client setup) may call the Gemini API.
2. Do not invent data. Any function generating text about a dataset can only
   state numbers/columns that actually appear in the profile_payload it was
   given. Do not fabricate statistics to make a suggestion sound complete.
3. Match existing function signatures exactly as defined in CONTRACT.md.
   Do not rename parameters or change return shapes to make your
   implementation easier — ask me first if a contract seems wrong.
4. Never hardcode API keys or credentials. Load them via os.getenv() only.
5. Run pytest before and after your change. Do not weaken or delete a test
   to make it pass — tell me if a test seems wrong instead of changing it.
```

```text
I am implementing the Interface layer for data_copilot.
Context: Read CONTRACT.md, FUNCTIONALITY.md, src/engine/engine.py, and
         tests/interface/test_interface.py.
Goal: Implement format_response(result: dict) -> str and run_session(process_fn=None)
      in src/interface/cli.py.
Requirements:
1. format_response: Read result["status"] and return a formatted string for each case:
   - "success" (from load_dataset)     -> summarize row_count, columns, key stats
   - "success" (from handle_message)   -> print result["data"]["reply"], then list
                                           each suggestion in result["data"]["suggestions"]
                                           if present, labeled by result["data"]["mode"]
   - "success" (from save_session)     -> confirm with the session id
   - "success" (from get_sessions)     -> list each saved session (topic, timestamp)
   - "unclear"  -> return result["message"]
   - "error"    -> return result.get("message", "Something went wrong.")
   Do not raise; always return a string.
2. run_session: Loop calling input("You: ") to read one line at a time.
   - Skip blank lines (after strip).
   - Exit cleanly (print a goodbye message and return) when the line equals
     "quit" or "exit" (case-insensitive), or when EOFError is raised.
   - Parse simple commands before falling through to freeform chat:
       "load <path>"  -> engine.load_dataset(path), store profile in session state
       "topic <text>" -> engine.set_topic(text), store topic in session state
       "save"         -> engine.save_session(current session state)
       "history"      -> engine.get_sessions()
     Anything else    -> process_fn(user_input, current_profile, current_topic)
                         which defaults to engine.handle_message
   - Print "Copilot: " followed by format_response(result), then a blank line.
3. process_fn defaults to handle_message imported from src.engine.engine at
   the top of cli.py. Inside run_session, always call process_fn(...) —
   never call handle_message directly — so tests can inject a mock.
4. Import engine functions at the top of cli.py so patch paths like
   "src.interface.cli.handle_message" resolve correctly.
5. Do not modify or weaken any tests.
```

## Guardrail Prompts

- `format_response` raises instead of returning:
  - `Every code path in format_response must return a string. Add a default case that returns result.get("message", "Something went wrong.") for any unrecognized status.`
- `run_session` bypasses `process_fn`:
  - `Replace any direct calls to handle_message inside the loop with calls to process_fn — this is what lets tests inject a mock engine.`
- Command parsing conflicts with freeform chat:
  - `Check for "load ", "topic ", "save", and "history" as exact command prefixes before falling through to process_fn — a user typing "topic" with no argument should still be handled gracefully, not crash on an index error.`
- Session loop does not exit on "quit":
  - `Compare user_input.strip().lower() against "quit" and "exit" at the top of the loop body, before any command parsing.`
- Suggestions not rendered clearly:
  - `When result["data"]["suggestions"] is a non-empty list, print each suggestion's "text" on its own indented line, and prefix the whole block with "[grounded]" or "[general]" based on result["data"]["mode"].`

## Verification Prompt

```text
Now run the interface tests and confirm which test validates each behavior:
- format_response for a dataset-load success summarizes the profile correctly
- format_response for a handle_message success lists suggestions with mode label
- format_response for "unclear" returns the clarifying message
- format_response for "error" returns a safe fallback string
- run_session with a mocked process_fn prints the formatted response to stdout
- run_session exits cleanly on "quit"/"exit" without calling process_fn again
If any test fails, fix the implementation only — do not modify or weaken the tests.
```