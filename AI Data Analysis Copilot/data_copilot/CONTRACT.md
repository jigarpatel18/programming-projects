# CONTRACT.md (Short Teaching Template)

## Purpose
Use this file to map each functionality to component ownership and explicit interfaces.

## 1) Three Components (3 Layers)
- `interface`: collects user input, displays output, no application logic
- `engine`: validates/parses/orchestrates logic, no direct UI rendering
- `storage`: reads/writes data, enforces uniqueness/integrity

## 2) Mapping Functionality to Components
For each functionality from `FUNCTIONALITY.md`, define:
- `interface` responsibility
- `engine` responsibility
- `storage` responsibility (if persistence is needed)

## 3) Example Types

`profile_payload` means the structured dataset summary produced by the porfiler (for example: `{"columns": [...], "row_count": int, "missing_by_column": {...}, "numeric_stats": {...}, "categorical_stats": {...}}`).

`recommendation_payload` means a list of grounded or general suggestions:
`{"mode": "grounded" | "general", "suggestions": [{"text": "...", "cited_columns": [...], "cited_values": [...]}]}`. In "general" mode, `cited_columns` and `cited_values` must be empty on every suggestion — there's no profile to cite from, so nothing here should read as dataset-specific.

`session_record` means a saved copilot session:
`{"topic": "...", "profile": profile_payload | None, "recommendations": [recommendation_payload, ...], "timestamp": "..."}`.



## 4) Functionality Contracts

### Functionality: Load Dataset
- `interface`: collect file path from user, display load confirmation or error
- `engine`: none (dispatches directly to the profiler module)
- `storage`: none — profiling happens in-memory, nothing persisted yet

`interface -> engine`
- `load_dataset(path: str) -> response_payload`
- Success: `{"status": "success", "data": profile_payload}`
- Failure: `{"status": "error", "message": "File not found"} | {"status": "error", "message": "Unsupported file type"}`

### Functionality: Set Analysis Topic
- `interface`: collect free-text topic description, display confirmation
- `engine`: store topic in session state, no validation needed
- `storage`: none until session is explicitly saved

`interface -> engine`
- `set_topic(topic: str) -> response_payload`
- Success: `{"status": "success", "data": {"topic": "..."}}`

### Functionality: Freeform Conversation
- `interface`: collect free-text message, display response
- `engine`: classify intent (does this map to eda/models/insights, or is it general Q&A that doesn't fit those three?) and route accordingly. This is the main Tool Use dispatcher — it decides whether to call `suggest_eda` / `suggest_models` / `suggest_insights` internally, or just answer conversationally.
- `storage`: none

`interface -> engine`
- `handle_message(user_input: str, profile: profile_payload | None, topic: str | None) -> response_payload`
- Success: `{"status": "success", "data": {"mode": "grounded" | "general", "reply": "...", "suggestions": [...] | None}}`
- Failure: `{"status": "unclear", "message": "Could you clarify what you're asking about?"}`

### Functionality: Suggest EDA Steps
- `interface`: collect request, display suggested EDA steps (labeled by mode)
- `engine`: Tool Use call to Gemini (grounded prompt if profile present, general prompt otherwise) + Reflection check
- `storage`: none

`interface -> engine`
- `suggest_eda(profile: profile_payload | None, topic: str) -> response_payload`
- Success (grounded): `{"status": "success", "data": {"mode": "grounded", "suggestions": [...]}}`
- Success (general): `{"status": "success", "data": {"mode": "general", "suggestions": [...]}}`

### Functionality: Suggest ML Models
- `interface`: collect request, display suggested models with rationale (labeled by mode)
- `engine`: Tool Use call to Gemini (grounded prompt if profile present, general prompt otherwise) + Reflection check
- `storage`: none

`interface -> engine`
- `suggest_models(profile: profile_payload | None, topic: str) -> response_payload`
- Success (grounded): `{"status": "success", "data": {"mode": "grounded", "suggestions": [...]}}`
- Success (general): `{"status": "success", "data": {"mode": "general", "suggestions": [...]}}`

### Functionality: Suggest Insights to Investigate
- `interface`: collect request, display suggested insight questions (labeled by mode)
- `engine`: Tool Use call to Gemini (grounded prompt if profile present, general prompt otherwise) + Reflection check
- `storage`: none

`interface -> engine`
- `suggest_insights(profile: profile_payload | None, topic: str) -> response_payload`
- Success (grounded): `{"status": "success", "data": {"mode": "grounded", "suggestions": [...]}}`
- Success (general): `{"status": "success", "data": {"mode": "general", "suggestions": [...]}}`

### Functionality: Save Session
- `interface`: trigger save, display confirmation
- `engine`: bundle profile + topic + recommendations into a `session_record`
- `storage`: write session, no duplicate check needed (sessions aren't unique by any natural key)

`engine -> storage`
- `save_session(session_data: session_record) -> response_payload`
- Success: `{"status": "success", "id": "session_123"}`
- Failure: `{"status": "error", "message": "..."}`

### Functionality: View Session History
- `interface`: trigger history view, display list of past sessions
- `engine`: fetch and format session list
- `storage`: read all sessions

`engine -> storage`
- `get_sessions() -> response_payload`
- Success: `{"status": "success", "data": [session_record, ...]}`
- Failure: `{"status": "error", "message": "..."}`

## 5) Grounding Check (AI-generated content only)
- **Grounded mode**: every suggestion must cite specific fields from the loaded `profile_payload` (`cited_columns`, `cited_values`). Any suggestion citing a column or value not present in the profile is dropped before reaching the interface layer.
- **General mode**: the reverse check — since no profile exists, any suggestion containing specific numbers, column names, or dataset-specific claims is dropped or rewritten. General advice must stay general; it must not sound like it's describing real data it never saw.
- `engine` enforces both directions before returning `{"status": "success", ...}` — `interface` never sees ungrounded (or falsely-grounded) content.


## 6) Quality Check
- Each responsibility has one owner only.
- Every contract defines success + failure statuses.
- No UI logic in `storage`, no persistence logic in `interface`.
- Every AI-generated `recommendation_payload` has passed the Grounding Check in Section 5 before being returned as `"success"`.
