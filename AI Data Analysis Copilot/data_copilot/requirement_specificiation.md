```markdown
# Requirement Specification Document (built off of lab 4)


## Part A: Functionality 

### Functionality 1: Load Dataset
- Input: user types `load` and a filename (like `load sales.csv`)
- Output: a summary of the dataset - rows, columns, column names
- Success: file is found and read in, user can now ask questions about it
- Failure/Edge Cases: file doesn't exist should say "file not found." wrong file type (not csv/xlsx) should say what types are supported. if no dataset is loaded yet, fall back to general advice instead of erroring out

### Functionality 2: Get Recommendations (EDA / Models / Insights)
- Input: user asks something like "what models should I use" or "give me insights"
- Output: a list of suggestions, labeled as grounded (based on real dataset) or general (no dataset loaded)
- Success: grounded suggestions only mention real columns/stats from the dataset. general suggestions don't make up any dataset details
- Failure/Edge Cases: if it can't tell what the user is asking for, ask them to clarify instead of guessing. if a suggestion makes up a column or number that isn't real, drop it before showing it

### Functionality 3: General Q&A
- Input: any regular data science question, like "what is overfitting"
- Output: a normal text answer from Gemini
- Success: answers the question, and uses the loaded dataset info if there is one
- Failure/Edge Cases: if there's an API error, return an error message instead of crashing the app. blank input just gets skipped


## Part B: Architecture Mapping
For each functionality, map responsibilities to components.

### Functionality 1 Load Dataset
- `interface` responsibilities: reads the `load <file>` command, shows the summary or error
- `engine` responsibilities: calls the profiler to read the file, catches errors if the file is bad, keeps the profile around for later
- `storage` responsibilities: none, loading a file doesn't touch Sheets

### Functionality 2 Get Recommendations (EDA / Models / Insights)
- `interface` responsibilities: sends the user's message to the engine, prints the suggestions back out
- `engine` responsibilities: figures out if it's eda/models/insights, calls the right function, checks the suggestions are actually based on real data before returning them
- `storage` responsibilities: none, suggestions aren't saved separately

### Functionality 3 General Q&A
- `interface` responsibilities: sends the question to the engine, prints the answer
- `engine` responsibilities: grabs chat history, adds dataset info to the prompt if one's loaded, calls Gemini for the answer
- `storage` responsibilities: gives back the past messages so Gemini has context


## Part C: Interface Contracts

### Functionality 1

#### `interface -> engine`
- Function(s): `load_dataset_profile(path)`
- Input payload: filename as a string
- Return payload/status: dataset profile (columns, row count, stats)
- Failure statuses: file not found, unsupported file type

#### `engine -> storage`
- Function(s): none

### Functionality 2

#### `interface -> engine`
- Function(s): `suggest_eda(profile, topic)`, `suggest_models(profile, topic)`, `suggest_insights(profile, topic)`
- Input payload: profile (or none) and topic string
- Return payload/status: list of suggestions with mode (grounded/general)
- Failure statuses: unclear, if it can't tell what's being asked

#### `engine -> storage`
- Function(s): none

### Functionality 3

#### `interface -> engine`
- Function(s): `handle_message(session_id, user_input, profile, topic)`
- Input payload: session id, the question, current profile, current topic
- Return payload/status: answer text back
- Failure statuses: error message if the Gemini call fails

#### `engine -> storage`
- Function(s): `load_history(session_id)`
- Input payload: session id
- Return payload/status: past messages, formatted for Gemini
- Failure statuses: just returns an empty list if something goes wrong

