# FUNCTIONALITY.md

## Purpose
Define what the system should do, i.e., functionalities.

## Example (Reference)
Use this as a template for level of detail.

### Functionality A: Load Dataset
- Input:
  - A file path typed by the user in the terminla (for exmaple: `load sales.csv`)

- Output:
  - Confirmation message summarizing the dataset (row count, column count, column names)

- Success:
  - File is found, parsed, and profiled; user sees a summary and can immediately ask for reccomendations about it 

- Failure/Edge Cases:
  - File path doesn't exist -> return "File not found," no profile is created
  - Unsupported file type (not `.csv`/`.xlsx`) -> return an error naming the supported formats
  - File exists but is empty or unreadable (e.g. corrupted, wrong delimiter) -> return an error, do not crash the session
  - User asks for recommendations before loading anything -> system falls back to general mode instead of failing


### Functionality B: Get Analysis Recommendations (EDA / Models / Insights)
- Input:
  - A conversaional request, either after loading a dataset (for example: "What models should I try?") or with no dataset loaded at all (for example: "what's a good approach for prediction?")

- Output:
  - A list of suggestions labeled by mode - `grounded` (tied to the loaded dataset's actual comlumns/stats) or `general` (domain advice with no dataset-specific claims)

- Success:
  - Grounded mode: every suggestion cites real column names/values from the loaded dataset — none are fabricated
  - General mode: suggestions are useful domain guidance and contain no invented column names, stats, or claims about data the system never saw
  - System correctly infers which of the three categories (EDA, model choice, insight direction) the user is asking about, or asks a clarifying question if it can't tell

- Failure/Edge Cases:
  - Request doesn't map to EDA/models/insights or general Q&A -> return `unclear` and ask the user to clarify, don't guess
  - A generated suggestion cites a column or number not present in the dataset profile -> suggestion is dropped before the user ever sees it
  - A generated suggestion in general mode invents dataset-specific detail -> suggestion is dropped or rewritten before being shown
  - Empty/blank input -> prompt the user to enter a question

### Functionality C: Save and Resume Sessions
- Input:
  - A save command (for example: `save`) after a topic and/or recommendations exist; or a history command (for example: `history`) to list past sessions

- Output:
  - Save: confirmation with a session ID
  - History: a list of past sessions (topic, dataset name if any, timestamp)

- Success:
  - Save persists the current topic, dataset profile (if any), and recommendations so far; history returns previously saved sessions in a readable list

- Failure/Edge Cases:
  - Save called with nothing to save yet (no topic, no recommendations) -> return an error rather than saving an empty session
  - History called with no saved sessions yet -> return a friendly empty state, not an error
  - Storage/network failure during save -> return an error, do not lose the user's current in-progress session state


## Quality Check
- Each functionality is written from user perspective.
- Success is measurable (not vague words like "works well").
- Failure cases are specific enough to become test cases later.
