# AI Data Analysis Copilot

## Background 
This is an AI-assisted data analysis Copilot that helps users dive into a dataset more quickly and learn about its important features and values. It can provide a summary of the values and basic-level stats, as well as different visualizations and models that would be beneficial for that dataset. It allows the user to chat with the bot and ask questions regarding specific datasets or regarding data science or data analysis. The purpose of this tool is to assist in faster data analysis turnouts and bring more efficiency and reasoning-based work. 

## Features 
The main features of this AI agent are the ability to utilize it as a normal chatbot in the field of data analysis, as well as the ability to ask questions regarding a specific dataset that a user may be interested in. In the background of the agent, 2 other engine tools help in recognizing the user's needs: a recommender and a profiler. The profiler creates a profile out of the uploaded dataset that makes the data readable by the AI, so that it can make recommendations based on it. It reads its categorical and numerical data and looks at all the features involved in the dataset. The recommender gives useful knowledge and statistical visualization based on the profile made for what would be most useful for the user based on the prompt entered. These two features allow for in-depth analysis to be done according to the user's needs and wants. 

## Paths
 (already in the data_copilot folder)
- Path to source code directory (``cd src``)
- Path to test directory(``cd tests``)
- Path to the requirement specification and design document is in the root of the project folder and can be found in the file called (``requirement_specification.md``)


## Demo Video
This is the link to the demo video on the complete start-up of integrating this agent into your own environment:

https://drive.google.com/file/d/1AKewAia0lhMq9IesjLVFh1DTLNniI8kA/view?usp=sharing 


## Architecture / Project Structure 
The entire project starts with the Presentation Layer, which is located in src/iterface/cli.py and leads into the Engine process and message handling. The next step involved is the Engine layer, which starts at src/engine/engine.py and generates the profile, classifies the intent, and handles the user's message. From these, it uses gemein_client.py and profiler.py to build the dataset profile and generate the JSON and response text. It then goes on to the Intent dispatcher in the Engine layer, which looks at the intent of the prompt and runs either suggest_eda(), suggest_models(), or suggest_insights(). Once the intent and suggest function is built and picked, the Engine sends the request to the Gemini AI used in this project. Once the response is collected, it is saved as a conversation in Google Sheets. In the sessions tab, it tracks the new session started, and in the messages tab, it tracks each message sent and received by the chatbot in that session. This is the Storage Layer handled by src.storage.storage_handler.py. If the session is ended and restarted, a new session ID will be made, and another row will track that session and messages within it. Once this is complete, the response of the AI is shown on the terminal for the user to read and further go off of. 



## Installation 
1. To begin with, download the project folder to your local device, or you can clone this repository into your workspace. Then is your IDE (recommended VS Code) of choice, you must create a virtual environment first. So run (``python -m venv .venv``) (``source .venv/bin/activate``)
2. The installation required to run this agent is all listed within requirements.txt. With the virtual environment activated, run (``pip install -r requirements.txt``). This installs pandas, numpy, google-genai, python-dotenv, gspread, and oauth2client. 
3. You will need to configure your own Gemini API key for the Gemini model you will be using and also get Google Cloud credentials. The (``.env.example``) is provided, which will need to be renamed to (``.env``), where you can add the Gemini API key and the Google Spreadsheets name (recommended data-analysis-copilot) because that is used in the code, but can be changed in storage_handler.py. When creating the folder for Google Cloud credentials, copy the credential info and paste it into the (``service_account.example.json``) file in the project folder, which you will need to rename to (``service_account.json``). Share the googlesheets spreadsheet with the email given in the credentials and turn on editor mode. 
4. The dataset folder already has some example datasets to use, but before prompting, add any datasets you want to use in there before running the program. 
5. Run a quick check on which version of Python you have with (``python --version``) to see whether Python or python3 works on your terminal. 


## Running Application 
Once all the above steps are completed, you run the program with (``python3 -m src.interface.cli``) and follow the prompts shown on the terminal. Press quit or exit when you want to end the session. 


## Running Tests
To run all the checks and tests on the agent's program, you can run them through their folder groups. From the project root, run (``pytest tests/storage/test_storage.py``) to run the storage layer tests. To run the Interface layer tests, run (``pytest tests/interface/test_interface.py``). And to run the engine layer and the helper files, run (``pytest tests/engine/ -v``)





