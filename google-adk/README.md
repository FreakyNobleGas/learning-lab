# Google ADK Learning Project

A minimal Python project for learning [Google ADK](https://google.github.io/adk-docs/) (Agent Development Kit) — running a local AI model via [Ollama](https://ollama.com). No API key or internet connection required once set up.

---

## Background: What Is an AI Agent?

Before diving into code, here's a quick mental model of the pieces involved.

### What is a Large Language Model (LLM)?

An LLM (like Llama, Gemini, or GPT) is a type of AI model trained on massive amounts of text. It learned patterns in language well enough to generate coherent, contextually appropriate responses. Think of it as a very sophisticated autocomplete.

Key thing to understand: **an LLM only processes text in → text out**. On its own, it can't browse the web, run code, check the time, or call an API. It can only reason about things it was trained on.

### What is Ollama?

Ollama is a tool that lets you download and run LLMs **on your own machine** instead of sending data to a cloud service. It runs a small local server your code talks to. No API key, no usage fees, no internet needed.

### What is a Tool (Function Calling)?

To give an LLM real-world capabilities, we define **tools** — regular Python functions the AI can ask to be called.

Here's the flow:
1. You send the AI a message: *"What's the weather in Tokyo?"*
2. The AI sees it has a `get_weather` tool available and decides it needs it
3. The AI responds with a structured request: *"call `get_weather` with `city = "Tokyo"`"*
4. ADK runs your actual Python function and gets the result
5. The result is sent back to the AI, which uses it to write a final response

The AI never runs your code directly — it just asks for it, and ADK does the calling.

### What is an Agent?

An **agent** is an LLM + tools + instructions, packaged together. It can handle multi-step tasks by deciding when to call tools, what to do with the results, and when it's done.

ADK manages the back-and-forth between your code and the model so you don't have to.

### What is LiteLLM?

LiteLLM is a library that acts as a universal adapter between ADK and hundreds of different model providers — including Ollama. It translates ADK's requests into whatever format the target model expects.

---

## Project Structure

```
google-adk-learning/
├── weather_agent/
│   ├── __init__.py   ← tells ADK this folder is an agent package
│   └── agent.py      ← defines tools and exports root_agent
├── .env              ← environment config (you create this)
├── .env.example      ← template showing what goes in .env
├── requirements.txt  ← Python dependencies
└── README.md         ← you are here
```

ADK expects agents to live in subdirectories (packages), not as loose `.py` files. You run `adk web` from the **project root** (`google-adk-learning/`) and ADK discovers the `weather_agent/` folder automatically.

### Key File: `weather_agent/agent.py`

This is where everything lives:

- **Tools** (plain Python functions) — functions the AI can call
- **`root_agent`** — the exported agent that ADK looks for by name

The `root_agent` variable name is required — ADK won't find your agent without it.

---

## Setup

### 1. Install Ollama

Download and install from [https://ollama.com](https://ollama.com), or via Homebrew:

```bash
brew install ollama
```

### 2. Pull a model

This downloads a model to your machine (~2GB for llama3.2):

```bash
ollama pull llama3.2
```

Ollama will start its local server automatically when needed.

### 3. Create a Python virtual environment

A virtual environment keeps this project's dependencies isolated from the rest of your system.

```bash
python3 -m venv .venv
source .venv/bin/activate
```

You'll need to run `source .venv/bin/activate` each time you open a new terminal for this project. Your prompt will show `(.venv)` when it's active.

### 4. Install dependencies

```bash
pip install -r requirements.txt
```

### 5. Create your `.env` file

```bash
cp .env.example .env
```

The `.env` file is already set up for local use — no changes needed.

---

## Running the Agent

Make sure your virtual environment is active (`source .venv/bin/activate`) and Ollama is running before any of these.

### Option A: Web UI (recommended for exploring)

```bash
adk web
```

Opens a chat UI at [http://localhost:8000](http://localhost:8000). You can type messages and see the agent's responses, including which tools it called and what they returned.

### Option B: Terminal chat

```bash
adk run agent.py
```

An interactive chat session in your terminal.

### Option C: REST API server

```bash
adk api_server
```

Starts a local HTTP server so you can call your agent programmatically from other apps.

---

## What the Agent Can Do

This demo agent (`agent.py`) has two tools:

| Tool | What it does |
|---|---|
| `get_weather` | Returns fake weather data for a few cities |
| `convert_temperature` | Converts °C ↔ °F |

Try asking it:
- *"What's the weather in Tokyo?"*
- *"What's 22 Celsius in Fahrenheit?"*
- *"Is it warmer in New York or London right now?"* (uses `get_weather` twice!)

---

## How to Add Your Own Tool

In `agent.py`, write a plain Python function with a docstring and type hints, then add it to `tools`:

```python
def my_tool(input: str) -> dict:
    """Describe what this tool does — the AI reads this to decide when to use it.

    Args:
        input: What this parameter is for.
    """
    return {"result": f"You said: {input}"}

root_agent = Agent(
    # ...
    tools=[get_weather, convert_temperature, my_tool],  # ← add it here
)
```

The docstring is crucial — ADK passes it to the model so the AI knows when and how to use your tool.

---

## Changing the Model

Edit the `model=` line in `agent.py`. First pull the model with Ollama:

```bash
ollama pull mistral
```

Then update `agent.py`:

```python
model=LiteLlm(model="ollama_chat/mistral"),
```

Run `ollama list` to see what models you have downloaded.

---

## Key Concepts Summary

| Term | What it means |
|---|---|
| **LLM** | The AI model (Llama here). Understands and generates text. |
| **Ollama** | Runs LLMs locally on your machine |
| **LiteLLM** | Adapter that connects ADK to Ollama (and other providers) |
| **Tool** | A Python function the AI can request to be called |
| **Agent** | LLM + tools + instructions, managed by ADK |
| **Instruction** | The system prompt — permanent rules/personality for the agent |
| **`root_agent`** | The required variable name ADK uses to find your agent |

---

## Further Reading

- [ADK Python Quickstart](https://google.github.io/adk-docs/get-started/quickstart/)
- [Using Ollama with ADK](https://google.github.io/adk-docs/models/litellm/)
- [Tools Guide](https://google.github.io/adk-docs/tools/)
- [Multi-Agent Systems](https://google.github.io/adk-docs/agents/multi-agents/)
