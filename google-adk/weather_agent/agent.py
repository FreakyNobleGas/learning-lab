from dotenv import load_dotenv
from google.adk.agents import Agent
from google.adk.models.lite_llm import LiteLlm

load_dotenv()

# ─── Tool #1: Get the weather (fake data for learning purposes) ───────────────
#
# A "tool" is a plain Python function the AI can decide to call.
# ADK automatically reads the function name, docstring, and type hints
# to tell the model what the tool does and what arguments it takes.

def get_weather(city: str) -> dict:
    """Returns the current weather for a given city.

    Args:
        city: The name of the city to look up.
    """
    fake_data = {
        "New York": "Partly cloudy, 18°C",
        "Tokyo": "Sunny, 22°C",
        "London": "Rainy, 12°C",
        "Sydney": "Clear skies, 27°C",
    }
    weather = fake_data.get(city, f'No data available for "{city}".')
    return {"city": city, "weather": weather}


# ─── Tool #2: Convert temperature ────────────────────────────────────────────

def convert_temperature(value: float, from_unit: str) -> dict:
    """Converts a temperature between Celsius and Fahrenheit.

    Args:
        value: The temperature value to convert.
        from_unit: The unit to convert from. Must be 'celsius' or 'fahrenheit'.
    """
    if from_unit.lower() == "celsius":
        return {"result": round(value * 9 / 5 + 32, 1), "unit": "fahrenheit"}
    else:
        return {"result": round((value - 32) * 5 / 9, 1), "unit": "celsius"}


# ─── The Agent ────────────────────────────────────────────────────────────────
#
# LiteLlm is a bridge that lets ADK talk to hundreds of model providers,
# including Ollama for local models. The model string format is:
#   "ollama_chat/<model-name>"
#
# The file MUST have a variable named `root_agent` — that's how ADK finds it.

root_agent = Agent(
    name="weather_agent",

    # LiteLlm routes the request to your local Ollama server.
    # Change "llama3.2" to whichever model you have pulled in Ollama.
    model=LiteLlm(model="ollama_chat/llama3.2"),

    # The instruction is like a permanent memo to the AI that sets the rules.
    instruction="""You are a helpful weather assistant.
When asked about the weather in a city, always use the get_weather tool.
When the user asks to convert temperatures, use the convert_temperature tool.
Keep your responses friendly and concise.""",

    description="Answers weather questions and converts temperatures.",

    tools=[get_weather, convert_temperature],
)
