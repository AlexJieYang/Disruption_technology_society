"""Repository-relative input configuration and figure destinations."""
from pathlib import Path
import json
import os

ROOT = Path(__file__).resolve().parent

def settings():
    path = Path(os.environ.get("RESEARCH_CONFIG", ROOT / "config.json"))
    if not path.is_absolute():
        path = ROOT / path
    if not path.is_file():
        if "RESEARCH_CONFIG" in os.environ:
            raise FileNotFoundError(f"Configuration file does not exist: {path}")
        path = ROOT / "config.example.json"
    with path.open(encoding="utf-8") as handle:
        return json.load(handle)

_FILES = {'papers': ['data', 'P4Reg.csv'], 'fields': ['data', 'fields.csv'], 'reg51.csv': ['regression', 'reg51.csv'], 'reg52.csv': ['regression', 'reg52.csv'], 'reg53.csv': ['regression', 'reg53.csv'], 'reg54.csv': ['regression', 'reg54.csv'], 'reg61.csv': ['regression', 'reg61.csv'], 'reg62.csv': ['regression', 'reg62.csv'], 'reg63.csv': ['regression', 'reg63.csv'], 'reg64.csv': ['regression', 'reg64.csv'], 'reg31.csv': ['regression', 'reg31.csv'], 'reg32.csv': ['regression', 'reg32.csv'], 'reg33.csv': ['regression', 'reg33.csv'], 'reg34.csv': ['regression', 'reg34.csv'], 'reg41.csv': ['regression', 'reg41.csv'], 'reg42.csv': ['regression', 'reg42.csv'], 'reg43.csv': ['regression', 'reg43.csv'], 'reg44.csv': ['regression', 'reg44.csv']}

def input_path(key):
    config = settings()
    kind, filename = _FILES[key]
    setting = "regression_dir" if kind == "regression" else "data_dir"
    directory = Path(config.get(setting, "data")).expanduser()
    if not directory.is_absolute():
        directory = ROOT / directory
    path = directory / filename
    if not path.is_file():
        raise FileNotFoundError(f"Input {key!r} is unavailable at {path}. Set {setting} in config.json to the study-data directory.")
    return path

def output_path(name):
    path = Path(settings().get("output_dir", "outputs")).expanduser()
    if not path.is_absolute():
        path = ROOT / path
    destination = path / name
    destination.parent.mkdir(parents=True, exist_ok=True)
    return destination
