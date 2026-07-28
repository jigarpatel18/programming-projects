
from __future__ import annotations

from pathlib import Path

import pandas as pd

SUPPORTED_EXTENSIONS = {".csv", ".xlsx", ".xls"}

# src/engine/profiler.py -> project root is two levels up
_PROJECT_ROOT = Path(__file__).resolve().parent.parent.parent
DATASETS_DIR = _PROJECT_ROOT / "datasets"


# look for dataset in dataset folder
def list_available_datasets() -> list[str]:
    
    if not DATASETS_DIR.exists():
        return []
    return sorted(
        f.name for f in DATASETS_DIR.iterdir()
        if f.is_file() and f.suffix.lower() in SUPPORTED_EXTENSIONS
    )


# load in a .csv o r.xlsx file if given
def load_dataset(path: str) -> pd.DataFrame:

    file_path = Path(path)

    if not file_path.exists():
        fallback_path = DATASETS_DIR / path
        if fallback_path.exists():
            file_path = fallback_path
        else:
            raise FileNotFoundError(
                f"File not found: checked '{path}' and '{fallback_path}'"
            )

    extension = file_path.suffix.lower()
    if extension not in SUPPORTED_EXTENSIONS:
        raise ValueError(
            f"Unsupported file type '{extension}'. Supported types: {sorted(SUPPORTED_EXTENSIONS)}"
        )

    try:
        if extension == ".csv":
            df = pd.read_csv(file_path)
        else:
            df = pd.read_excel(file_path)
    except Exception as e:
        raise ValueError(f"Could not parse '{path}': {e}")

    if df.empty:
        raise ValueError(f"'{path}' contains no data")

    return df


# create a profile dict to keep track of data stats as a dataframe is loaded and used in the conversation
def build_profile(df: pd.DataFrame) -> dict:
    
    columns = list(df.columns)
    row_count = len(df)
    missing_by_column = {col: int(df[col].isnull().sum()) for col in columns}

    numeric_stats = {}
    categorical_stats = {}

    for col in columns:
        if pd.api.types.is_numeric_dtype(df[col]):
            numeric_stats[col] = _numeric_column_stats(df[col])
        else:
            categorical_stats[col] = _categorical_column_stats(df[col])

    return {
        "columns": columns,
        "row_count": row_count,
        "missing_by_column": missing_by_column,
        "numeric_stats": numeric_stats,
        "categorical_stats": categorical_stats,
    }


# compute all ket stats for numeric features which could be usefull in analysis
def _numeric_column_stats(series: pd.Series) -> dict:
    clean = series.dropna()
    if clean.empty:
        return {"mean": None, "std": None, "min": None, "max": None, "skew": None}
    return {
        "mean": round(float(clean.mean()), 2),
        "std": round(float(clean.std()), 2) if len(clean) > 1 else 0.0,
        "min": round(float(clean.min()), 2),
        "max": round(float(clean.max()), 2),
        "skew": round(float(clean.skew()), 2) if len(clean) > 2 else 0.0,
    }


# find key values for categorical features like top val, freq, or counts
def _categorical_column_stats(series: pd.Series) -> dict:
    clean = series.dropna()
    n_unique = int(clean.nunique())
    if clean.empty:
        return {"top_value": None, "top_freq": 0, "n_unique": 0}
    value_counts = clean.value_counts()
    return {
        "top_value": str(value_counts.index[0]),
        "top_freq": int(value_counts.iloc[0]),
        "n_unique": n_unique,
    }