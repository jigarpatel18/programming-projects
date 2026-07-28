import pandas as pd
import pytest

from src.engine.profiler import load_dataset, build_profile


# test 1: check for when a file doesn't exist 
def test_load_dataset_missing_file_error():
    with pytest.raises(FileNotFoundError):
        load_dataset("/tmp/does_not_exist_xyz.csv")


# test 2: should return an error for when an incorrect file type is inputed
def test_load_dataset_unsupported_extension_error(tmp_path):
    bad_file = tmp_path / "notes.txt"
    bad_file.write_text("hello")
    with pytest.raises(ValueError):
        load_dataset(str(bad_file))


# test 3: shoud successfullyload in a real csv when given one
def test_load_dataset_parses_csv(tmp_path):
    csv_file = tmp_path / "sample.csv"
    csv_file.write_text("age,region\n25,West\n34,East\n")
    df = load_dataset(str(csv_file))
    assert len(df) == 2


# test 4: check to see if build_profile makes an accurate profile and return the summary
def test_build_profile_shape():
    df = pd.DataFrame({"age": [25, 34, 41], "region": ["West", "East", "West"]})
    profile = build_profile(df)

    assert profile["row_count"] == 3
    assert profile["columns"] == ["age", "region"]
    assert "age" in profile["numeric_stats"]
    assert "region" in profile["categorical_stats"]


# test 5: check to see if build_profile numercial stats are correctly computed 
def test_build_profile_numeric_stats_correct():
    df = pd.DataFrame({"age": [20, 30, 40]})
    profile = build_profile(df)
    assert profile["numeric_stats"]["age"]["mean"] == 30.0
    assert profile["numeric_stats"]["age"]["min"] == 20.0
    assert profile["numeric_stats"]["age"]["max"] == 40.0