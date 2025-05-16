import pytest
import difflib

from lib.bcfp import BCFP  # Adjust import based on your module structure


@pytest.fixture
def bcfp_instance2():
    """Create a BCFP instance from ../db/O/BCFP.INP (Format 1)."""
    return BCFP("BCFP-fac.INP")  # Replace with actual file path


@pytest.fixture
def bcfp_instance1():
    """Create a BCFP instance from ../db/O/fac/BCFP.INP (Format 2)."""
    return BCFP("BCFP-piter.INP")  # Replace with actual file path


def test_bcfp_functionality(bcfp_instance1, bcfp_instance2):
    """Test core functionality of BCFP class for both file formats."""
    # Test Format 1: bcfp_instance1 (branching ratio + 3 coefficients)
    # Verify spectroscopic numbers
    assert bcfp_instance1.get_sp_numbers() == ['1', '2', '3', '4', '5', '6', '7', '8', '9'], "Incorrect spectroscopic numbers for Format 1"

    # Verify specific transitions exist
    assert bcfp_instance1.contains_transition("1", "1", "2", "1"), "Transition (1,1 -> 2,1) not found in Format 1"
    assert bcfp_instance1.contains_transition("1", "2", "2", "2"), "Transition (1,2 -> 2,2) not found in Format 1"

    # Verify transition data for (1,1 -> 2,1)
    data1 = bcfp_instance1.get_transition_data("1", "1", "2", "1")
    assert data1['branching_ratio'] == 0.2, "Incorrect branching ratio for (1,1 -> 2,1)"
    assert data1['coefficients'] == [0.0, 0.0, 0.0], "Incorrect coefficients for (1,1 -> 2,1)"

    # Verify transition data for (1,2 -> 2,2)
    data2 = bcfp_instance1.get_transition_data("1", "2", "2", "2")
    assert data2['branching_ratio'] == 0.3, "Incorrect branching ratio for (1,2 -> 2,2)"
    assert data2['coefficients'] == [0.0, 0.0, 0.0], "Incorrect coefficients for (1,2 -> 2,2)"

    # Verify get_branching_ratio and get_coefficients
    assert bcfp_instance1.get_branching_ratio("1", "1", "2", "1") == 0.2, "get_branching_ratio failed for (1,1 -> 2,1)"
    assert bcfp_instance1.get_coefficients("1", "2", "2", "2") == [0.0, 0.0, 0.0], "get_coefficients failed for (1,2 -> 2,2)"

    # Test non-existent transition
    with pytest.raises(ValueError, match="Transition \\(1, 1, 2, 999\\) not found"):
        bcfp_instance1.get_transition_data("1", "1", "2", "999")

    # Test Format 2: bcfp_instance2 (4 coefficients)
    # Verify spectroscopic numbers
    assert bcfp_instance2.get_sp_numbers() == ['1', '2', '3', '4', '5', '6', '7', '8', '9'], "Incorrect spectroscopic numbers for Format 2"

    # Verify specific transitions exist
    assert bcfp_instance2.contains_transition("1", "1", "2", "1"), "Transition (1,1 -> 2,1) not found in Format 2"
    assert bcfp_instance2.contains_transition("1", "1", "2", "26"), "Transition (1,1 -> 2,26) not found in Format 2"

    # Verify transition data for (1,1 -> 2,1)
    data3 = bcfp_instance2.get_transition_data("1", "1", "2", "1")
    assert 'branching_ratio' not in data3, "Unexpected branching ratio in Format 2"
    assert data3['coefficients'] == [184.1, -38.808, 51.286, -196.4], "Incorrect coefficients for (1,1 -> 2,1)"

    # Verify transition data for (1,1 -> 2,26)
    data4 = bcfp_instance2.get_transition_data("1", "1", "2", "26")
    assert 'branching_ratio' not in data4, "Unexpected branching ratio in Format 2"
    assert data4['coefficients'] == [0.089963, -0.21233, -0.0044655, -0.28152], "Incorrect coefficients for (1,1 -> 2,26)"

    # Verify get_coefficients
    assert bcfp_instance2.get_coefficients("1", "1", "2", "1") == [184.1, -38.808, 51.286, -196.4], "get_coefficients failed for (1,1 -> 2,1)"

    # Verify get_branching_ratio raises error for Format 2
    with pytest.raises(ValueError, match="No branching ratio available for transition \\(1, 1, 2, 1\\)"):
        bcfp_instance2.get_branching_ratio("1", "1", "2", "1")
   # Test non-existent transition
    with pytest.raises(ValueError, match="Transition \\(1, 1, 2, 999\\) not found"):
        bcfp_instance2.get_transition_data("1", "1", "2", "999")

def test_bcfp_dump_to_string():
    """Test the dump_to_string method of BCFP class."""
    bcfp_file_to_test = "BCFP-piter.INP"

    try:
        # 1. Read the original content of the file
        with open(bcfp_file_to_test, 'r') as f_orig:
            original_file_content = f_orig.read()
    except FileNotFoundError:
        print(f"Error: The file '{bcfp_file_to_test}' was not found.")
        exit(1)

    # 2. Create a BCFP instance from the file path
    #    This internally calls _load_data to parse the file
    bcfp_data_instance = BCFP(bcfp_path=bcfp_file_to_test)

    # 3. Dump the BCFP instance's data back to a string
    dumped_bcfp_content = bcfp_data_instance.dump_to_string()

    # 4. Compare the original file content with the dumped string
    original_lines = original_file_content.splitlines(keepends=True)
    dumped_lines = dumped_bcfp_content.splitlines(keepends=True)

    f = open("BCFP-piter.INP.1","w")
    f.writelines(dumped_lines)
    f.close()

    if original_lines == dumped_lines:
        print(f"Success: The content of '{bcfp_file_to_test}' and its dumped version are identical.")
    else:
        print(f"Differences found between '{bcfp_file_to_test}' (original) and its dumped version:")
        print("--- DIFF START ---")

        diff_generator = difflib.unified_diff(
            original_lines,
            dumped_lines,
            fromfile=f'{bcfp_file_to_test} (Original)',
            tofile=f'{bcfp_file_to_test} (Dumped by BCFP class)',
            lineterm=''  # Important as splitlines(keepends=True) already includes newlines
        )

        for diff_line in diff_generator:
            print(diff_line, end='')  # diff_line already contains newline if present

        print("\n--- DIFF END ---")
        print("\nNote: Differences may arise from:")
        print("  - Re-ordering of transition entries (dump_to_string sorts them).")
        print("  - Re-formatting of numeric values (e.g., to consistent scientific notation).")
        print("  - Normalization of spacing or header lines.")
        print("  - Lines in the original file that are skipped during parsing (e.g., comments, malformed lines).")
