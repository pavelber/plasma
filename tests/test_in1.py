import os
import traceback

from lib.in1 import IN1

# Define test configurations for each file
TEST_CONFIGS = {
    "IN1-1.INP": {
        "ionization_potential": {
            "sp_num": "15",
            "expected": 1007.32
        },
        "configuration": {
            "sp_num": "17",
            "level": "2",
            "expected": ["2s2", "2p4"]
        },
        "stat_weight": {
            "sp_num": "18",
            "level": "3",
            "expected": 6.0
        },
        "ionization_energy": {
            "sp_num1": "15",
            "level1": "3",
            "sp_num2": "16",
            "level2": "2",
            "expected": 1007.32 - 585.050 + 8.744
        },
        "branching_ratio": {
            "sp_num": "16",
            "level": "1",
            "expected": 4.0 / (2.0 + 4.0)
        }
    },
    "IN1.INP": {
        # Add different test parameters for IN1-1.INP
        "ionization_potential": {
            "sp_num": "35",
            "expected": 17293.81
        },
        "configuration": {
            "sp_num": "32",
            "level": "5",
            "expected": ["2s1", "2p2"]
        },
        "stat_weight": {
            "sp_num": "33",
            "level": "11",
            "expected": 12.0
        },
        "ionization_energy": {
            "sp_num1": "32",
            "level1": "3",
            "sp_num2": "33",
            "level2": "2",
            "expected": 3755.66 - 99.872 + 68.029
        },
        "branching_ratio": {
            "sp_num": "33",
            "level": "34",
            "expected": 12.0 / (12.0 + 16.0 + 24.0 + 32.0 + 48.0 + 64.0 + 96.0 + 128.0)
        }
    }
}


def run_tests(test_file, config):
    """Run tests for IN1 class with given configuration."""
    script_directory = os.path.dirname(os.path.abspath(__file__))
    test_file_path = os.path.join(script_directory, test_file)
    passed = 0
    total = 0

    try:
        # Instantiate IN1
        in1_data = IN1(test_file_path)

        # Test 1: Ionization potential
        total += 1
        params = config["ionization_potential"]
        ip = in1_data.get_ionization_potential(params["sp_num"])
        assert abs(ip - params["expected"]) < 1e-6, f"Expected IP {params['expected']}, got {ip}"
        print("Test 1 (Ionization Potential): PASSED")
        passed += 1

        # Test 2: Configuration
        total += 1
        params = config["configuration"]
        config_result = in1_data.get_config(params["sp_num"], params["level"])
        assert config_result == params["expected"], f"Expected config {params['expected']}, got {config_result}"
        print("Test 2 (Configuration): PASSED")
        passed += 1

        # Test 3: Statistical weight
        total += 1
        params = config["stat_weight"]
        weight = in1_data.get_stat_weight(params["sp_num"], params["level"])
        assert abs(weight - params["expected"]) < 1e-6, f"Expected weight {params['expected']}, got {weight}"
        print("Test 3 (Statistical Weight): PASSED")
        passed += 1

        # Test 4: Ionization energy
        total += 1
        params = config["ionization_energy"]
        energy = in1_data.get_ionization_energy(params["sp_num1"], params["level1"],
                                                params["sp_num2"], params["level2"])
        assert abs(energy - params["expected"]) < 1e-6, f"Expected energy {params['expected']}, got {energy}"
        print("Test 4 (Ionization Energy): PASSED")
        passed += 1

        # Test 5: Branching ratio
        total += 1
        params = config["branching_ratio"]
        ratio = in1_data.get_branching_ratio(params["sp_num"], params["level"])
        assert abs(ratio - params["expected"]) < 1e-6, f"Expected ratio {params['expected']}, got {ratio}"
        print("Test 5 (Branching Ratio): PASSED")
        passed += 1

    except AssertionError as e:
        print(f"Test {total}: FAILED - {e}")
    except Exception as e:
        traceback.print_exc()
        print(f"Test {total}: ERROR - {e}")

    print(f"\n{passed}/{total} tests passed for {test_file}")
    return passed, total


if __name__ == "__main__":
    total_passed = 0
    total_tests = 0
    for test_file, config in TEST_CONFIGS.items():
        print(f"\nRunning tests for {test_file}...")
        passed, tests = run_tests(test_file, config)
        total_passed += passed
        total_tests += tests
    print(f"\nOverall: {total_passed}/{total_tests} tests passed")
