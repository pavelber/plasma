import os
from lib.in1 import IN1
import traceback

def run_tests():
    """Run tests for IN1 class."""
    script_directory = os.path.dirname(os.path.abspath(__file__))
    test_file = os.path.join(script_directory,  "IN1.INP")
    passed = 0
    total = 0

    # Create sample input file

    try:
        # Instantiate IN1
        in1_data = IN1(test_file)

        # Test 1: Ionization potential
        total += 1
        expected_ip = 1007.32
        ip = in1_data.get_ionization_potential("15")
        assert abs(ip - expected_ip) < 1e-6, f"Expected IP {expected_ip}, got {ip}"
        print("Test 1 (Ionization Potential): PASSED")
        passed += 1

        # Test 2: Configuration
        total += 1
        expected_config = ['2s2', '2p4']
        config = in1_data.get_config("17", "2")
        assert config == expected_config, f"Expected config {expected_config}, got {config}"
        print("Test 2 (Configuration): PASSED")
        passed += 1

        # Test 3: Statistical weight
        total += 1
        expected_weight = 6.0
        weight = in1_data.get_stat_weight("18", "3")
        assert abs(weight - expected_weight) < 1e-6, f"Expected weight {expected_weight}, got {weight}"
        print("Test 3 (Statistical Weight): PASSED")
        passed += 1

        # Test 4: Ionization energy
        total += 1
        expected_energy = 1007.32 - 585.050 + 8.744  # IP - E(Level1) + E(Level2)
        energy = in1_data.get_ionization_energy("15", "3", "16", "2")
        assert abs(energy - expected_energy) < 1e-6, f"Expected energy {expected_energy}, got {energy}"
        print("Test 4 (Ionization Energy): PASSED")
        passed += 1

        # Test 5: Branching ratio
        total += 1
        expected_ratio = 4.0 / (2.0 + 4.0)  # g(Level1) / (g(Level1) + g(Level2))
        ratio = in1_data.get_branching_ratio("16", "1")
        assert abs(ratio - expected_ratio) < 1e-6, f"Expected ratio {expected_ratio}, got {ratio}"
        print("Test 5 (Branching Ratio): PASSED")
        passed += 1

    except AssertionError as e:
        print(f"Test {total}: FAILED - {e}")
    except Exception as e:
        traceback.print_exc()
        print(f"Test {total}: ERROR - {e}")

    print(f"\n{passed}/{total} tests passed")

if __name__ == "__main__":
    run_tests()