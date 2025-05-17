import pytest
import os
from lib.in1 import IN1

@pytest.fixture
def in1_data():
    """
    Fixture to provide an IN1 instance with the test input file.
    """
    script_directory = os.path.dirname(os.path.abspath(__file__))
    test_file = os.path.join(script_directory, "IN1.INP")
    return IN1(test_file)

def test_ionization_potential(in1_data):
    """
    Test the get_ionization_potential method.
    """
    expected_ip = 1007.32
    ip = in1_data.get_ionization_potential("15")
    assert abs(ip - expected_ip) < 1e-6, f"Expected IP {expected_ip}, got {ip}"

def test_configuration(in1_data):
    """
    Test the get_config method.
    """
    expected_config = ['2s2', '2p4']
    config = in1_data.get_config("17", "2")
    assert config == expected_config, f"Expected config {expected_config}, got {config}"

def test_statistical_weight(in1_data):
    """
    Test the get_stat_weight method.
    """
    expected_weight = 6.0
    weight = in1_data.get_stat_weight("18", "3")
    assert abs(weight - expected_weight) < 1e-6, f"Expected weight {expected_weight}, got {weight}"

def test_ionization_energy(in1_data):
    """
    Test the get_ionization_energy method.
    """
    expected_energy = 1007.32 - 585.050 + 8.744  # IP - E(Level1) + E(Level2)
    energy = in1_data.get_ionization_energy("15", "3", "16", "2")
    assert abs(energy - expected_energy) < 1e-6, f"Expected energy {expected_energy}, got {energy}"

def test_branching_ratio(in1_data):
    """
    Test the get_branching_ratio method.
    """
    expected_ratio = 4.0 / (2.0 + 4.0)  # g(Level1) / (g(Level1) + g(Level2))
    ratio = in1_data.get_branching_ratio("16", "1")
    assert abs(ratio - expected_ratio) < 1e-6, f"Expected ratio {expected_ratio}, got {ratio}"

def test_save_file():
    IN1("IN1.INP").dump_to_file("test_output.inp")
