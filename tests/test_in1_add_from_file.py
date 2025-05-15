import pytest

from lib.in1 import IN1  # Adjust import based on your module structure


@pytest.fixture
def in1_instance1():
    """Create an IN1 instance from in1_test1.inp."""
    return IN1("in1_test1.inp")


@pytest.fixture
def in1_instance2():
    """Create an IN1 instance from in1_test2.inp."""
    return IN1("in1_test2.inp")


def test_add_or_replace_sp_data(in1_instance1, in1_instance2):
    """Test add_or_replace_sp_data for spectroscopic number 2, including renumeration table."""
    # Store original data for sp_num 1 from in1_instance1 to verify it doesn't change
    sp1_orig_ion_pot = in1_instance1.get_ionization_potential("1")
    sp1_orig_levels = in1_instance1.get_levels("1")
    sp1_orig_energy_1 = in1_instance1.get_energy("1", "1")
    sp1_orig_stat_weight_1 = in1_instance1.get_stat_weight("1", "1")
    sp1_orig_config_1 = in1_instance1.get_config("1", "1")
    sp1_orig_energy_2 = in1_instance1.get_energy("1", "2")
    sp1_orig_stat_weight_2 = in1_instance1.get_stat_weight("1", "2")
    sp1_orig_config_2 = in1_instance1.get_config("1", "2")

    # Replace data for sp_num 2 in in1_instance1 with data from in1_instance2 and get renumeration table
    renumeration_table = in1_instance1.add_or_replace_sp_data("2", in1_instance2)

    # Verify renumeration table
    expected_renumeration = {"1": "1", "2": "2"}
    assert renumeration_table == expected_renumeration, f"Expected renumeration table {expected_renumeration}, got {renumeration_table}"

    # Verify data for sp_num 2 has been replaced
    assert in1_instance1.get_ionization_potential("2") == in1_instance2.get_ionization_potential("2") == 200.0
    assert in1_instance1.get_levels("2") == in1_instance2.get_levels("2") == ["1", "2", "3"]

    # Check level 1 for sp_num 2
    assert in1_instance1.get_energy("2", "1") == in1_instance2.get_energy("2", "1") == 0.0
    assert in1_instance1.get_stat_weight("2", "1") == in1_instance2.get_stat_weight("2", "1") == 4.0
    assert in1_instance1.get_config("2", "1") == in1_instance2.get_config("2", "1") == ["2s2", "2p5"]

    # Check level 2 for sp_num 2
    assert in1_instance1.get_energy("2", "2") == in1_instance2.get_energy("2", "2") == 8.744
    assert in1_instance1.get_stat_weight("2", "2") == in1_instance2.get_stat_weight("2", "2") == 2.0
    assert in1_instance1.get_config("2", "2") == in1_instance2.get_config("2", "2") == ["2s2", "2p5"]

    # Check level 3 for sp_num 2
    assert in1_instance1.get_energy("2", "3") == in1_instance2.get_energy("2", "3") == 10.744
    assert in1_instance1.get_stat_weight("2", "3") == in1_instance2.get_stat_weight("2", "3") == 1.0
    assert in1_instance1.get_config("2", "3") == in1_instance2.get_config("2", "3") == ["2s2", "2p6"]

    # Verify data for sp_num 1 has not changed
    assert in1_instance1.get_ionization_potential("1") == sp1_orig_ion_pot == 100.0
    assert in1_instance1.get_levels("1") == sp1_orig_levels == ["1", "2"]
    assert in1_instance1.get_energy("1", "1") == sp1_orig_energy_1 == 0.0
    assert in1_instance1.get_stat_weight("1", "1") == sp1_orig_stat_weight_1 == 1.0
    assert in1_instance1.get_config("1", "1") == sp1_orig_config_1 == ["2s2", "2p6"]
    assert in1_instance1.get_energy("1", "2") == sp1_orig_energy_2 == 583.276
    assert in1_instance1.get_stat_weight("1", "2") == sp1_orig_stat_weight_2 == 5.0
    assert in1_instance1.get_config("1", "2") == sp1_orig_config_2 == ["2p5", "3s1"]

    # Verify branching ratio data for sp_num 2
    config_2s2_2p5 = tuple(["2s2", "2p5"])
    config_2s2_2p6 = tuple(["2s2", "2p6"])
    assert in1_instance1._branching_ratio["2"][config_2s2_2p5] == in1_instance2._branching_ratio["2"][config_2s2_2p5] == ["1", "2"]
    assert in1_instance1._branching_ratio["2"][config_2s2_2p6] == in1_instance2._branching_ratio["2"][config_2s2_2p6] == ["3"]

    # Verify branching ratio data for sp_num 1 remains unchanged
    config_2s2_2p6 = tuple(["2s2", "2p6"])
    config_2p5_3s1 = tuple(["2p5", "3s1"])
    assert in1_instance1._branching_ratio["1"][config_2s2_2p6] == ["1"]
    assert in1_instance1._branching_ratio["1"][config_2p5_3s1] == ["2"]