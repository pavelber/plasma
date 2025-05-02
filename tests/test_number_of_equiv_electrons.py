import unittest

from lib.configurations import get_number_of_electrons


class TestGetNumberOfElectrons(unittest.TestCase):
    def test_electron_loss_in_first_subshell(self):
        # Test case: Electron lost in first subshell (1s2 -> 1s1)
        from_config = ["1s2"]
        to_config = ["1s1"]
        result = get_number_of_electrons(from_config, to_config)
        self.assertEqual(result, (2, 0))  # 2 electrons in 1s, index 0

    def test_electron_loss_in_second_subshell(self):
        # Test case: Electron lost in second subshell (1s2, 2s2 -> 1s2, 2s1)
        from_config = ["1s2", "2s2"]
        to_config = ["1s2", "2s1"]
        result = get_number_of_electrons(from_config, to_config)
        self.assertEqual(result, (2, 1))  # 2 electrons in 2s, index 1

    def test_single_subshell_and_reordering(self):
        # Test case: Single subshell in from_config and different ordering in to_config
        from_config = ["2s2"]
        to_config = ["1s2", "2s1"]
        result = get_number_of_electrons(from_config, to_config)
        self.assertEqual(result, (2, 0))  # 2 electrons in 2s, index 0

    def test_second_subshell_removed(self):
        # Test case: Two subshells in from_config, only first subshell in to_config
        from_config = ["1s2", "2s1"]
        to_config = ["1s2"]
        result = get_number_of_electrons(from_config, to_config)
        self.assertEqual(result, (1, 1))  # 1 electron in 2s, index 1
    def test_nucleus_case(self):
        # Test case: to_config is "Nucleus"
        from_config = ["1s2"]
        to_config = ["Nucleus"]
        result = get_number_of_electrons(from_config, to_config)
        self.assertEqual(result, (None, None))  # Should return None, None

    def test_single_subshell_padding(self):
        # Test case: Single subshell in from_config, electron loss in first subshell
        from_config = ["1s2"]
        to_config = ["1s1", "2s1"]
        result = get_number_of_electrons(from_config, to_config)
        self.assertEqual(result, (2, 0))  # 2 electrons in 1s, index 0


    def test_no_electron_loss_in_first(self):
        # Test case: No electron loss in first subshell, loss in second
        from_config = ["1s2", "2p3"]
        to_config = ["1s2", "2p2"]
        result = get_number_of_electrons(from_config, to_config)
        self.assertEqual(result, (3, 1))  # 3 electrons in 2p, index 1

    def test_zero_electrons_in_second_subshell(self):
        # Test case: Second subshell has 0 electrons in to_config
        from_config = ["1s2", "2s2"]
        to_config = ["1s2", "2s0"]
        result = get_number_of_electrons(from_config, to_config)
        self.assertEqual(result, (2, 1))  # 2 electrons in 2s, index 1

    def test_no_electron_loss(self):
        # Test case: No electron loss, same configurations
        from_config = ["1s2", "2s2"]
        to_config = ["1s2", "2s2"]
        result = get_number_of_electrons(from_config, to_config)
        self.assertEqual(result, (None, None))  # No loss, return None, None

if __name__ == "__main__":
    unittest.main()