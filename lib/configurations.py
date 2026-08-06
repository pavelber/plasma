from lib.levels_string import get_level_label_parts, remove_level_electrons


def _split_config(config):
    level = remove_level_electrons(config)
    electrons = get_level_label_parts(config)[2]
    return level, electrons


def get_number_of_electrons(from_config, to_config):
    """
    Determine number of electrons in the subshell that lost electrons.
    It is called also nl

    Args:
        from_config (list): Initial electron configuration as list of strings (e.g., ['1s2'] or ['1s2', '5g1'])
        to_config (list): Final electron configuration as list of strings (e.g., ['1s1'] or ['1s1', '5g1'])

    Returns:
        (int: Number of electrons in the subshell that lost electrons, int: index of configuration that lost electrons (0 or 1)
    """
    # Handle case when to_config is just "Nucleus"
    if to_config[0] == "Nucleus":
        return None, None

    # Extract info from first subshell of from_config
    from_n1, from_electrons1 = _split_config(from_config[0])

    # Extract info from first subshell of to_config
    to_n1, to_electrons1 = _split_config(to_config[0])

    # Case: single subshell losing electrons
    if len(from_config) == 1 and len(to_config) >= 1:
        if from_n1 == to_n1 and from_electrons1 > to_electrons1:
            return from_electrons1, 0
        # Check if our subshell appears as second subshell in to_config
        if len(to_config) > 1:
            to_n2, to_electrons2 = _split_config(to_config[1])
            if from_n1 == to_n2 and from_electrons1 > to_electrons2:
                return from_electrons1, 0
        return None, None

    # Case: checking first subshell
    if from_n1 == to_n1 and from_electrons1 > to_electrons1:
        return from_electrons1, 0

    # Case: checking second subshell if it exists
    if len(from_config) > 1:
        from_n2, from_electrons2 = _split_config(from_config[1])

        # Check if second subshell in from_config exists in to_config
        found = False

        if len(to_config) > 1:
            to_n2, to_electrons2 = _split_config(to_config[1])

            if from_n2 == to_n2 and from_electrons2 > to_electrons2:
                return from_electrons2, 1

            found = from_n2 == to_n2

        # Special case: second subshell in from_config doesn't exist in to_config
        if not found:
            # If the second subshell is completely gone, it lost all its electrons
            return from_electrons2, 1

    # No electron loss detected
    return None, None