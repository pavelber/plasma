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
    from_n1 = from_config[0][0:2]
    from_electrons1 = int(from_config[0][2])

    # Extract info from first subshell of to_config
    to_n1 = to_config[0][0:2]
    to_electrons1 = int(to_config[0][2])

    # If either config has only 1 subshell, pad the second with a default "0" subshell
    if len(from_config) == 1:
        from_config.append(to_config[0][:-1] + "0")  # e.g., if '1s2' -> '1s0'
    if len(to_config) == 1:
        to_config.append(from_config[0][:-1] + "0")  # e.g., if '1s2' -> '1s0'

    # Extract info from second subshell
    from_n2 = from_config[1][0:2]
    from_electrons2 = int(from_config[1][2])

    to_n2 = to_config[1][0:2]
    to_electrons2 = int(to_config[1][2])

    # Check which subshell lost electrons
    if from_n1 == to_n1 and from_electrons1 > to_electrons1:
        return from_electrons1, 0
    elif from_n2 == to_n2 and from_electrons2 > to_electrons2:
        return from_electrons2, 1
    elif from_n1 == to_n2:
        return from_electrons2, 1
    elif from_config[1] == to_config[1] and from_config[0] != to_config[0]:
        return from_electrons1, 0
    elif from_config[0] == to_config[0] and to_electrons2 == 0:
        return from_electrons2, 1

    return None, None
