def get_number_of_electrons(from_config, to_config):
    """
    Determine number of electrons in the subshell that lost electrons.
    It is called also nl

    Args:
        from_config (list): Initial electron configuration as list of two strings (e.g., ['1s2', '5g1'])
        to_config (list): Final electron configuration as list of two strings (e.g., ['1s1', '5g1'])

    Returns:
        int: Number of electrons in the subshell that lost electrons
    """
    # Check first subshell
    from_n1 = from_config[0][0:2]
    from_electrons1 = int(from_config[0][2])

    to_n1 = to_config[0][0:2]
    to_electrons1 = int(to_config[0][2])

    if len(to_config) == 1:
        to_config.append(from_config[1][:-1] + "0")
    # Check second subshell
    from_n2 = from_config[1][0:2]
    from_electrons2 = int(from_config[1][2])

    to_n2 = to_config[1][0:2]
    to_electrons2 = int(to_config[1][2])

    # Check which subshell lost electrons
    if from_n1 == to_n1 and from_electrons1 > to_electrons1:
        return from_electrons1
    elif from_n2 == to_n2 and from_electrons2 > to_electrons2:
        return from_electrons2
    elif from_n1 == to_n2:
        return from_electrons2
    elif from_config[1] == to_config[1] and from_config[0] != to_config[0]:
        return from_electrons1
    elif from_config[0] == to_config[0] and to_electrons2 == 0:
        return from_electrons2

    return None
