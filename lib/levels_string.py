import re

from lib.exceptions import GenericPlasmaException

letters_order = ['s', 'p', 'd', 'f', 'g', 'h', 'i', 'k', 'l', 'm', 'n']
letters_to_order = {letter: str(index) for index, letter in enumerate(letters_order)}
_config_re = re.compile(r'^(\d+)([a-z])(\d*)$')


def get_level_label_parts(config):
    match = _config_re.match(config)
    if match is None:
        raise GenericPlasmaException("Invalid level configuration: " + config)
    shell, letter, electrons = match.groups()
    return int(shell), letter, int(electrons) if electrons else None


def remove_level_electrons(config):
    shell, letter, electrons = get_level_label_parts(config)
    if electrons is None:
        return config
    return str(shell) + letter


def build_levels_order(max_shell):
    levels = []
    for shell in range(1, max_shell + 1):
        for letter in letters_order[:shell]:
            levels.append(str(shell) + letter)
    return levels


def max_electrons_for_level(level):
    shell, letter, electrons = get_level_label_parts(level)
    return 4 * letters_order.index(letter) + 2


levels_order = build_levels_order(20)
level_to_electrons = {level: max_electrons_for_level(level) for level in levels_order}


def create_levels_string(num_of_electrons, line):
    columns = line.split()

    num = num_of_electrons
    i = 6
    num_to_electrons = {}
    holes = {}
    while i < len(columns):
        column = columns[i].rstrip("*")  # remove trailing stars
        star_index = column.find("*")
        if 0 < star_index < len(column) - 1:
            num_to_electrons[column[0:star_index]] = int(column[star_index + 1:])
        else:
            if '+' in column or '-' in column:
                break
            else:
                holes[remove_level_electrons(column)] = get_level_label_parts(column)[2]
        i += 1
    should_be = sum(num_to_electrons.values())
    if should_be != num_of_electrons:
        raise GenericPlasmaException(
            "Got different electrons number in " + line + ". Should be " + str(should_be) + ", got " + str(
                num_of_electrons))
    # print num_of_electrons
    # print num_to_electrons
    # print holes
    if '1' not in num_to_electrons:
        num_to_electrons['1'] = 0

    result = []

    # remove holes num of electrons in advance
    for hole_level in holes:
        hole_shell = str(get_level_label_parts(hole_level)[0])
        num_to_electrons[hole_shell] -= holes[hole_level]

    go_until_holes = max([get_level_label_parts(k)[0] for k in holes.keys()])
    go_until_num_of = max([int(k) for k in num_to_electrons.keys()])
    go_until = max(go_until_holes, go_until_num_of)
    current = '1'
    for level in levels_order:
        now = str(get_level_label_parts(level)[0])
        if now != current:
            if num_to_electrons[current] > 0:  # got to next level, not all electrons used
                raise GenericPlasmaException("Now: " + level + " result until now: " + str(result))
            else:
                current = now
                if current not in num_to_electrons and int(current) > go_until:
                    break
        if level in holes:
            # num_to_electrons[current] -= holes[level]
            result.append(level + str(holes[level]))
        else:
            num_in_level = level_to_electrons[level]
            if current not in num_to_electrons:
                num_to_electrons[current] = 0
            if num_to_electrons[current] < num_in_level:
                if num_to_electrons[current] > 0:
                    result.append(level + str(num))
                num_to_electrons[current] = 0
            else:
                result.append(level + str(num_in_level))
                num_to_electrons[current] = num_to_electrons[current] - num_in_level

    remain_electrons = sum(num_to_electrons.values())
    if remain_electrons > 0:
        raise GenericPlasmaException("Remained electrons")

    return ' '.join(result)


def find_previous(c):
    config_wo_electrons = remove_level_electrons(c)
    index = levels_order.index(config_wo_electrons)
    if index == 0:
        return ""
    return levels_order[index - 1] + str(level_to_electrons[levels_order[index - 1]])

# 1s2 2s2 2p6 3s2 3p6 3d10 4s2 4p6 4d10 5s2 5p6 4f14 5d10 6s2 6p6 5f3 6d1 7s2
