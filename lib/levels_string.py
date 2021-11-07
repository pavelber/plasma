from lib.utils import error

levels_order = ["1s", "2s", "2p", "3s", "3p", "3d", "4s", "4p", "4d", "4f", "5s", "5p", "5d", "5f", "6s", "6p", "6d"]
level_to_electrons = {
    "1s": 2,
    "2s": 2,
    "2p": 6,
    "3s": 2,
    "3p": 6,
    "3d": 10,
    "4s": 2,
    "4p": 6,
    "4d": 10,
    "4f": 14,
    "5s": 2,
    "5p": 6,
    "5d": 10,
    "5f": 14,
    "6s": 2,
    "6p": 6,
    "6d": 10
}


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
                holes[column[0:2]] = int(column[2:])
        i += 1
    should_be = sum(num_to_electrons.values())
    if should_be != num_of_electrons:
        error("Got different electrons number in " + line + ". Should be " + str(should_be) + ", got " + str(
            num_of_electrons))
    # print num_of_electrons
    # print num_to_electrons
    # print holes
    if '1' not in num_to_electrons:
        num_to_electrons['1'] = 0

    result = []

    # remove holes num of electrons in advance
    for hole_level in holes:
        num_to_electrons[hole_level[0:1]] -= holes[hole_level]

    go_until_holes = max([int(k[0:1]) for k in holes.keys()])
    go_until_num_of = max([int(k[0:1]) for k in num_to_electrons.keys()])
    go_until = max(go_until_holes, go_until_num_of)
    current = '1'
    for level in levels_order:
        now = level[0:1]
        if now != current:
            if num_to_electrons[current] > 0:  # got to next level, not all electrons used
                error("Now: " + level + " result until now: " + str(result))
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
        error("Remained electrons")

    return ' '.join(result)


# 1s2 2s2 2p6 3s2 3p6 3d10 4s2 4p6 4d10 5s2 5p6 4f14 5d10 6s2 6p6 5f3 6d1 7s2
