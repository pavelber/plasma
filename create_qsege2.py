import os
import sys

from lib.utils import read_table, skip_n_lines

HEADER_FORMAT_STRING = '%3s  %3s  %3s   %8s  %8s'
OUTPUT_FORMAT_STRING = '%24s%5s%13s%7d%9d'
OUTPUT_FORMAT_STRING_AI = '%24s%5s%13s'
OUTPUT_FORMAT_STRING_AI2 = '%s%7d%9d'
OUTPUT_FORMAT_STRING2 = '%24s%5s%13s%7d%9d%9s%15s'

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


def parse_level(s):
    level = s[0:2]
    num = int(s[2:])
    return level, num


def create_levels_string(num_of_electrons, fac_file):
    line = fac_file.readline()
    columns = line.split()

    num = num_of_electrons
    i = 6
    num_to_electrons = {}
    holes = {}
    while i < len(columns):
        star_index = columns[i].find("*")
        if star_index > 0:
            num_to_electrons[columns[i][0:star_index]] = int(columns[i][star_index + 1:])
        else:
            if '+' in columns[i] or '-' in columns[i]:
                break
            else:
                holes[columns[i][0:2]] = int(columns[i][2:])
        i += 1
    should_be = sum(num_to_electrons.values())
    if should_be != num_of_electrons:
        error("Got different electrons number in " + line)
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

def error(s):
    sys.stderr.write(s + '\n')
    exit(1)


def print_header():
    print(HEADER_FORMAT_STRING % ('SpS', 'QSs', 'AI', 'FAC PI', 'NIST PI'))
    print('------------------------------------------')


def skip_lines(f):
    skip_n_lines(f, 12)


def verify_fac(el, fac_dir):
    path = fac_dir + os.path.sep + str(el) + os.path.sep + "fac.lev"
    if not os.path.exists(path):
        error("Expected file fac lev at " + path)


def copy_lines(f, element, fac_dir):
    el = int(name_to_table[element]["AtomicNumber"])
    verify_fac(el, fac_dir)
    for line in f:
        columns = line.split()
        if len(columns) == 7:
            num = el - int(columns[0]) + 1
            if num == 0:
                break
            name = num_to_table[str(num)]["Symbol"]
            print(
                    HEADER_FORMAT_STRING % (
                columns[0], columns[1], columns[2], columns[4], columns[6]) + "  [" + name + "]")
        else:
            break


def read_until(file, prefix):
    line = ""
    while prefix not in line:
        line = file.readline()


def copy_atomic(f, element, fac_dir):
    el = int(name_to_table[element]["AtomicNumber"])
    counter = 1
    block_counter = 1
    autoionization = False
    autoionization_levels = {}
    fac_file = None
    for line in f:
        columns = line.split()
        if len(columns) == 1:
            autoionization = False
            e = columns[0]
            num = el - int(e) + 1
            if fac_file is not None:
                fac_file.close()

            if num == 0:
                print("33")
                print(OUTPUT_FORMAT_STRING % ("nucleus", "1", "0.000", 1, counter))
                counter += 1
                break
            fac_file_name = fac_dir + os.path.sep + e + os.path.sep + "fac.lev"
            fac_file = open(fac_file_name, "r")
            read_until(fac_file, "  ILEV")
            name = num_to_table[str(num)]["Symbol"]
            if counter == 1:  # first time
                print(e + " [" + name + "]" + "                    g0       E(eV)       #       ##   ")
            else:
                print(e + " [" + name + "]")
            block_counter = 1
        elif len(columns) == 7:
            if not autoionization:
                print(OUTPUT_FORMAT_STRING % (
                    create_levels_string(num, fac_file), columns[2], columns[3], block_counter, counter))
                counter += 1
                block_counter += 1
            else:  # Store autoionization
                autoionization_lines = autoionization_levels[e]
                autoionization_lines.append(
                    OUTPUT_FORMAT_STRING_AI % (create_levels_string(num, fac_file), columns[2], columns[3]))
        elif len(columns) == 9:
            if not autoionization:
                print  (OUTPUT_FORMAT_STRING2 % (
                    create_levels_string(num, fac_file), columns[2], columns[3], block_counter,
                    counter,
                    columns[7],
                    columns[8]))
                counter += 1
                block_counter += 1
        elif len(columns) == 2:
            autoionization = True
            num = el - int(e) + 1
            name = num_to_table[str(num)]["Symbol"]
            autoionization_levels[e] = []
        else:
            print(line),

    if fac_file is not None:
        fac_file.close()

    for e in sorted(autoionization_levels):
        lines = autoionization_levels[e]
        num = el - int(e) + 1
        name = num_to_table[str(num)]["Symbol"]
        print(e + " " + name + "-like AIs")
        block_counter = -1
        for ai_line in lines:
            print(OUTPUT_FORMAT_STRING_AI2 % (ai_line, block_counter, counter))
            block_counter -= 1
            counter += 1


def read_element(inp):
    line = inp.readline()
    columns = line.split()
    return columns[0]


if len(sys.argv) != 3:
    error('Usage: ' + sys.argv[0] + ' path_to_IN1R.INP fac_directory')

(name_to_table, num_to_table) = read_table()

if os.path.exists(sys.argv[1]):
    with open(sys.argv[1], 'rb') as inp:
        element = read_element(inp)
        print_header()
        skip_lines(inp)
        copy_lines(inp, element, sys.argv[2])
        print("----------------------------------------------------------------")
        copy_atomic(inp, element, sys.argv[2])
else:
    error('Can\'t open file ' + sys.argv[1])
