import csv
import os
import sys

HEADER_FORMAT_STRING = '%3s  %3s  %3s %8s %8s'
OUTPUT_FORMAT_STRING = '%24s%5s%13s%7d%9d'
OUTPUT_FORMAT_STRING2 = '%24s%5s%13s%7d%9d%9s%15s'

levels_order = ["1s", "2s", "2p", "3s", "3p", "3d", "4s", "4p", "4d", "5s", "5p", "4f", "5d", "6s", "6p", "5f", "6d"]
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


def create_levels_string(level1, level2, num_of_electrons):
    num = num_of_electrons
    result = []
    (level1_name, level1_num) = parse_level(level1)
    (level2_name, level2_num) = parse_level(level2)
    num = num - level2_num - level1_num
    for level in levels_order:
        if level == level1_name and level1_num > 0:
            result.append(level1_name + str(level1_num))
        elif level == level2_name:
            result.append(level2_name + str(level2_num))
            break
        else:
            num_in_level = level_to_electrons[level]
            if num < num_in_level:
                if num > 0:
                    result.append(level + str(num))
                num = 0
            else:
                result.append(level + str(num_in_level))
                num = num - num_in_level

    return ' '.join(result)


# 1s2 2s2 2p6 3s2 3p6 3d10 4s2 4p6 4d10 5s2 5p6 4f14 5d10 6s2 6p6 5f3 6d1 7s2

def error(s):
    sys.stderr.write(s + '\n')
    exit(1)


def print_header():
    print HEADER_FORMAT_STRING % ('SpS', 'QSs', 'AI', 'FAC PI', 'NIST PI')
    print '------------------------------------------'


def skip_n_lines(f, num):
    for _ in range(num):
        next(f)


def skip_lines(f):
    skip_n_lines(f, 12)


def copy_lines(f, element):
    el = int(name_to_table[element]["AtomicNumber"])
    for line in f:
        columns = line.split()
        if len(columns) == 7:
            num = el - int(columns[0]) + 1
            if num == 0:
                break
            name = num_to_table[str(num)]["Symbol"]
            print HEADER_FORMAT_STRING % (columns[0], columns[1], columns[2], columns[4], columns[6]),
            print(" [" + name + "]")
        else:
            break


def copy_atomic(f, element):
    el = int(name_to_table[element]["AtomicNumber"])
    counter = 1
    block_counter = 1
    autoionization = False
    autoionization_levels = {}

    for line in f:
        columns = line.split()
        if len(columns) == 1:
            autoionization = False
            e = columns[0]
            num = el - int(columns[0]) + 1
            if num == 0:
                print "33"
                print OUTPUT_FORMAT_STRING % ("nucleus", "1", "0.000", 1, counter)
                counter += 1
                break
            name = num_to_table[str(num)]["Symbol"]
            if counter == 1:  # first time
                print(e + " [" + name + "]" + "                    g0        E(eV)       #       ##")
            else:
                print(e + " [" + name + "]")
            block_counter = 1
        elif len(columns) == 7:
            if not autoionization:
                print OUTPUT_FORMAT_STRING % (
                    create_levels_string(columns[0], columns[1], num), columns[2], columns[3], block_counter, counter)
                counter += 1
                block_counter += 1
            else:  # Store autoionization
                autoionization_lines = autoionization_levels[e]
                autoionization_lines.append(columns)

        elif len(columns) == 9:
            if not autoionization:
                print OUTPUT_FORMAT_STRING2 % (
                    create_levels_string(columns[0], columns[1], num), columns[2], columns[3], block_counter, counter,
                    columns[7],
                    columns[8])
                counter += 1
                block_counter += 1
        elif len(columns) == 2:
            autoionization = True
            autoionization_levels[e] = []
        else:
            print(line),

    for e in sorted(autoionization_levels):
        lines = autoionization_levels[e]
        num = el - int(e) + 1
        name = num_to_table[str(num)]["Symbol"]
        print(e + " " + name + "-like AIs")
        block_counter = -1
        for columns in lines:
            print OUTPUT_FORMAT_STRING % (
                create_levels_string(columns[0], columns[1], num), columns[2], columns[3], block_counter, counter)
            block_counter -= 1
            counter += 1


def read_element(inp):
    line = inp.readline()
    columns = line.split()
    return columns[0]


def read_table():
    dict1 = {}
    dict2 = {}
    path = os.path.dirname(__file__)
    if path == "" or path is None:
        path = "."
    table_file = path + os.path.sep + "PeriodicTable.csv"
    print table_file
    with open(table_file, 'rb') as infile:
        reader = csv.reader(infile)
        headers = next(reader)[0:]
        for row in reader:
            dict1[row[2]] = {key: value for key, value in zip(headers, row[0:])}
            dict2[row[0]] = {key: value for key, value in zip(headers, row[0:])}
    return dict1, dict2


if len(sys.argv) != 2:
    error('Usage: ' + sys.argv[0] + ' path_to_IN1R.INP')

(name_to_table, num_to_table) = read_table()

if os.path.exists(sys.argv[1]):
    with open(sys.argv[1], 'rb') as inp:
        element = read_element(inp)
        print_header()
        skip_lines(inp)
        copy_lines(inp, element)
        print "----------------------------------------------------------------"
        copy_atomic(inp, element)
else:
    error('Can\'t open file ' + sys.argv[1])
