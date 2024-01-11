import os
import sys

from lib.levels_string import create_levels_string
from lib.utils import read_table, skip_n_lines, error

HEADER_FORMAT_STRING = '%3s  %3s  %3s   %8s'
OUTPUT_FORMAT_STRING = '%24s%5s%13s%7d%9d'
OUTPUT_FORMAT_STRING_AI = '%24s%5s%13s'
OUTPUT_FORMAT_STRING_AI2 = '%s%7d%9d'
OUTPUT_FORMAT_STRING2 = '%24s%5s%13s%7d%9d%9s%15s'


def print_header():
    print(HEADER_FORMAT_STRING % ('SpS', 'QSs', 'AI', 'FAC PI'))
    print('------------------------------------------')


def skip_lines(f):
    skip_n_lines(f, 12)


def verify_fac(el, fac_dir):
    path = fac_dir + os.path.sep + str(el) + os.path.sep + "fac.lev"
    if not os.path.exists(path):
        error("Expected file fac lev at " + path)


def copy_lines(f, element, fac_dir):
    el = int(name_to_table[element]["AtomicNumber"])
    #verify_fac(el, fac_dir)
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
                line = fac_file.readline()
                print(OUTPUT_FORMAT_STRING % (
                    create_levels_string(num, line), columns[2], columns[3], block_counter, counter))
                counter += 1
                block_counter += 1
            else:  # Store autoionization
                autoionization_lines = autoionization_levels[e]
                line = fac_file.readline()
                autoionization_lines.append(
                    OUTPUT_FORMAT_STRING_AI % (create_levels_string(num, line), columns[2], columns[3]))
        elif len(columns) == 9:
            if not autoionization:
                line = fac_file.readline()
                print (OUTPUT_FORMAT_STRING2 % (
                    create_levels_string(num, line), columns[2], columns[3], block_counter,
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
    with open(sys.argv[1], 'r') as inp:
        element = read_element(inp)
        print_header()
        skip_lines(inp)
        copy_lines(inp, element, sys.argv[2])
        print("----------------------------------------------------------------")
        copy_atomic(inp, element, sys.argv[2])
else:
    error('Can\'t open file ' + sys.argv[1])
