import csv
import os
import sys

HEADER_FORMAT_STRING = '%3s  %3s  %3s %8s %8s'
OUTPUT_FORMAT_STRING = '%24s%5s%13s%7d%9d'
OUTPUT_FORMAT_STRING2 = '%24s%5s%13s%7d%9d%9s%15s'


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

    for line in f:
        columns = line.split()
        if len(columns) == 1:
            autoionization = False
            e = columns[0]
            num = el - int(columns[0]) + 1
            if num == 0:
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
                    columns[0] + " " + columns[1], columns[2], columns[3], block_counter, counter)
                counter += 1
                block_counter += 1
        elif len(columns) == 9:
            if not autoionization:
                print OUTPUT_FORMAT_STRING2 % (
                    columns[0] + " " + columns[1], columns[2], columns[3], block_counter, counter, columns[7],
                    columns[8])
                counter += 1
                block_counter += 1
        elif len(columns) == 2:
            autoionization = True
        else:
            print(line),


def read_element(inp):
    line = inp.readline()
    columns = line.split()
    return columns[0]


def read_table():
    dict1 = {}
    dict2 = {}
    with open(os.path.dirname(__file__) + os.path.sep + "PeriodicTable.csv", 'rb') as infile:
        reader = csv.reader(infile)
        headers = next(reader)[0:]
        for row in reader:
            dict1[row[2]] = {key: value for key, value in zip(headers, row[0:])}
            dict2[row[0]] = {key: value for key, value in zip(headers, row[0:])}
    return dict1, dict2


if len(sys.argv) != 2:
    error('Usage: ' + sys.argv[0] + ' path_to_IN1R.INP')

(name_to_table, num_to_table) = read_table()

print os.path.abspath(__file__)
print os.path.dirname(__file__)

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
