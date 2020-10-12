import os
import sys

HEADER_FORMAT_STRING = ' %2s %3s %3s %8s %8s'
OUTPUT_FORMAT_STRING = ' %2s %3s %3s %5.2f %5.2f'


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
    skip_n_lines(f, 13)


def copy_lines(f):
    for line in f:
        columns = line.split()
        if len(columns) == 7:
            print(line),
        else:
            break


def copy_atomic(f):
    for line in f:
        columns = line.split()
        if len(columns) == 7:
            print HEADER_FORMAT_STRING % (columns[0], columns[1], columns[2], columns[4], columns[6])

        else:
            print(line),


if len(sys.argv) != 3:
    error('Usage: ' + sys.argv[0] + ' path_to_IN1R.INP path_to_fac_files_dir')

if os.path.exists(sys.argv[1]):
    with open(sys.argv[1], 'rb') as inp:
        print_header()
        skip_lines(inp)
        copy_lines(inp)
        copy_atomic(inp)
else:
    error('Can\'t open file ' + sys.argv[1])
