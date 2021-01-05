#!/usr/bin/python
import os
import sys

MAX_LINES = 1000000
COLUMNS_IN_BLOCK_START = 6


def error(s):
    sys.stderr.write(s + '\n')
    exit(1)


def start_file(lines, in_file, odir, file_num, l):
    out_file = odir + os.path.sep + os.path.basename(in_file) + str(file_num)
    out = open(out_file, 'wb')
    for h in lines:
        out.write(h)
    out.write(l)
    return out


def split(in_file, out_dir):
    if os.path.exists(in_file):
        header = True
        header_lines = []
        file_counter = 0
        line_counter = 0

        with open(in_file, 'rb') as inf:
            for line in inf:
                columns = line.split()
                if len(columns) < COLUMNS_IN_BLOCK_START and header:  # Header
                    header_lines.append(line)
                elif len(columns) == COLUMNS_IN_BLOCK_START and header:  # End of header
                    header = False
                    out_f = start_file(header_lines, in_file, out_dir, file_counter, line)
                elif not header and len(columns) == COLUMNS_IN_BLOCK_START and line_counter >= MAX_LINES:  # exceeded
                    out_f.close()
                    file_counter += 1
                    line_counter = 0
                    out_f = start_file(header_lines, in_file, out_dir, file_counter, line)
                elif not header and len(columns) == COLUMNS_IN_BLOCK_START and line_counter < MAX_LINES:  # regular line
                    out_f.write(line)
                    line_counter += 1
                elif not header and len(columns) < COLUMNS_IN_BLOCK_START:  # regular line
                    out_f.write(line)
                    line_counter += 1
                else:
                    error("Should not be there")
        out_f.close()

    else:
        error('Can\'t open input file ' + in_file)


################## MAIN ######################
if len(sys.argv) != 3:
    error('Usage: ' + sys.argv[0] + ' input-file output-directory')

in_file = sys.argv[1]
out_dir = sys.argv[2]
split(in_file, out_dir)
