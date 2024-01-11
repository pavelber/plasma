#!/usr/bin/python
import os
import sys

OUTPUT_FORMAT_STRING = '%5s%5s%6s%5s%14s%14s%14s%14s'
MAX_LINES = 1000000
COLUMNS_IN_BLOCK_START = 6


def start_file(lines, in_file, odir, file_num, l):
    out_file = odir + os.path.sep + os.path.basename(in_file) + str(file_num)
    out = open(out_file, 'w')
    for h in lines:
        out.write(h)
    out.write(l)
    return out


class Res:
    def __init__(self, err, out, continue_processing):
        self.err = err
        self.out = out
        self.continue_processing = continue_processing

    @classmethod
    def done(cls, line):  # Don't process this line more
        return cls(None, line + os.linesep, False)

    @classmethod
    def insert(cls, line):  # Don't process this line more
        return cls(None, line + os.linesep, True)

    @classmethod
    def done_line(cls, line):  # Don't process this line more
        return cls(None, line, False)

    @classmethod
    def err(cls, message):  # raise exception
        return cls(message, None, False)

    @classmethod
    def skip(cls):  # this processor is not relevant for this line
        return cls(None, None, True)

    @classmethod
    def delete(cls):  # this processor is not relevant for this line
        return cls(None, None, False)


def default_processor(line_number, line):
    return Res.done_line(line)


def fac_processor(line_number, line):
    if line_number == 1:
        if line.startswith('cFAC'):
            return Res.done('FAC 1.1.1')
        else:
            return Res.err('Expected cFAC 1.6.3 in the first line of file ')
    else:
        return Res.skip()


def qkmode_processor(line_number, line):
    if line_number == 11:
        return Res.insert('QKMODE = 0')
    else:
        return Res.skip()


def error(s):
    sys.stderr.write(s + '\n')
    exit(1)


def convert(in_file, out_file, processors, optional=False):
    procs = processors + [default_processor]
    if os.path.exists(in_file):
        with open(in_file, 'r') as inf:
            with open(out_file, 'w') as outf:
                for line_number, line in enumerate(inf, 1):
                    for p in procs:
                        res = p(line_number, line)
                        if res.err is not None:
                            error(res.err + " in " + in_file)
                        if res.out is not None:
                            outf.write(res.out)
                        if not res.continue_processing:
                            break

    elif not optional:
        error('Can\'t open transitions file ' + in_file)


def is_zero(c):
    return float(c) == 0.0


def null_line(columns):
    return is_zero(columns[4]) and is_zero(columns[5]) and is_zero(columns[7])


class TrProcessor:
    def __init__(self):
        self.block_num = 0
        self.block_1_lines = 0
        self.block_4_lines = 0

    def change_ntrans(self, line_number, line):
        if line.startswith('NELE'):
            self.block_num += 1
            return Res.skip()
        else:
            if line.startswith('NTRANS'):
                if self.block_num == 2 or self.block_num == 3:
                    return Res.skip()
                else:
                    if self.block_num == 1:
                        return Res.done('NTRANS	= ' + str(self.block_1_lines))
                    if self.block_num == 4:
                        return Res.done('NTRANS	= ' + str(self.block_4_lines))
        return Res.skip()

    def skip_lines(self, line_number, line):
        if line.startswith('NELE'):
            self.block_num += 1
            return Res.skip()
        elif line.startswith('NBlocks'):
            self.block_count = line.split()[2]
            return Res.skip()
        else:
            columns = line.split()
            if len(columns) != 9:
                return Res.skip()
            if null_line(columns):
                if (self.block_num == 2 or self.block_num == 3) and self.block_count == "4":
                    return Res.err("0s in block N " + str(self.block_num))
                else:  # 1 or 4
                    return Res.delete()
            else:
                if self.block_num == 1:
                    self.block_1_lines += 1
                if self.block_num == 4:
                    self.block_4_lines += 1
                out = OUTPUT_FORMAT_STRING % (
                    columns[0], columns[1], columns[2], columns[3],
                    columns[4], columns[6], columns[7], columns[8])
                return Res.done(out)


################## MAIN ######################
if len(sys.argv) != 3:
    error('Usage: ' + sys.argv[0] + 'directory-with-cFAC-1.6.3-files output-directory')

in_dir = sys.argv[1]
out_dir = sys.argv[2]

if os.path.exists(out_dir):
    error('Output directory ' + out_dir + ' should not exist')
else:
    os.makedirs(out_dir)

ai = os.path.sep + "fac.ai"
ce = os.path.sep + "fac.ce"
ci = os.path.sep + "fac.ci"
lev = os.path.sep + "fac.lev"
tr = os.path.sep + "fac.tr"
rr = os.path.sep + "fac.rr"

tr_processor = TrProcessor()

ai_processors = [fac_processor]
ci_processors = [fac_processor]
lev_processors = [fac_processor]
rr_processors = [fac_processor]
ce_processors = [fac_processor, qkmode_processor]
tr_pass_1_processors = [fac_processor, tr_processor.skip_lines]
tr_pass_2_processors = [tr_processor.change_ntrans]

convert(in_dir + ai, out_dir + ai, ai_processors, True)
convert(in_dir + ce, out_dir + ce, ce_processors)
convert(in_dir + ci, out_dir + ci, ci_processors)
convert(in_dir + lev, out_dir + lev, lev_processors)
convert(in_dir + rr, out_dir + rr, rr_processors)

convert(in_dir + tr, out_dir + tr + '.tmp', tr_pass_1_processors)
tr_processor.block_num = 0

convert(out_dir + tr + '.tmp', out_dir + tr, tr_pass_2_processors)
os.remove(out_dir + tr + '.tmp')

# os.remove(out_dir + ce)
# os.remove(out_dir + rr)
