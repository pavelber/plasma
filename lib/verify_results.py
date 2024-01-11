import os

from lib.utils import error


def test_number_of_levels_inp1(in1_inp):
    with open(in1_inp, "r") as f:
        num_per_sp = {}
        for l in f:
            fields = l.split()
            if len(fields) == 1:
                if 'sp_num' in locals() and num_per_sp[sp_num] != level_num:
                    error("Level number in " + sp_num)
                if 'sp_num' in locals() and level_num == 0:
                    error("Level number 0 in " + sp_num)
                sp_num = fields[0]
                level_num = 0
            if len(fields) == 10 and fields[9] == '0.000':
                num_in_header = int(fields[1])
                if num_in_header == 0:
                    error("Level number 0 in " + sp_num)
                num_per_sp[fields[0]] = num_in_header
            if len(fields) == 7:
                fields.insert(0, "")
            if len(fields) == 8:
                level_num += 1
                if int(fields[7]) != level_num:
                    error("Level number in " + l)


def check_file(f_path, required_lines):
    with open(f_path, "r") as f:
        num = len(f.readlines())
        if num < required_lines:
            error("In file " + f_path + " less than " + str(required_lines) + " lines")


def files_not_empty(elem_dir):
    check_file(os.path.join(elem_dir, "RREC.INP"), 20)
    check_file(os.path.join(elem_dir, "EXCIT.INP"), 20)
    check_file(os.path.join(elem_dir, "SPECTR.INP"), 20)
    check_file(os.path.join(elem_dir, "BFCP.INP"), 20)
    check_file(os.path.join(elem_dir, "IN1.INP"), 20)
