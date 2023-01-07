import sys

from lib.iterate_photo_cross import compute_and_iterate
from lib.utils import error


def read_element(in1):
    l = in1.readline().split()
    return l[0], int(l[1])


def read_n0l0(in1):
    sp_num_to_n0l0 = {}
    while True:
        l = in1.readline().split()
        if len(l) < 5 or float(l[4]) == 0.0:
            break
        sp_num_to_n0l0[l[0]] = float(l[4])
    return sp_num_to_n0l0


def skip_n_lines(in1, n):
    for _ in range(n):
        in1.readline()


def read_sp_nums_and_print_cross(elem, atomic_number, n0l0, in1):
    sp_num = in1.readline().split()[0]
    process = True
    while True:
        line = in1.readline()
        if line is None:
            break
        l = line.split()
        if len(l) == 1:
            sp_num = l[0]
            process = True
        elif line.startswith("Auto"):
            process = False
        elif process and len(l) > 4:
            e = float(l[4])
            e_n0l0 = n0l0[sp_num] - e
            if e_n0l0 < 0:
                error("Less than 0 e_n0l0")
            compute_and_iterate(l[0:2], e_n0l0, atomic_number, int(sp_num))


################## MAIN ######################
if len(sys.argv) < 1:
    error('\nUsage: ' + sys.argv[0] + 'in1.inp')

in1_inp_path = sys.argv[1]

with open(in1_inp_path, "r") as in1_inp:
    el, atomic_number = read_element(in1_inp)
    skip_n_lines(in1_inp, 12)
    n0l0 = read_n0l0(in1_inp)
    read_sp_nums_and_print_cross(el, atomic_number, n0l0, in1_inp)
