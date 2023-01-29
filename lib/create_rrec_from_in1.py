import os
from os.path import join, exists

from lib.iterate_photo_cross import compute_and_iterate
from lib.utils import error


def read_element(in1):
    l = in1.readline().split()
    return l[0], int(l[1])


def read_n0l0(in1, sp_nums):
    sp_num_to_n0l0 = {}
    for _ in sp_nums:
        line = in1.readline()
        l = line.split()
        if float(l[4]) == 0.0:
            break
        sp_num_to_n0l0[l[0]] = float(l[4])
    return sp_num_to_n0l0


def last_config_wo_one_electron(config_1, config_2):
    last_digit = config_2[-1]
    if last_digit.isdigit() and not last_digit == '1':
        return config_2[:-1] + str(int(last_digit) - 1)
    return config_1


def skip_n_lines(in1, n):
    for _ in range(n):
        in1.readline()


def read_sp_nums(n0l0, in1):
    sp_num_to_level = {}
    sp_num = in1.readline().split()[0]
    sp_num_to_level[sp_num] = []
    process = True
    while True:
        line = in1.readline()
        if line is None or line == '':
            break
        l = line.split()
        if len(l) == 1:
            sp_num = l[0]
            sp_num_to_level[sp_num] = []
            process = True
        elif line.startswith("Auto"):
            process = False
        elif process and len(l) >7:
            level_num = int(l[7])
            config_1 = l[0]
            config_2 = l[1]
            stat_weight = float(l[3])
            e = float(l[4])
            e_n0l0 = n0l0[sp_num] - e
            if e_n0l0 < 0:
                error("Less than 0 e_n0l0")
            level = (level_num, config_1, config_2, e, e_n0l0, stat_weight)
            sp_num_to_level[sp_num].append(level)
    return sp_num_to_level


def add_one_to_config(c):
    if not c[-1].isdigit():
        return c+"1"
    else:
        return c


def create_rrec_from_in1(in1_inp_path, out_dir, sp_nums):
    with open(in1_inp_path, "r+") as in1_inp:
        el, atomic_number = read_element(in1_inp)
        skip_n_lines(in1_inp, 12)
        n0l0 = read_n0l0(in1_inp, sp_nums)
        levels_by_sp_num = read_sp_nums(n0l0, in1_inp)
    for s_n in sp_nums:
        sp_dir = join(out_dir, str(s_n))
        if not exists(sp_dir):
            os.mkdir(sp_dir)
        with open(join(sp_dir, "rrec"), "w") as o_f:
            print(s_n)
            levels = levels_by_sp_num[str(s_n)]
            next_sn = s_n + 1
            str_next_sn = str(next_sn)
            if next_sn in sp_nums:
                next_sp_levels = levels_by_sp_num[str_next_sn]
                for level in levels:
                    level_num = level[0]
                    config_1 = level[1]
                    config_2 = level[2]
                    e = level[3]
                    e_n0l0 = level[4]

                    if next_sn == atomic_number + 1:
                        next_levels = [(1, None, None, None, None, 1.0)]
                    else:
                        config = last_config_wo_one_electron(config_1, config_2)
                        next_levels = filter(lambda x:
                                             (x[2][-1] == '0' and
                                              add_one_to_config(x[1]) == add_one_to_config(config)) or
                                             add_one_to_config(x[2]) == add_one_to_config(config),
                                             next_sp_levels)
                    sum_of_stat_weights = sum(map(lambda x: x[5], next_levels))
                    for lvl in next_levels:
                        print(str(level[0]) + ", " + str(lvl[0]))
                        stat_weight = level[5]
                        o_f.write("%4s  %4s\n" % (level_num, lvl[0],))
                        #                o_f.write("%4s  %4s\n" % (lvl[0], level_num))
                        compute_and_iterate([config_1, config_2], e_n0l0, atomic_number, s_n,
                                            stat_weight / sum_of_stat_weights,
                                            o_f)
                        o_f.write("--\n")
