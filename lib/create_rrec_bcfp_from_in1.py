import os
from os.path import join, exists

from lib.create_cut_from_formula import write_rrec_from_formula, write_bfcp
from lib.create_cut_from_strasburg import write_rrec_from_strasburg
from lib.data import In1Level
from lib.exceptions import GenericPlasmaException
from lib.strsbrg_db import read_strsbrg_db


def read_element(in1):
    l = in1.readline().split()
    return l[0], int(l[1])


def read_n0l0(in1, sp_nums):
    sp_num_to_n0l0 = {}
    for _ in sp_nums:
        line = in1.readline()
        l = line.split()
        if len(l) < 5:
            sp_num_to_n0l0[l[0]] = 0.0
        else:
            sp_num_to_n0l0[l[0]] = float(l[4])
    return sp_num_to_n0l0


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
        elif process and len(l) == 8:
            level_num = int(l[7])
            config_1 = l[0]
            config_2 = l[1]
            term = l[2]
            stat_weight = float(l[3])
            e = float(l[4])
            e_n0l0 = n0l0[sp_num] - e
            if e_n0l0 < 0:
                raise GenericPlasmaException("Less than 0 e_n0l0")
            level = In1Level(level_num, config_1, config_2, e, e_n0l0, stat_weight, term)
            sp_num_to_level[sp_num].append(level)
        elif process and len(l) == 7:
            level_num = int(l[6])
            config_1 = ""
            config_2 = l[0]
            term = l[1]
            stat_weight = float(l[2])
            e = float(l[3])
            e_n0l0 = n0l0[sp_num] - e
            if e_n0l0 < 0:
                raise GenericPlasmaException("Less than 0 e_n0l0")
            level = In1Level(level_num, config_1, config_2, e, e_n0l0, stat_weight, term)
            sp_num_to_level[sp_num].append(level)
    return sp_num_to_level


def create_rrec_from_in1(in1_inp_path, elem, out_dir, sp_nums, nucleus, use_formula=True):
    if not use_formula:
        read_strsbrg_db(elem, sp_nums, nucleus)

    with open(in1_inp_path, "r+") as in1_inp:
        el, atomic_number = read_element(in1_inp)
        skip_n_lines(in1_inp, 12)
        n0l0 = read_n0l0(in1_inp, sp_nums)
        levels_by_sp_num = read_sp_nums(n0l0, in1_inp)
    levels_by_sp_num[str(nucleus)] = []
    for s_n in sp_nums:
        sp_dir = join(out_dir, str(s_n))
        if not exists(sp_dir):
            os.mkdir(sp_dir)
        with open(join(sp_dir, "rrec"), "w") as o_f:
            print(s_n)
            levels = levels_by_sp_num[str(s_n)]
            next_sn = s_n + 1

            if next_sn in sp_nums or next_sn == nucleus:
                for level in levels:
                    level_num = level.level_num
                    config_1 = level.config_1
                    config_2 = level.config_2

                    if use_formula:
                        write_rrec_from_formula(atomic_number, config_1, config_2, level, level_num,
                                                levels_by_sp_num, next_sn, o_f, s_n, sp_dir)
                    else:
                        write_rrec_from_strasburg(el, level,
                                                  levels_by_sp_num, next_sn, o_f, s_n, sp_dir, nucleus)


def create_bcfp_from_in1(in1_inp_path, out_dir, sp_nums, nucleus):
    with open(in1_inp_path, "r+") as in1_inp:
        el, atomic_number = read_element(in1_inp)
        skip_n_lines(in1_inp, 12)
        n0l0 = read_n0l0(in1_inp, sp_nums)
        levels_by_sp_num = read_sp_nums(n0l0, in1_inp)
    levels_by_sp_num[str(nucleus)] = []
    with open(join(out_dir, "BFCP.INP"), "w") as bfcp_f:
        bfcp_f.write("    Z  lvl#  Z+1 lvl#      Coefficient	0 0 0\n")
        bfcp_f.write("--------------------------------------\n")
        for s_n in sp_nums:
            sp_dir = join(out_dir, str(s_n))
            if not exists(sp_dir):
                os.mkdir(sp_dir)
            levels = levels_by_sp_num[str(s_n)]
            next_sn = s_n + 1

            if next_sn in sp_nums or next_sn == nucleus:
                for level in levels:
                    level_num = level.level_num
                    config_1 = level.config_1
                    config_2 = level.config_2
                    write_bfcp(atomic_number, bfcp_f, config_1, config_2,
                               level, levels_by_sp_num, next_sn, s_n, sp_dir)
