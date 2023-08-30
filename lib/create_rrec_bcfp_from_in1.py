import os
from os.path import join, exists

from lib.iterate_photo_cross import compute_and_iterate, supported_configs, p1
from lib.levels_string import levels_order, level_to_electrons
from lib.utils import error


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


def remove_last_external_electron(config_1, config_2):
    last_digit = config_2[-1]
    if last_digit.isdigit() and not last_digit == '1':
        return config_2[:-1] + str(int(last_digit) - 1)
    return config_1


def remove_last_internal_electron(config_1, config_2):
    return config_2


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
            stat_weight = float(l[3])
            e = float(l[4])
            e_n0l0 = n0l0[sp_num] - e
            if e_n0l0 < 0:
                error("Less than 0 e_n0l0")
            level = (level_num, config_1, config_2, e, e_n0l0, stat_weight)
            sp_num_to_level[sp_num].append(level)
        elif process and len(l) == 7:
            level_num = int(l[6])
            config_1 = ""
            config_2 = l[0]
            stat_weight = float(l[2])
            e = float(l[3])
            e_n0l0 = n0l0[sp_num] - e
            if e_n0l0 < 0:
                error("Less than 0 e_n0l0")
            level = (level_num, config_1, config_2, e, e_n0l0, stat_weight)
            sp_num_to_level[sp_num].append(level)
    return sp_num_to_level


def add_digit(c):
    if not c[-1].isdigit():
        return c + "1"
    else:
        return c


def remove_num_electrones(c):
    if c[-1].isdigit():
        return c[0:-1]
    else:
        return c


def create_configs_without_one_internal_electron_in_next_sp(config_1, config_2, next_sn, atomic_number,
                                                            levels_by_sp_num):
    str_next_sn = str(next_sn)
    if next_sn == atomic_number + 1:
        next_levels = [(1, None, None, None, None, 1.0)]
        alternative_iteration_formula = True
    else:
        next_sp_levels = levels_by_sp_num[str_next_sn]
        config = add_digit(remove_last_internal_electron(config_1, config_2))
        alternative_iteration_formula = remove_num_electrones(config_2) not in p1.keys()

        next_levels = list(filter(lambda x:
                                  (x[2][-1] == '0' and
                                   add_digit(x[1]) == config) or
                                  add_digit(x[2]) == config,
                                  next_sp_levels))
    return alternative_iteration_formula, next_levels


def create_configs_without_one_external_electron_in_next_sp(config_1, config_2, next_sn, atomic_number,
                                                            levels_by_sp_num):
    str_next_sn = str(next_sn)
    if next_sn == atomic_number + 1:
        next_levels = [(1, None, None, None, None, 1.0)]
        alternative_iteration_formula = True
    else:
        next_sp_levels = levels_by_sp_num[str_next_sn]
        config = add_digit(remove_last_external_electron(config_1, config_2))
        alternative_iteration_formula = remove_num_electrones(config_2) not in p1.keys()

        next_levels = list(filter(lambda x:
                                  (x[2][-1] == '0' and
                                   add_digit(x[1]) == config) or
                                  add_digit(x[2]) == config,
                                  next_sp_levels))
    return alternative_iteration_formula, next_levels


def create_rrec_bcfp_from_in1(in1_inp_path, out_dir, sp_nums):
    with open(in1_inp_path, "r+") as in1_inp:
        el, atomic_number = read_element(in1_inp)
        skip_n_lines(in1_inp, 12)
        n0l0 = read_n0l0(in1_inp, sp_nums)
        levels_by_sp_num = read_sp_nums(n0l0, in1_inp)
    levels_by_sp_num["7"] = []  # TODO C
    with open(join(out_dir, "BFCP.INP"), "w") as bfcp_f:
        bfcp_f.write("    Z  lvl#  Z+1 lvl#      Coefficient	0 0 0\n")
        bfcp_f.write("--------------------------------------\n")
        for s_n in sp_nums:
            sp_dir = join(out_dir, str(s_n))
            if not exists(sp_dir):
                os.mkdir(sp_dir)
            with open(join(sp_dir, "rrec"), "w") as o_f:
                print(s_n)
                levels = levels_by_sp_num[str(s_n)]
                next_sn = s_n + 1

                if next_sn in sp_nums or next_sn == 7:  # TODO!!! C
                    for level in levels:
                        level_num = level[0]
                        config_1 = level[1]
                        config_2 = level[2]

                        e = level[3]
                        e_n0l0 = level[4]
                        (alternative_iteration_formula, next_levels) = \
                            create_configs_without_one_external_electron_in_next_sp(config_1, config_2, next_sn,
                                                                                    atomic_number,
                                                                                    levels_by_sp_num)
                        iterate_next_levels(alternative_iteration_formula, atomic_number, bfcp_f, config_1, config_2,
                                            e_n0l0, level, level_num, next_levels, next_sn, o_f, s_n, sp_dir, "")

                        (alternative_iteration_formula, next_levels) = \
                            create_configs_without_one_internal_electron_in_next_sp(config_1, config_2, next_sn,
                                                                                    atomic_number,
                                                                                    levels_by_sp_num)
                        iterate_next_levels(alternative_iteration_formula, atomic_number, bfcp_f, config_1, config_2,
                                            e_n0l0, level, level_num, next_levels, next_sn, o_f, s_n, sp_dir, " # internal electron")


def iterate_next_levels(alternative_iteration_formula, atomic_number, bfcp_f, config_1, config_2, e_n0l0, level,
                        level_num, next_levels, next_sn, o_f, s_n, sp_dir, comment):
    sum_of_stat_weights = sum(map(lambda x: x[5], next_levels))
    print("*** From " + str(s_n) + " " + config_1 + " " + config_2 + " to " + str(
        next_sn) + " " + str(next_levels))
    if len(next_levels) == 0:
        print(" <NO LEVELS FOUND>")
    else:
        for lvl in next_levels:
            stat_weight = lvl[5]
            relative_weight = stat_weight / sum_of_stat_weights
            # print("From " + str(s_n) + " level " + str(level[0]) + " to " + str(
            #    next_sn) + " level " + str(
            #    lvl[0]) + " with weight " + str(relative_weight))

            lvl_to = lvl[0]
            o_f.write("%4s  %4s\n" % (level_num, lvl_to,))
            bfcp_f.write(" %4d %4d %4d %4d      %.7f    0    0    0  %s\n" %
                         (s_n, level[0], next_sn, lvl_to, relative_weight, comment))
            with open(join(sp_dir, "%s_%s_%s.txt" % (s_n, level_num, lvl_to)), "w") as f_data:
                compute_and_iterate([config_1, config_2], e_n0l0, atomic_number, s_n,
                                    relative_weight,
                                    o_f, f_data,
                                    alternative_iteration_formula)
            o_f.write("--\n")


def create_rrec_for_bad_lines(in1_inp_path, out_dir, sp_nums, s_n, bad_lines):
    level_2_bad = {}
    for bad in bad_lines:
        from_level = bad[1]
        to_level = bad[2]
        if from_level not in level_2_bad:
            levels = []
            level_2_bad[from_level] = levels
        else:
            levels = level_2_bad[from_level]
        levels.append(to_level)

    with open(in1_inp_path, "r+") as in1_inp:
        el, atomic_number = read_element(in1_inp)
        skip_n_lines(in1_inp, 12)
        n0l0 = read_n0l0(in1_inp, sp_nums)
        levels_by_sp_num = read_sp_nums(n0l0, in1_inp)

    sp_dir = join(out_dir, str(s_n))
    bad_dir = join(sp_dir, "bad_lines")
    if not exists(bad_dir):
        os.mkdir(bad_dir)

    with open(join(bad_dir, "rrec"), "w") as o_f:
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
                e_n0l0 = level[4]

                if remove_num_electrones(config_2) not in supported_configs:
                    continue

                if next_sn == atomic_number + 1:
                    next_levels = [(1, None, None, None, None, 1.0)]
                else:
                    config = add_digit(remove_last_external_electron(config_1, config_2))
                    if remove_num_electrones(config) not in supported_configs:
                        continue

                    next_levels = filter(lambda x:
                                         (x[2][-1] == '0' and
                                          add_digit(x[1]) == config) or
                                         add_digit(x[2]) == config,
                                         next_sp_levels)
                sum_of_stat_weights = sum(map(lambda x: x[5], next_levels))
                for lvl in next_levels:
                    if str(level_num) in level_2_bad and str(lvl[0]) in level_2_bad[str(level_num)]:
                        print("Recreating rrec for bad trans: " + str(level_num) + "->" + str(lvl[0]))
                        stat_weight = level[5]
                        relative_weight = stat_weight / sum_of_stat_weights
                        o_f.write("%4s  %4s\n" % (level_num, lvl[0],))
                        compute_and_iterate([config_1, config_2], e_n0l0, atomic_number, s_n,
                                            relative_weight,
                                            o_f, True)
                        o_f.write("--\n")
