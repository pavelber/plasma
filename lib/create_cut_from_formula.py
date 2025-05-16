from os.path import join

from lib.data import In1Level
from lib.iterate_photo_cross import p1, compute_and_iterate
from lib.utils import remove_num_electrones, add_digit


def remove_last_external_electron(config_1, config_2):
    last_digit = config_2[-1]
    if last_digit.isdigit() and not last_digit == '1':
        return config_2[:-1] + str(int(last_digit) - 1)
    return config_1


def remove_last_internal_electron(config_1, config_2):
    return config_2


#################################################################
# USE FORMULA TO COMPUTE CUT
#################################################################


def create_configs_without_one_external_electron_in_next_sp(config_1, config_2, next_sn, atomic_number,
                                                            levels_by_sp_num):
    str_next_sn = str(next_sn)
    if next_sn == atomic_number + 1:  # nucleus
        next_levels = [In1Level(1, None, None, None, None, 1.0, None)]
        alternative_iteration_formula = True
    else:
        next_sp_levels = levels_by_sp_num[str_next_sn]
        config = add_digit(remove_last_external_electron(config_1, config_2))
        alternative_iteration_formula = remove_num_electrones(config_2) not in p1.keys()

        next_levels = list(filter(lambda x:
                                  (x.config_2[-1] == '0' and
                                   add_digit(x.config_1) == config) or
                                  add_digit(x.config_2) == config,
                                  next_sp_levels))
    return alternative_iteration_formula, next_levels


def create_configs_without_one_internal_electron_in_next_sp(config_1, config_2, next_sn, atomic_number,
                                                            levels_by_sp_num):
    str_next_sn = str(next_sn)
    if next_sn == atomic_number + 1:
        next_levels = [In1Level(1, None, None, None, None, 1.0, None)]
        alternative_iteration_formula = True
    else:
        next_sp_levels = levels_by_sp_num[str_next_sn]
        config = add_digit(remove_last_internal_electron(config_1, config_2))
        alternative_iteration_formula = remove_num_electrones(config_2) not in p1.keys()

        next_levels = list(filter(lambda x:
                                  (x.config_2[-1] == '0' and
                                   add_digit(x.config_1) == config) or
                                  add_digit(x.config_2) == config,
                                  next_sp_levels))
    return alternative_iteration_formula, next_levels


def iterate_next_levels(alternative_iteration_formula, atomic_number, config_1, config_2, e_n0l0, level,
                        level_num, next_levels, next_sn, o_f, s_n, sp_dir):
    sum_of_stat_weights = sum(map(lambda x: x.stat_weight, next_levels))
    # print("*** From " + str(s_n) + " " + config_1 + " " + config_2 + " to " + str(next_sn) + " " + str(next_levels))
    if len(next_levels) == 0:
        print("*** From " + str(s_n) + " " + config_1 + " " + config_2 + " to " + str(next_sn) + " <NO LEVELS FOUND>")
    else:
        for lvl in next_levels:
            stat_weight = lvl.stat_weight
            relative_weight = stat_weight / sum_of_stat_weights
            lvl_to = lvl.level_num
            o_f.write("%4s  %4s\n" % (level_num, lvl_to,))
            with open(join(sp_dir, "%s_%s_%s.txt" % (s_n, level_num, lvl_to)), "w") as f_data:
                compute_and_iterate([config_1, config_2], e_n0l0, atomic_number, s_n,
                                    relative_weight,
                                    o_f, f_data,
                                    alternative_iteration_formula)
            o_f.write("--\n")


def iterate_bcfp_levels(bcfp_f, config_1, config_2, level, next_levels, next_sn, s_n, bcfp_cross_section_database, comment):
    sum_of_stat_weights = sum(map(lambda x: x.stat_weight, next_levels))
    if len(next_levels) == 0:
        print("*** From " + str(s_n) + " " + config_1 + " " + config_2 + " to " + str(next_sn) + " <NO LEVELS FOUND>")
    else:
        for lvl in next_levels:
            stat_weight = lvl.stat_weight
            relative_weight = stat_weight / sum_of_stat_weights
            lvl_to = lvl.level_num
            bcfp_f.write(" %4d %4d %4d %4d      %.7f    0    0    0  %s\n" %
                         (s_n, level.level_num, next_sn, lvl_to, relative_weight, comment))


def write_rrec_from_formula(atomic_number, config_1, config_2, level, level_num, levels_by_sp_num, next_sn, o_f,
                            s_n, sp_dir):
    e = level.energy
    e_n0l0 = level.e_n0l0
    (alternative_iteration_formula, next_levels) = \
        create_configs_without_one_external_electron_in_next_sp(config_1, config_2, next_sn,
                                                                atomic_number,
                                                                levels_by_sp_num)
    iterate_next_levels(alternative_iteration_formula, atomic_number, config_1, config_2,
                        e_n0l0, level, level_num, next_levels, next_sn, o_f, s_n, sp_dir)
    (alternative_iteration_formula, next_levels) = \
        create_configs_without_one_internal_electron_in_next_sp(config_1, config_2, next_sn,
                                                                atomic_number,
                                                                levels_by_sp_num)
    iterate_next_levels(alternative_iteration_formula, atomic_number, config_1, config_2,
                        e_n0l0, level, level_num, next_levels, next_sn, o_f, s_n, sp_dir)


def write_bcfp(atomic_number, bcfp_f, config_1, config_2, level, levels_by_sp_num, next_sn,
               s_n, sp_dir, bcfp_cross_section_database):
    (alternative_iteration_formula, next_levels) = \
        create_configs_without_one_external_electron_in_next_sp(config_1, config_2, next_sn,
                                                                atomic_number,
                                                                levels_by_sp_num)
    iterate_bcfp_levels(bcfp_f, config_1, config_2, level, next_levels, next_sn, s_n, bcfp_cross_section_database, "")
    (alternative_iteration_formula, next_levels) = \
        create_configs_without_one_internal_electron_in_next_sp(config_1, config_2, next_sn,
                                                                atomic_number,
                                                                levels_by_sp_num)
    iterate_bcfp_levels(bcfp_f, config_1, config_2, level, next_levels, next_sn, s_n, bcfp_cross_section_database,
                        " # internal electron")
