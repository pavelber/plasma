from os.path import join, dirname, abspath

from lib.levels_string import letters_to_order
from lib.strsbrg_db import strsbrg_levels, strsbrg_cuts
from lib.utils import error, energy_ryd_to_ev

base_dir = dirname(abspath(__file__))


def get_in1_levels_by_strasburg_level(elem, s_n, level, in1_levels_by_sp_num):
    return list(filter(lambda l: get_strasburg_level_by_in1_level(elem, s_n, l, level.stat_weight) == level,
                       in1_levels_by_sp_num))


def get_strasburg_level_by_in1_level(elem, s_n, in1_level, sum_of_stat_weights):
    levels_per_config = strsbrg_levels[s_n][str([in1_level.config_1, in1_level.config_2])]

    strasburg_levels = list(filter(lambda l: l.stat_weight == sum_of_stat_weights, levels_per_config))

    if len(strasburg_levels) > 1 and in1_level.term[0].isdigit():
        islp = in1_level.term[0] + letters_to_order[in1_level.term[1].lower()]
        strasburg_levels = list(filter(lambda l: islp == l.islp[0:2], strasburg_levels))

    if len(strasburg_levels) == 1:
        return strasburg_levels[0]

    # relative diff in energy < 15%
    strasburg_levels = list(filter(lambda l: (abs(in1_level.energy - l.energy) / l.energy) < 15.0, strasburg_levels))

    if len(strasburg_levels) > 1:
        return min(strasburg_levels, key=lambda l: abs(in1_level.energy - l.energy))
    else:
        print("NO LEVELS %s %s,%s (%s)" % (s_n, in1_level.config_1, in1_level.config_2, in1_level.term))


def find_strasburg_level(s_n, energy):
    levels = strsbrg_levels[s_n]
    flat_list_of_levels = [item for sublist in levels.values() for item in sublist]
    ############### HARDCODED Level 1 - ground state ########################
    level = list(filter(lambda x: x.num_i == 1, flat_list_of_levels))[0]
    return level


def get_to_in_1_levels(elem, s_n, level, sum_of_stat_weights, in1_levels_by_sp_num):
    level_in_strasburg = get_strasburg_level_by_in1_level(elem, s_n, level, sum_of_stat_weights)
    if level_in_strasburg is None:
        return None
    ionization_potentional = strsbrg_cuts[s_n][1].energy
    transition = strsbrg_cuts[s_n][level_in_strasburg.num_i]
    to_strsbrg_level = find_strasburg_level(s_n + 1,
                                            transition.energy - (ionization_potentional + level_in_strasburg.energy))

    return get_in1_levels_by_strasburg_level(elem, s_n, to_strsbrg_level, in1_levels_by_sp_num)


# Потенциал ионизации иона это энергия первого перехода в таблице переходов.
# Из энерии перехода вычитаю (потенциал ионизции - энергия уровня из которого переход).
# Получаю энергию уровня в который переход


def take_from_db_and_write(level, e_n0l0, elem, s_n, relative_weight, o_f, f_data):
    from_db = strsbrg_cuts[s_n][level.config_1]
    for record in from_db:
        e_ryd = record[0]
        sigma = float(record[1]) * relative_weight
        e = energy_ryd_to_ev(float(e_ryd))
        e_relative = e / e_n0l0
        o_f.write(" %.3e   %.3e   %.3e\n" % (e_relative, sigma, e))
        f_data.write(" %.3e,%.3e,%.3e\n" % (e_relative, sigma, e))


def find_levels_by_config_and_term(levels, level):
    return list(filter(lambda l: l.config_1 == level.config_1 and l.config_2 == level.config_2 and l.term == level.term,
                       levels))


def write_rrec_from_strasburg(elem, bfcp_f, level,
                              levels_by_sp_num, next_sn, o_f, s_n, sp_dir):
    e_n0l0 = level.e_n0l0
    # find in1 levels with the same config, different terms
    all_term_from_levels = find_levels_by_config_and_term(levels_by_sp_num[str(s_n)], level)
    # calculate their sum stat weight
    sum_of_stat_weights_levels_from = int(sum(map(lambda l: l.stat_weight, all_term_from_levels)))
    if sum_of_stat_weights_levels_from == 0:
        error("0")

    # find all in1 "to" levels for the transition (we assume that transition always to ground state (level 1) of the next sp num)
    to_in1_levels = get_to_in_1_levels(elem, s_n, level, sum_of_stat_weights_levels_from, levels_by_sp_num)

    # calculate sum of stat weights of "to" levels #TODO why do we need it?
    sum_of_stat_weights = sum(map(lambda l: l.stat_weight, to_in1_levels))
    if len(to_in1_levels) == 0:  # we dont found or "from" strasburg levels or "to" in1 levels
        print("*** From " + str(s_n) + " " + level.config_1 + " " + level.config_2 + " to " + str(next_sn) + " " + str(
            to_in1_levels) + " <NO LEVELS FOUND>")
    else:
        for lvl in to_in1_levels:
            stat_weight = lvl.stat_weight
            relative_weight = stat_weight / sum_of_stat_weights
            lvl_to = lvl.num_i
        o_f.write("%4s  %4s\n" % (level.level_num, lvl_to,))
        bfcp_f.write(" %4d %4d %4d %4d      %.7f    0    0    0 \n" %
                     (s_n, level.level_num, next_sn, lvl_to, relative_weight))
        with open(join(sp_dir, "%s_%s_%s.txt" % (s_n, level.level_num, lvl_to)), "w") as f_data:
            take_from_db_and_write(level, e_n0l0, elem, s_n,
                                   relative_weight, o_f, f_data)
        o_f.write("--\n")
