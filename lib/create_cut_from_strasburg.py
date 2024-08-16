from os.path import join, dirname, abspath

import numpy as np
from scipy import interpolate

from lib.data import In1Level
from lib.levels_string import letters_to_order
from lib.strsbrg_db import strsbrg_levels, strsbrg_cuts
from lib.utils import energy_ryd_to_ev

base_dir = dirname(abspath(__file__))


def get_in1_levels_by_strasburg_level(elem, s_n, level, in1_levels_by_sp_num):
    return list(
        filter(lambda l: get_strasburg_level_by_in1_level(elem, s_n, l, in1_levels_by_sp_num) == level,
               in1_levels_by_sp_num[str(s_n)]))


def get_strasburg_level_by_in1_level(elem, s_n, in1_level, in1_levels_by_sp_num):
    all_term_from_levels = find_levels_by_config_and_term(in1_levels_by_sp_num[str(s_n)], in1_level)
    # calculate their sum stat weight  #TODO why do we need it?
    sum_of_stat_weights = int(sum(map(lambda l: l.stat_weight, all_term_from_levels)))

    if in1_level.config_1 == '':
        config_key = str([in1_level.config_2])
    elif in1_level.config_2 == '':
        config_key = str[in1_level.config_1]
    else:
        config_key = str([in1_level.config_1, in1_level.config_2])

    levels_per_config = strsbrg_levels[s_n][config_key]

    strasburg_levels = list(filter(lambda l: l.stat_weight == sum_of_stat_weights, levels_per_config))

    if len(strasburg_levels) > 1 and in1_level.term[0].isdigit() and in1_level.term[1] != "/" and in1_level.term[1] != "[":
        islp = in1_level.term[0] + letters_to_order[in1_level.term[1].lower()]
        strasburg_levels = list(filter(lambda l: islp == l.islp[0:2], strasburg_levels))

    if len(strasburg_levels) == 1:
        return strasburg_levels[0]

    # relative diff in energy < 15%
    strasburg_levels = list(
        filter(lambda l: (abs(in1_level.energy - l.energy) / l.energy) < 15.0, strasburg_levels))

    if len(strasburg_levels) > 1:
        return min(strasburg_levels, key=lambda l: abs(in1_level.energy - l.energy))
    else:
        pass
        # print("NO LEVELS %config_key %config_key,%config_key (%config_key)" % (s_n, in1_level.config_1, in1_level.config_2, in1_level.term))


def find_strasburg_level(s_n, energy):
    levels = strsbrg_levels[s_n]
    flat_list_of_levels = [item for sublist in levels.values() for item in sublist]
    ############### HARDCODED Level 1 - ground state ########################
    level = list(filter(lambda x: x.num_i == 1, flat_list_of_levels))[0]
    return level


def get_to_in_1_levels(elem, s_n, level, in1_levels_by_sp_num, nucleus):
    from_strqasburg_level = get_strasburg_level_by_in1_level(elem, s_n, level, in1_levels_by_sp_num)
    if from_strqasburg_level is None:
        return None
    ionization_potentional = strsbrg_cuts[s_n][1].energy
    if from_strqasburg_level.num_i not in strsbrg_cuts[s_n]:
        return []
    transition = strsbrg_cuts[s_n][from_strqasburg_level.num_i]
    s_n_1 = s_n + 1
    if s_n_1 == nucleus:
        return [In1Level(1, "", "", 0.0, 0.0, 1.0, "")]
    if s_n_1 not in strsbrg_levels:
        return []
    to_strsbrg_level = find_strasburg_level(s_n_1,
                                            transition.energy - (
                                                    ionization_potentional + from_strqasburg_level.energy))

    to_in1_levels = get_in1_levels_by_strasburg_level(elem, s_n_1, to_strsbrg_level, in1_levels_by_sp_num)
    return to_in1_levels


# Потенциал ионизации иона это энергия первого перехода в таблице переходов.
# Из энерии перехода вычитаю (потенциал ионизции - энергия уровня из которого переход).
# Получаю энергию уровня в который переход

def create_n_values_map(original_dict, n):
    # Separate the keys and values into two lists
    x = list(original_dict.keys())
    y = list(original_dict.values())

    # Create the interpolation function
    f = interpolate.interp1d(x, y)

    # Create an array of 100 evenly spaced points between the min and max of your original x values
    xnew = np.linspace(min(x), max(x), num=n, endpoint=True)

    # Use the interpolation function to calculate the new y values
    ynew = f(xnew)

    # Combine the new x and y values into a new dictionary
    new_dict = dict(zip(xnew, ynew))
    return new_dict


def take_from_db_and_write(in1_level, e_n0l0, elem, s_n, relative_weight, levels_by_sp_num, o_f, f_data):
    srasburg_level = get_strasburg_level_by_in1_level(elem, s_n, in1_level, levels_by_sp_num)
    from_db = strsbrg_cuts[s_n][srasburg_level.num_i]
    interpolated = create_n_values_map(from_db.cuts, 99)
    cut_constant = 1e-18
    for e_ryd, cut in interpolated.items():
        sigma = cut_constant * float(cut) * relative_weight
        e = energy_ryd_to_ev(float(e_ryd))
        e_relative = e / e_n0l0
        o_f.write(" %.3e   %.3e   %.3e\n" % (e_relative, sigma, e))
        f_data.write(" %.3e,%.3e,%.3e\n" % (e_relative, sigma, e))


def find_levels_by_config_and_term(levels, level):
    return list(
        filter(lambda l: l.config_1 == level.config_1 and l.config_2 == level.config_2 and l.term == level.term,
               levels))


def write_rrec_from_strasburg(elem, from_in1_level,
                              levels_by_sp_num, next_sn, o_f, s_n, sp_dir,nucleus):
    e_n0l0 = from_in1_level.e_n0l0

    # find all in1 "to" levels for the transition (we assume that transition always to ground state (level 1)
    # of the next sp num)
    to_in1_levels = get_to_in_1_levels(elem, s_n, from_in1_level, levels_by_sp_num, nucleus)

    if to_in1_levels is None or len(
            to_in1_levels) == 0:  # we dont found either "from" strasburg levels or "to" in1 levels
        print("*** From " + str(s_n) + " " + from_in1_level.config_1 + " " + from_in1_level.config_2 +
              " to " + str(next_sn) + " " + str(to_in1_levels) + " <NO LEVELS FOUND>")
    else:
        print("*** From " + str(s_n) + " " + from_in1_level.config_1 + " " + from_in1_level.config_2 +
              " to " + str(next_sn) + " " + str(to_in1_levels))
        sum_of_stat_weights_to = sum(map(lambda l: l.stat_weight, to_in1_levels))
        for lvl in to_in1_levels:
            stat_weight = lvl.stat_weight
            relative_weight = stat_weight / sum_of_stat_weights_to
            lvl_to = lvl.level_num
            o_f.write("%4s  %4s\n" % (from_in1_level.level_num, lvl_to,))
            with open(join(sp_dir, "%s_%s_%s.txt" % (s_n, from_in1_level.level_num, lvl_to)), "w") as f_data:
                take_from_db_and_write(from_in1_level, e_n0l0, elem, s_n,
                                       relative_weight, levels_by_sp_num, o_f, f_data)
            o_f.write("--\n")
