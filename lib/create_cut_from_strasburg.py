from os.path import join, dirname, abspath

from lib.strsbrg_db import parse_levels

base_dir = dirname(abspath(__file__))


def get_level_num(elem, s_n, config_1, config_2, term):
    f_name = join(base_dir, "..", "db", elem, "strasbg-levels", "%s.txt" % s_n)
    levels = parse_levels(f_name)


def get_next_levels_from_db(elem, s_n, config_1, config_2, term):
    num_of_level_in_strasburg = get_level_num(elem, s_n, config_1, config_2, term)
    return []


def read_db(elem, s_n, config, term):
    return []


def take_from_db_and_write(config, e_n0l0, elem, s_n, relative_weight, o_f, f_data, term):
    from_db = read_db(elem, s_n, config, term)
    for record in from_db:
        e_ryd = record[0]
        sigma = float(record[1]) * relative_weight
        e = float(e_ryd) * 13.605693009
        e_relative = e / e_n0l0
        o_f.write(" %.3e   %.3e   %.3e\n" % (e_relative, sigma, e))
        f_data.write(" %.3e,%.3e,%.3e\n" % (e_relative, sigma, e))


def write_rrec_from_strasburg(elem, bfcp_f, config_1, config_2, term, level, level_num,
                              levels_by_sp_num, next_sn, o_f, s_n, sp_dir):
    e_n0l0 = level[4]
    next_levels = get_next_levels_from_db(elem, s_n, config_1, config_2, term)
    sum_of_stat_weights = sum(map(lambda x: x[5], next_levels))
    print("*** From " + str(s_n) + " " + config_1 + " " + config_2 + " to " + str(next_sn) + " " + str(next_levels))
    if len(next_levels) == 0:
        print(" <NO LEVELS FOUND>")
    else:
        for lvl in next_levels:
            stat_weight = lvl[5]
            relative_weight = stat_weight / sum_of_stat_weights
            lvl_to = lvl[0]
            o_f.write("%4s  %4s\n" % (level_num, lvl_to,))
            bfcp_f.write(" %4d %4d %4d %4d      %.7f    0    0    0 \n" %
                         (s_n, level[0], next_sn, lvl_to, relative_weight))
            with open(join(sp_dir, "%s_%s_%s.txt" % (s_n, level_num, lvl_to)), "w") as f_data:
                take_from_db_and_write([config_1, config_2], e_n0l0, elem, s_n,
                                       relative_weight, o_f, f_data, term)
            o_f.write("--\n")
