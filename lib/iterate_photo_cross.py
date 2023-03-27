import math
from math import sqrt

pi_a0_2 = 8.79e-17
ry = 13.6057

letter_to_num = {'s': 0, 'p': 1, 'd': 2, 'f': 3, 'g': 4, 'n': 5}


def get_n0(c):
    return int(c[0])


def get_l0(c):
    return letter_to_num[c[1]]


def get_num_of_electrons_last_level(c):
    last_letter = c[-1]
    if last_letter in letter_to_num:
        return 1
    else:
        return int(last_letter)  # TODO: assumption - single digit!!!!!


def iterate(e_n0l0, z_tilda, z_eff, last_level_without_num, m, l0, stat_weights_part, out_file,
            alternate_sigma_computation):
    e = 2 * e_n0l0
    step = 2 * e_n0l0
    n = 0
    while n < 50:
        e_relative = e / e_n0l0
        u = (e - e_n0l0) / (z_tilda * z_tilda * ry)
        if alternate_sigma_computation:
            sigma = 2.2e-20 * pow(e_relative, -2.9)
        else:
            sigma = (pi_a0_2 / (z_eff * z_eff)) * \
                    (2.0 * m / (2.0 * l0 + 1.0)) * p1[last_level_without_num] * \
                    ((u + p2[last_level_without_num]) / (u + p3[last_level_without_num])) * \
                    (1.0 / math.pow(u + p4[last_level_without_num], 7.0 / 2.0 + l0)) * stat_weights_part

        out_file.write(" %.3e   %.3e   %.3e\n" % (e_relative, sigma, e))
        e = e + step
        n = n + 1


def compute_and_iterate(config, e_n0l0, z_n, z, stat_weights_part, out_file, alternate_sigma_computation=False):
    last_level = config[-1]
    last_level_without_num = last_level[0:2]
    n0 = get_n0(last_level)
    l0 = get_l0(last_level)
    num_of_electrons_last_level = get_num_of_electrons_last_level(last_level)
    m = num_of_electrons_last_level
    # n+bound - number of bounded electrons- compute from z
    n_bound = z_n - z - 1
    n_nl_greater_n0l0 = num_of_electrons_last_level
    z_eff = n0 * sqrt(e_n0l0 / ry)
    single_electron = n_nl_greater_n0l0 == 1
    if single_electron:
        z_tilda = z_eff + (z_eff - z)
    else:
        z_tilda = z_n - n_bound + n_nl_greater_n0l0
    iterate(e_n0l0, z_tilda, z_eff, last_level_without_num, m, l0, stat_weights_part, out_file,
            alternate_sigma_computation)


p1 = {
    '1s': 4.667e-1,
    '2s': 5.711e-2,
    '2p': 8.261e-2,
    '3s': 1.682e-2,
    '3p': 2.751e-2,
    '3d': 3.788e-3,
    '4s': 7.096e-3,
    '4p': 1.493e-2,
    '4d': 1.769e-3,
    '4f': 1.092e-4,
    '5s': 3.956e-3,
    '5p': 5.846e-2,  # duplication
    '5d': 5.846e-2,  # duplication
    '5f': 5.846e-2,  # duplication
    '5g': 5.846e-2,  # duplication
}

p2 = {
    '1s': 2.724e1,
    '2s': 6.861e-1,
    '2p': 1.843e-1,
    '3s': 1.436e-1,
    '3p': 1.742e-1,
    '3d': 1.566e-1,
    '4s': 8.799e-2,
    '4p': 1.197e-1,
    '4d': 1.205e-1,
    '4f': 1.055e-1,
    '5s': 5.846e-2,
    '5p': 5.846e-2,  # duplication
    '5d': 5.846e-2,  # duplication
    '5f': 5.846e-2,  # duplication
    '5g': 5.846e-2,  # duplication
}

p3 = {
    '1s': 9.458e1,
    '2s': 7.768e1,
    '2p': 7.340,
    '3s': 7.356,
    '3p': 7.162,
    '3d': 7.880,
    '4s': 7.308,
    '4p': 1.027e1,
    '4d': 6.346,
    '4f': 9.231,
    '5s': 8.651,
    '5p': 8.651,  # duplication
    '5d': 8.651,  # duplication
    '5f': 8.651,  # duplication
    '5g': 8.651,  # duplication
}

p4 = {
    '1s': 1.189e1,
    '2s': 3.644e-1,
    '2p': 2.580e-1,
    '3s': 1.436e-1,
    '3p': 1.742e-1,
    '3d': 1.566e-1,
    '4s': 8.799e-2,
    '4p': 1.197e-1,
    '4d': 1.205e-1,
    '4f': 1.055e-1,
    '5s': 5.846e-2,
    '5p': 5.846e-2,  # duplication
    '5d': 5.846e-2,  # duplication
    '5f': 5.846e-2,  # duplication
    '5g': 5.846e-2,  # duplication
}

supported_configs = p1.keys()

#
# config = ['2s2', '2p6']
#
# e_n0l0 = 21.6  # per spectr number- 5th column in in1.inp header table - level energy (>0)
# z_n = 10  # Ne
# z = 1  # spectr number
#
# compute_and_iterate(config, e_n0l0, z_n, z)
