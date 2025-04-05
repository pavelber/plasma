from math import log

from lib.configurations import get_number_of_electrons
from lib.consts import ry

cl_table = {'s': 1.7796E-16, 'p': 2.1597E-16, 'd': 1.2131E-16}
cl_keys = ['s', 'p', 'd']
lambda_l_table = {'s': 0.0471, 'p': 0.0910, 'd': 0.3319}


def get_cl(l):
    return cl_table[l]


def get_lambda_l(l):
    return lambda_l_table[l]


def get_constants_for_bernshtam_ralchenko(ionization_energy, coef, from_config, to_config):
    num_of_electrons = get_number_of_electrons(from_config, to_config)
    if num_of_electrons is None:
        return None, None, None, ionization_energy, coef
    l = from_config[-1][1]
    if l not in cl_keys:
        return None, None, num_of_electrons, ionization_energy, coef
    c_l = get_cl(l)
    delta_l = get_lambda_l(l)
    return c_l, delta_l, num_of_electrons, ionization_energy, coef


def create_energy_function(ionization_energy, coef, from_config, to_config):
    (c_l, delta_l, num_of_electrons, ionization_energy, coef) = get_constants_for_bernshtam_ralchenko(ionization_energy,
                                                                                                      coef, from_config,
                                                                                                      to_config)

    if num_of_electrons is None:
        return None
    if delta_l is None:
        return lambda x: 4.5 * 10E-14 * num_of_electrons * log(x) / x
    bernshtam_ralchenko = lambda x: c_l * pow(ry / ionization_energy, 2 - delta_l) * num_of_electrons * coef * log(
        x) / x
    return bernshtam_ralchenko
