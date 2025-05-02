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


def get_constants_for_bernshtam_ralchenko(from_config, to_config):
    (num_of_electrons, lost_electron_config_index) = get_number_of_electrons(from_config, to_config)
    if num_of_electrons is None:
        return None, None, None
    l = from_config[lost_electron_config_index][1] # Use the configuration part, which lose electrons
    if l not in cl_keys:
        return None, None, num_of_electrons
    c_l = get_cl(l)
    delta_l = get_lambda_l(l)
    return c_l, delta_l, num_of_electrons

def get_lotz_om2(e, num_of_electrons, ionization_energy):
    if e < 1:  # Ensure E >= Ei
        return 0.0
    a = 4.5e-14  # cm² eV²
    return (a * num_of_electrons * log(e) / (e * ionization_energy * ionization_energy))



def get_bp_om(e, c_l, delta_l, num_of_electrons, branching_ration, ionization_energy):
    return c_l * pow(ry / ionization_energy, 2 - delta_l) * num_of_electrons * branching_ration * log(e) / e

def create_cross_section_function(ionization_energy, branching_ration, from_config, to_config):
    (c_l, delta_l, num_of_electrons) = get_constants_for_bernshtam_ralchenko(
                                                                                                from_config,
                                                                                                to_config)

    if num_of_electrons is None:
        return None
    if delta_l is None:
        return lambda x: get_lotz_om2(x, num_of_electrons, ionization_energy)
    bernshtam_ralchenko = lambda x: get_bp_om(x, c_l, delta_l, num_of_electrons, branching_ration, ionization_energy)
    return bernshtam_ralchenko
