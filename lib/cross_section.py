from math import log
from math import sqrt, exp

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
    l = from_config[lost_electron_config_index][1]  # Use the configuration part, which lose electrons
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


def nomad_5(params, E0, stat_weight, x):
    a, b, c, d, e = params
    return a / x + b / x ** 2 + c / x ** 3 + d / x ** 4 + (e * log(x)) / x ** 5


def nomad_11(params, E0, stat_weight, x):
    a, b, c, d, e = params
    return (a * x ** 2 + b * x + c) / (((d + x) ** 4) * x ** e)


def nomad_16(params, E0, stat_weight, x) -> float:
    a, b, c, d, e, f = params
    # Check for valid f to avoid division by zero or negative sqrt
    if f <= 1.0:
        raise ValueError("Parameter f must be greater than 1")
    if e == 0:
        raise ValueError("Parameter e cannot be zero")
    if (x - 1.0) < 0 or (x + f) <= 0:
        raise ValueError("Invalid x: leads to negative or zero denominator in sqrt")
    # Calculate alpha: 0.9899495 * sqrt(f / (f - 1.0))
    alpha = 0.9899495 * sqrt(f / (f - 1.0))

    # Calculate XN: sqrt((x - 1.0) / (x + f)) * alpha
    if (x - 1.0) < 0 or (x + f) <= 0:
        raise ValueError("Invalid x: leads to negative or zero denominator in sqrt")
    xn = sqrt((x - 1.0) / (x + f)) * alpha

    # Calculate E1: 1.0 / e
    if e == 0:
        raise ValueError("Parameter e cannot be zero")
    e1 = 1.0 / e

    # Calculate sigma: sum of Gaussian terms divided by x
    sigma = (
                    a * exp(-xn * xn * e1) +
                    b * exp(-(xn - 0.333) ** 2 * e1) +
                    c * exp(-(xn - 0.666) ** 2 * e1) +
                    d * exp(-(xn - 1.000) ** 2 * e1)
            ) / x

    return sigma
