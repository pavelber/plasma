import os
from math import log
from os import path

import numpy as np
from scipy.optimize import minimize

from lib.configurations import get_number_of_electrons
from lib.consts import ry
from lib.cross_section import create_energy_function
from lib.in1 import create_tables, get_ionization_energy
from lib.utils import skip_n_lines

reject_bad_fits = True

header = """  iSS  iQS  fSS  fQS         D          -A           B           C
-----------------------------------------------------------------------------------
"""

START_E = 1.0
END_E = 100.0
STEP_E = 1.0
METHOD = 'powell'


def approximation_fun(a, b, c, d, E0, stat_weight, x):
    y = 1 - 1 / x
    E = x * E0
    return 3.8101e-16 * (a * log(x) + b * y * y + c * y / x + d * y / (x * x)) * E / stat_weight



def is_bad(sum_square_diffs):
    return sum_square_diffs > 1E-16


def create_fits(energy_function, ionization_potential, statweight, newa, newb, newc, newd):
    def def_fun_to_minimize(x):
        a = x[0]
        b = x[1]
        c = x[2]
        d = x[3]

        e = START_E
        s = 0
        while e <= END_E:
            v = approximation_fun(a, b, c, d, ionization_potential, statweight, float(e))
            expected_v = energy_function(e)
            s += (v - expected_v) * (v - expected_v)
            e += STEP_E
        return s

    if newa is not None:
        x0 = np.array([newa, newb, newc, newd])
    else:
        zero_v = energy_function(2.0)
        x0 = np.array([zero_v, zero_v, zero_v, zero_v])
    res = minimize(def_fun_to_minimize, x0, method=METHOD)

    if res.success:
        newa = res.x[0]
        newb = res.x[1]
        newc = res.x[2]
        newd = res.x[3]
        sum_square_diffs = def_fun_to_minimize(res.x)
        if reject_bad_fits:
            if is_bad(sum_square_diffs):
                print("+", end="")

        #        print(sum_square_diffs)
        print(".", end="")
    else:
        print("X", end="")

    return newa, newb, newc, newd


def create_tabulation(energy_function, from_sp, from_level, to_sp, to_level, transition_energy, outdir):
    filename = path.join(outdir, f"{from_sp}_{from_level}_{to_sp}_{to_level}.txt")
    with open(filename, 'w') as f:
        f.write(f"# {from_sp} {from_level} {to_sp} {to_level} {transition_energy}\n")
        f.write(f"# e/I_zqq'        sigma\n")
        e = START_E
        while e <= END_E:
            f.write(f"{e:8.3} {energy_function(e):10.3}\n")
            e += STEP_E


def process_file(bcfp_input_path, in1_path, bcfp_output_path):
    outdir = path.join(path.dirname(bcfp_output_path), 'tabulation')
    if not path.exists(outdir):
        os.mkdir(outdir)
    a = b = c = d = None
    transitions_energy_table, ionization_potential, configurations_table, stat_weight = create_tables(in1_path)
    with open(bcfp_input_path, 'r') as infile, open(bcfp_output_path, 'w') as outfile:
        outfile.write(header)
        skip_n_lines(infile, 2)
        for line in infile:
            parts = line.split()
            coef = float(parts[4])
            from_sp = parts[0]
            from_level = parts[1]
            to_sp = parts[2]
            to_level = parts[3]
            transition_energy = get_ionization_energy(from_sp, from_level, to_sp, to_level, transitions_energy_table,
                                                      ionization_potential)
            from_config = configurations_table[(from_sp, from_level)]
            to_config = configurations_table[(to_sp, to_level)]
            from_stat_weight = stat_weight[(from_sp, from_level)]
            energy_function = create_energy_function(transition_energy, coef,
                                                     from_config,
                                                     to_config)
            if energy_function is None:
                print(from_config, to_config, line, sep='')
            else:
                create_tabulation(energy_function, from_sp, from_level, to_sp, to_level, transition_energy, outdir)

                (a, b, c, d) = create_fits(energy_function, ionization_potential[from_sp], from_stat_weight, a, b, c, d)
                modified_line = line[:24] + f"{a:10.3e} {b:10.3e} {c:10.3e} {d:10.3e}\n"
                outfile.write(modified_line)


# Main execution
if __name__ == "__main__":
    import sys

    if len(sys.argv) != 4:
        print("Usage: python script.py <bcfp_input_file> <in1_file>  <bcfp_output_file>")
        sys.exit(1)

    bcfp_input_path = sys.argv[1]
    in1_path = sys.argv[2]
    bcfp_output_path = sys.argv[3]
    process_file(bcfp_input_path, in1_path, bcfp_output_path)
