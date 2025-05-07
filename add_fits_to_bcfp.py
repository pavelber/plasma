import os
from math import log
from os import path

import numpy as np

from lib.cross_section import create_cross_section_function
from lib.in1 import IN1
from lib.utils import skip_n_lines
from lib.fit_parameters import create_fits_from_range  # Import the new function

reject_bad_fits = True

header = """  iSS  iQS  fSS  fQS         D          -A           B           C
-----------------------------------------------------------------------------------
"""

START_E = 1.0
END_E = 100.0
STEP_E = 1.0
METHOD = 'powell'
BAD_FIT_THRESHOLD = 1e-16  # New parameter


def approximation_fun(params, E0, stat_weight, x):
    """
    Original approximation function with 4 parameters.

    Args:
        params (list or array): Parameters [a, b, c, d].
        E0 (float): Ionization potential.
        stat_weight (float): Statistical weight.
        x (float): Energy ratio.

    Returns:
        float: Approximated value.
    """
    a, b, c, d = params
    y = 1 - 1 / x
    E = x * E0
    return 3.8101e-16 * (a * log(x) + b * y * y + c * y / x + d * y / (x * x)) * E / stat_weight


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
    params = None  # Initialize parameters
    in1_data = IN1(in1_path)
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
            transition_energy = in1_data.get_ionization_energy(from_sp, from_level, to_sp, to_level)
            from_config = in1_data.get_config(from_sp, from_level)
            to_config = in1_data.get_config(to_sp, to_level)
            from_stat_weight = in1_data.get_stat_weight(from_sp, from_level)
            energy_function = create_cross_section_function(transition_energy, coef, from_config, to_config)
            if energy_function is None:
                print(from_config, to_config, line, sep='')
            else:
                create_tabulation(energy_function, from_sp, from_level, to_sp, to_level, transition_energy, outdir)

                # Use the new create_fits function
                (result, square_diff) = create_fits_from_range(
                    cross_cut_function=energy_function,
                    approximation_fun=approximation_fun,
                    ionization_potential=in1_data.get_ionization_potential(from_sp),
                    stat_weight=from_stat_weight,
                    initial_params=params,
                    start_e=START_E,
                    end_e=END_E,
                    step_e=STEP_E,
                    method=METHOD,
                    reject_bad_fits=reject_bad_fits,
                    bad_fit_threshold=BAD_FIT_THRESHOLD
                )

                if result is None:
                    print(f"Warning: Skipping fit for {from_sp} {from_level} -> {to_sp} {to_level}")
                    outfile.write(line)  # Write original line as placeholder
                    continue

                params = result  # Update parameters for next iteration
                a, b, c, d = result  # Unpack for output (assuming 4 parameters)
                modified_line = line[:24] + f"{a:10.3e} {b:10.3e} {c:10.3e} {d:10.3e}\n"
                outfile.write(modified_line)


# Main execution
if __name__ == "__main__":
    import sys

    if len(sys.argv) != 4:
        print("Usage: python script.py <bcfp_input_file> <in1_file> <bcfp_output_file>")
        sys.exit(1)

    bcfp_input_path = sys.argv[1]
    in1_path = sys.argv[2]
    bcfp_output_path = sys.argv[3]
    process_file(bcfp_input_path, in1_path, bcfp_output_path)
