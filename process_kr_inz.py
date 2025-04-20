import sys
import os
import traceback

from lib.cross_section import get_constants_for_bernshtam_ralchenko
from lib.in1 import IN1
from lib.utils import skip_lines, skip_n_lines


def process_transition(in1_data, from_sp, from_level, to_sp, to_level, coefficient):
    from_config = in1_data.get_config(from_sp, from_level)
    to_config = in1_data.get_config(to_sp, to_level)
    (c_l, delta_l, num_of_electrons) = get_constants_for_bernshtam_ralchenko(
        from_config,
        to_config)
    if num_of_electrons is None:
        num_of_electrons = 0
    if delta_l is None:
        delta_l = -1.0
    if c_l is None:
        c_l = -1.0
    return [c_l, delta_l, num_of_electrons, coefficient]


def main():
    # Check command line arguments
    inz = sys.argv[1]
    bfcp = sys.argv[2]
    in1 = IN1(sys.argv[3])



    # Create output filename
    base, ext = os.path.splitext(inz)
    output_file = f"{base}_new{ext}"

    # Read second file and create dictionary
    transitions = {}
    with open(bfcp, 'r') as f:
        skip_n_lines(f,2)
        for line in f:
            if line.strip() and not line.startswith('-'):
                parts = line.split()
                if len(parts) >= 5:
                    z, lvl, z1, lvl1 = map(str, parts[:4])
                    coefficient = float(parts[4])
                    key = (z, lvl, z1, lvl1)
                    transitions[key] = coefficient

    # Process first file and write to output
    with open(inz, 'r') as f_in, open(output_file, 'w') as f_out:
        for line in f_in:
            parts = line.split()
            if len(parts) >= 14:  # Ensure it's a data line
                try:
                    iss = parts[0]
                    iqs = parts[1]
                    fss = parts[2]
                    fqs = parts[3]

                    # Check if transition exists in dictionary
                    key = (iss, iqs, fss, fqs)
                    if key in transitions:
                        coefficient = transitions[key]
                        # Call your method to get 5 numbers
                        extra_values = process_transition(
                            in1,
                            iss, iqs, fss, fqs, coefficient
                        )
                    else:
                        extra_values = [-1.0, -1.0, 0, -1.0]

                    # Write original line plus new columns
                    f_out.write(line.strip())
                    f_out.write(f" {extra_values[0]:12.4e}")
                    f_out.write(f" {extra_values[1]:12.4e}")
                    f_out.write(f"  {extra_values[2]:12}")
                    f_out.write(f" {extra_values[3]:12.3f}")
                    f_out.write('\n')

                except ValueError:
                    # Write non-data lines as is
                    traceback.print_exc()
                    f_out.write(line)
            else:
                # Write header lines as is
                f_out.write(line.strip()+"          c_l      delta_l num_of_electrons branching_ratio\n")


if __name__ == "__main__":
    main()