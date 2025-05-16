import os
from math import log
from os import path

import numpy as np

from lib.create_cross_section_fits import parse_excitation_spectroscopic_files, TransitionType, fitting_functions
from lib.cross_section import create_cross_section_function
from lib.in1 import IN1
from lib.utils import skip_n_lines
from lib.fit_parameters import create_fits_from_range, select_best_fit  # Import the new function

reject_bad_fits = True
BAD_FIT_THRESHOLD = 1e-16  # New parameter



def modify_line(line, best_id, best_params):
    """
    Modifies a fixed-width formatted line based on new values.

    Args:
        line (str): The original line string.
        best_id (str or int): The new value for the column at index 3.
        best_params (list): A list of new values for columns at indexes 4-9.
                              If fewer than 6 values, the rest will be '0.0'.

    Returns:
        str: The modified line string.
    """
    # Define column widths based on the example
    # Adjust these if your actual data has different fixed widths
    #   1    1   14   16       6.747E-19    1.657E-20    9.392E-21    4.031E-29    1.038E-01    9.149E+00      -1.162E-09
    # 0-4  5-9 10-14 15-19    20-32        33-45        46-58        59-71        72-84        85-97         98-111
    widths = [4, 5, 5, 5, 14, 13, 13, 13, 13, 13, 16] # Last column width adjusted to fit the example

    # Parse the line based on fixed widths
    parsed_parts = []
    current_pos = 0
    for width in widths:
        parsed_parts.append(line[current_pos:current_pos + width])
        current_pos += width

    # Create the new list of parts
    new_parts = list(parsed_parts) # Make a copy to modify

    # Update column with index 3 (method)
    # Ensure the new value is formatted to fit the column width
    new_parts[3] = str(best_id).rjust(widths[3]) # Assuming it should be right-justified like integers

    # Update columns with indexes 4, 5, 6, 7, 8, 9
    for i in range(6):
        col_index = 4 + i
        if i < len(best_params):
            # Format as scientific notation, similar to original, and right-justify
            try:
                value_to_format = float(best_params[i])
                # Format to a general scientific notation that fits, then pad
                # This format aims for something like "X.YYYE+ZZ" or "X.YYYE-ZZ"
                # Adjust precision as needed. Using .3E for 3 decimal places in mantissa.
                formatted_value = "{:> {}.3E}".format(value_to_format, widths[col_index] -1) # -1 for potential leading space
                # Ensure it doesn't exceed width, if so, try less precision or different format
                if len(formatted_value) > widths[col_index]: # Fallback if too long
                    formatted_value = "{:> {}.2E}".format(value_to_format, widths[col_index] -1)
                if len(formatted_value) > widths[col_index]:
                     formatted_value = str(best_params[i])[:widths[col_index]].rjust(widths[col_index])


            except ValueError: # If cannot convert to float, use string representation
                formatted_value = str(best_params[i]).rjust(widths[col_index])
        else:
            # Fill with '0.0' formatted to fit
            # Format 0.0 in a way that matches the others, e.g., "0.000E+00"
            # Considering the width, a simple "0.0" might be too short.
            # Let's aim for a similar scientific notation representation if possible.
            formatted_value = "{:> {}.3E}".format(0.0, widths[col_index]-1)
            if len(formatted_value) > widths[col_index]:
                 formatted_value = "0.0".rjust(widths[col_index])


        new_parts[col_index] = formatted_value.rjust(widths[col_index])


    # All other columns (0, 1, 2, 10) remain the same as parsed
    # The fixed-width parsing already preserves their original format and spacing

    return "".join(new_parts)


def replace_existing_cuts_in_excit(excit_input_path, in1_path, excit_output_path, cross_sections):
    outdir = path.join(path.dirname(excit_output_path), 'tabulation')
    if not path.exists(outdir):
        os.mkdir(outdir)
    params = None  # Initialize parameters
    in1_data = IN1(in1_path)
    with open(excit_input_path, 'r') as infile, open(excit_output_path, 'w') as outfile:
        outfile.write(infile.readline())
        outfile.write(infile.readline())
        for line in infile:
            parts = line.split()
            sp_num = parts[0]
            from_level = parts[1]
            to_level = parts[2]

            if (sp_num, from_level, to_level) in cross_sections.keys():
                #print("FOUND!!!!")
                # Use the new create_fits function
                (best_id, best_params) = select_best_fit(
                    table=cross_sections[(sp_num, from_level, to_level)],
                    function_objects= fitting_functions[TransitionType.EXCITATION],
                    ionization_potential=in1_data.get_ionization_potential(sp_num),
                    stat_weight= 0, #not used
                    reject_bad_fits=reject_bad_fits,
                    bad_fit_threshold=BAD_FIT_THRESHOLD
                )


                modified_line = modify_line(line, best_id, best_params)
                outfile.write(modified_line+" #changed\n")
            else:
                outfile.write(line)


# Main execution
if __name__ == "__main__":
    import sys

    # if len(sys.argv) != 4:
    #     print("Usage: python script.py <excit_input_file> <in1_file> <excit_output_file> <cross_section_directory>")
    #     sys.exit(1)

    #excit_input_path = sys.argv[1]
    #in1_path = sys.argv[2]
    #excit_output_path = sys.argv[3]
    #cross_section_directory = sys.argv[4]
    cross_section_directory = "C:\\work2\\db\\O\\excitation-crosssection\\"
    cross_sections = parse_excitation_spectroscopic_files(cross_section_directory)
    #process_file(excit_input_path, in1_path, excit_output_path, cross_sections)
    replace_existing_cuts_in_excit("C:\\work4\\tmp\\O-fac\\EXCIT.INP", "C:\\work4\\tmp\\O-fac\\IN1.INP", "C:\\work4\\tmp\\O-fac\\EXCIT.OUT", cross_sections)
