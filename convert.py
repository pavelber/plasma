#!/usr/bin/python
import os
import sys

################# CONSTANTS Change if input files format changes #####################################################
LINE_OF_SS_IN_LEVELS_FILE = 6
LINE_OF_SS_IN_TRANSITIONS_FILE = 6

HEADER_LINES_NUM_LEVELS_FILE = 11
HEADER_LINES_NUM_TRANSITIONS_FILE = 13

HEADER_FORMAT_STRING = '%10s%6s%10s%16s%6s%10s%16s%16s%16s%16s'
OUTPUT_FORMAT_STRING = '%10s%6d%10s%16s%6d%10s%16s%16s%16s%16s'


######################### FUNCTIONS #################################################################################
# Read SS from a file
def get_ss(f, alomic_num_line_num, header_lines_num):
    headers = {}
    element_atomic_num = 0

    # loop over header lines
    for line_num in range(0, header_lines_num):
        line = f.readline()
        columns = line.split('=')  # Split eachline by =
        if len(columns) == 2:  # Line was like key = value
            key = columns[0].strip()
            value = columns[1].strip()
            headers[key] = value  # Store key and value
        if line_num == alomic_num_line_num - 1:  # For atomic number we don't know key, but know line number
            element_atomic_num = float(value)

    if element_atomic_num == 0:
        sys.stderr.write('Unexpected format: can\'t find element atomic number in file ' + f.name + '\n')
        exit(1)
    if not 'NELE' in headers:
        sys.stderr.write('Unexpected format: can\'t find NELE in file ' + f.name + '\n')
        exit(1)

    return element_atomic_num + 1 - int(headers['NELE'])


# Read levels energy into a dictionary
def read_levels(file):
    energy_by_level = {}
    for line in file:
        columns = line.split()
        if len(columns) > 0:  # Skip empty lines
            #### Remember indexes in array start from 0 !!!
            energy_by_level[columns[0]] = columns[2]
    return energy_by_level


# Read AI coefficient
def read_ai(ai_f):
    for line in ai_f:
        columns = line.split()
        if len(columns) > 4:  # Skip empty lines
            return int(columns[0])

    sys.stderr.write('Unexpected format of autoionization file ' + sys.argv[3] + '\n')
    exit(1)


# Make it negative if greater than ai or increment by one otherwise
def convert_level(level_str, ai_coef, start_level, start_ai_level):
    level = int(level_str)
    if level >= ai_coef:
        if start_ai_level is None:
            sys.stderr.write('Detected AI level, where it don\'t expected. Level = ' + level_str + '\n')
            exit(1)
        return abs(ai_coef - 1 - level) + start_ai_level - 1
    else:
        return level + start_level


def read_utl_transition_file(ss_f, ss_lev):
    ss_f.readline()  # skip first line
    ss = int(ss_lev)
    level_positive_detected = False
    level_negative_detected = False
    start_negative_level = None
    for line in ss_f:
        columns = line.split()
        if level_positive_detected:
            start_positive_level = int(columns[len(columns) - 1])
            level_positive_detected = False
        if level_negative_detected:
            start_negative_level = int(columns[len(columns) - 1])
            level_negative_detected = False
        if len(columns) > 1 and columns[1][0] == '[' and int(columns[0]) == ss:  # ASK - 33 w/o [
            level_positive_detected = True
        if len(columns) > 2 and columns[2] == "AIs" and int(columns[0]) == ss:
            level_negative_detected = True

    return start_positive_level, start_negative_level


# Main loop of converting transitions file line by line
def print_transitions(trans_f, ss, levels_energy, ai_coef, start_level, start_ai_level):
    print(HEADER_FORMAT_STRING % (
        'SS', 'UP', 'UP ST.W', 'UP ENERGY', 'LOW', 'LOW ST.W', 'LOW ENERGY', 'PHOTON ENERGY', 'EINSTEIN A', 'STD DEV'))

    levels_to_output = {}
    for line in trans_f:
        columns = line.split()
        if len(columns) > 5:  # Skip empty lines
            #### Remember indexes in array start from 0 !!!
            col_1_ss = ss
            col_2_up_level = convert_level(columns[0], ai_coef, start_level, start_ai_level)
            col_3_up_stat_weight = int(columns[1]) + 1
            col_4_up_energy = levels_energy[columns[0]]  # Use original column as a key
            col_5_low_level = convert_level(columns[2], ai_coef, start_level, start_ai_level)
            col_6_low_stat_weight = int(columns[3]) + 1
            col_7_low_energy = levels_energy[columns[2]]  # Use original column as a key
            col_8_photon_energy = columns[4]
            col_9_einstein = columns[7]
            col_10_std_dev = columns[5]

            out = OUTPUT_FORMAT_STRING % (
                col_1_ss, col_2_up_level, col_3_up_stat_weight, col_4_up_energy, col_5_low_level, col_6_low_stat_weight,
                col_7_low_energy, col_8_photon_energy, col_9_einstein, col_10_std_dev)

            ### Check for the same levels whether it was stored already
            dictionary_key = (col_2_up_level, col_5_low_level)  # The key is tuple of 2 levels (int values)
            if dictionary_key in levels_to_output:
                stored = levels_to_output[dictionary_key]
                if float(stored[0]) < float(col_9_einstein):  # if stored line has smaller einstein coef, replace it
                    levels_to_output[dictionary_key] = [col_9_einstein, out]  # Store einstein coef and output line
            else:
                levels_to_output[dictionary_key] = [col_9_einstein, out]  # Store einstein coef and output line

    for key, value in sorted(levels_to_output.iteritems()):
        print(value[1])


################################## HERE THE MAIN CODE ##################################################################
if len(sys.argv) != 5:
    sys.stderr.write(
        'Usage: ' + sys.argv[0] + ' levels_file transitions_file autoionization_file UTL_transition_file'+'\n')
    exit(1)

if os.path.exists(sys.argv[1]):
    with open(sys.argv[1], 'r') as levels_f:
        if os.path.exists(sys.argv[2]):
            with open(sys.argv[2], 'r') as trans_f:
                if os.path.exists(sys.argv[3]):
                    with open(sys.argv[3], 'r') as ai_f:
                        if os.path.exists(sys.argv[4]):
                            with open(sys.argv[4], 'r') as ss_f:  # UTL/transition
                                # Read Spectroscopic Symbol from two files and compare
                                ss_lev = get_ss(levels_f, LINE_OF_SS_IN_LEVELS_FILE, HEADER_LINES_NUM_LEVELS_FILE)
                                ss_tran = get_ss(trans_f, LINE_OF_SS_IN_TRANSITIONS_FILE,
                                                 HEADER_LINES_NUM_TRANSITIONS_FILE)
                                if ss_lev != ss_tran:
                                    sys.stderr.write(
                                        'Spectroscopic symbol is not identical in files or is not on 6 line'+'\n')
                                    exit(1)

                                start_level, start_ai_level = read_utl_transition_file(ss_f, ss_lev)

                                # Read levels energy into a dictionary
                                levels_energy = read_levels(levels_f)

                                # Read autoionization coefficient from autoionization file
                                ai_coef = read_ai(ai_f)

                                # Look over transitions and print output lines
                                print_transitions(trans_f, ss_lev, levels_energy, ai_coef, start_level, start_ai_level)
                        else:
                            sys.stderr.write('Can\'t open UTL/transition file ' + sys.argv[3] + '\n')
                            exit(1)
                else:
                    sys.stderr.write('Can\'t open autoionization file ' + sys.argv[3] + '\n')
                    exit(1)
        else:
            sys.stderr.write('Can\'t open transitions file ' + sys.argv[2] + '\n')
            exit(1)
else:
    sys.stderr.write('Can\'t open levels file ' + sys.argv[1] + '\n')
    exit(1)
