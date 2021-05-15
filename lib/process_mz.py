import os

# TODO: check
from lib.utils import skip_n_lines

# Q: He/H, table, letter to config 1 tabl - IIa, 2aq iib
# Q recongize tranzition

# 1. H:
# Not correct IIa, no ' in IIa
# don't use n1 in the key
# use n1 = 2, check that for n1=3 is the same einstein number

# 2. take wave length and fix, 1st column after Z in MZ, 5th in spectr

# 3. in spectr also add to comment line name

# 4. He : IIb, the same logic, take n =2, for n=3 should be the same eins
# 5  Fix Y, R, R' - make like in the table
# We have the same perehod and stat weight for different lines
# we should choose according to the first digit after R -
# we see also several lines with the same perehov in in1.inp, but with different energy
# for every line in in1.inp we store the second line with the same (from, to, stat weight)
# if our line is with less energy we search for the perehod with 3 as the first digit
# if with more than with 1 as first digit

letter2config_h = {
    "Y": "1s", "R": "2p", "R'": "3p",
    # "C": "2s2p", "E": "2s2s", "F": "2p2p", "P": "1s2p", "S": "1s2s", "S'": "1s3s", "P'": "1s3p",
    # "A'": "2p3d", "B'": "2p3s", "C'": "2s3p", "F'": "2p3p", "G'": "2s3d", "E'": "2s3s", "D'": "1s3d"
}

# TODO: change
letter2config_he = {
    "Y": "1s2", "R": "1s2p", "R'": "1s3p",
    # "C": "2s2p", "E": "2s2s", "F": "2p2p", "P": "1s2p", "S": "1s2s", "S'": "ls3s", "P'": "1s3p",
    # "A'": "2p3d", "B'": "2p3s", "C'": "2s3p", "F'": "2p3p", "G'": "2s3d", "E'": "2s3s", "D'": "1s3d"
}


# TODO: change, check whether may be not in the dictionary or it is mistake
def create_key(parts,letter_2_config):  # level config, stat weight
    if parts[9] not in letter_2_config or parts[11] not in letter_2_config:
        return None
    return (letter_2_config[parts[9]], parts[10][2]), (letter_2_config[parts[11]], parts[12][2])

# TODO: check
COEFF_EINS_INDEX_IN_MS = 5

# TODO: check
COEFF_EINS_INDEX_IN_SPECTR = 5


def read_mz(table_name, el_num, letter_2_config):
    path = os.path.dirname(__file__)
    if path == "" or path is None:
        path = "."
    mz_file_path = path + os.path.sep + "MZ" + table_name + ".csv"
    mz = {}
    with open(mz_file_path, "rb") as mz_file:
        mz_file.readline()
        for line in mz_file:
            parts = line.split(",")
            if int(parts[2]) == el_num:
                key = create_key(parts, letter_2_config)
                coeff_eins = str(float(parts[COEFF_EINS_INDEX_IN_MS]) * 1e13)
                if key is not None:
#                   if key in mz:
#                      print "Overriding " + str(key) + " in " + table_name + " was " + \
#                              mz[key] + " - " + coeff_eins + '\n' + line
                    mz[key] = coeff_eins

    return mz


def create_key_spectr(parts, search_table_in1):
    return search_table_in1[(parts[0], parts[1])], \
           search_table_in1[(parts[0], parts[2])]  # spectroscopic number,from level -   number, to level


def convert_config(cfg):
    if cfg[-1] == "0":
        return ""
    elif cfg[-1] == "1":
        return cfg[:-1]
    return cfg


def convert_level_configs(cfg1, cfg2):
    return convert_config(cfg1) + convert_config(cfg2)


def read_in1_inp(out_dir):
    levels = {}
    with open(os.path.join(out_dir, "IN1.INP"), "rb") as inf:
        skip_n_lines(inf, 20)
        for line in inf:
            parts = line.split()
            if len(parts) == 1:
                spect_num = parts[0]
            if len(parts) == 7:
                levels[(spect_num, parts[6])] = \
                    convert_level_configs(parts[0], parts[1]), parts[
                        2]  # TODO change - last part of level configuration and stat weight
    return levels


def adjust_eins_weight(python_path, el_num, out_dir):
    print "Creation of " + os.path.join(out_dir, "SPECTR.INP.UPD") + " with updated Einstein weights"
    search_table_h_iia = read_mz("IIa", el_num, letter2config_h)
    search_table_he_iib = read_mz("IIb", el_num, letter2config_he)
    search_table_in1 = read_in1_inp(out_dir)
    with open(os.path.join(out_dir, "SPECTR.INP"), "rb") as inf:
        inf.readline()  # header
        with open(os.path.join(out_dir, "SPECTR.INP.UPD"), "wb") as outf:
            for line in inf:
                replaced = False
                parts = line.split()
                sp_num = int(parts[0])
                if sp_num == el_num:  # H - like
                    key = create_key_spectr(parts, search_table_in1)
                    if key in search_table_h_iia:
                        old_einstein = parts[COEFF_EINS_INDEX_IN_SPECTR]
                        parts[COEFF_EINS_INDEX_IN_SPECTR] = search_table_h_iia[key]
                        replaced = True
                        print "Replaced for key " + str(key) + " from IIa"
                elif sp_num == sp_num == el_num - 1:  # He - like
                    key = create_key_spectr(parts, search_table_in1)
                    if key in search_table_he_iib:
                        old_einstein = parts[COEFF_EINS_INDEX_IN_SPECTR]
                        parts[COEFF_EINS_INDEX_IN_SPECTR] = search_table_he_iib[key]
                        replaced = True
                        print "Replaced for key " + str(key) + " from IIb"
                # elif sp_num == el_num - 2  # Li like
                #     key = create_key_spectr(parts, search_table_in1)
                #     if key in search_table_li:
                #         old_einstein = parts[COEFF_EINS_INDEX_IN_SPECTR]
                #         parts[COEFF_EINS_INDEX_IN_SPECTR] = search_table_li[key]
                #         replaced = True
                #         print "Replaced for key " + str(key)
                outf.write("%2s %4s %4s %7s %13s %12s"
                           % (parts[0], parts[1], parts[2], parts[3], parts[4], parts[5]))
                if replaced:
                    outf.write(
                        "  # replaced " + old_einstein + " by " + parts[COEFF_EINS_INDEX_IN_SPECTR] + " for key " + str(
                            key))
                outf.write("\n")
