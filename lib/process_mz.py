import os

# TODO: check
from lib.utils import skip_n_lines

# letter2config_he - letter to config of the last level
# search_table_he - ((from last level, stat weight), (to last level, start weight))
# search_table_in1 - spectr num, level num to stat weight, last level config

letter2config_he = {
    "Y": "1s", "R": "2p", "R'": "3p",
    "C": "2s2p", "E": "2s2s", "F": "2p2p", "P": "1s2p", "S": "1s2s", "S'": "ls3s", "P'": "1s3p",
    "A'": "2p3d", "B'": "2p3s", "C'": "2s3p", "F'": "2p3p", "G'": "2s3d", "E'": "2s3s", "D'": "1s3d"
}

# TODO: change
letter2config_li = {
    "Y": "1s", "R": "2p", "R'": "3p",
    "C": "2s2p", "E": "2s2s", "F": "2p2p", "P": "1s2p", "S": "1s2s", "S'": "ls3s", "P'": "1s3p",
    "A'": "2p3d", "B'": "2p3s", "C'": "2s3p", "F'": "2p3p", "G'": "2s3d", "E'": "2s3s", "D'": "1s3d"
}


# TODO: change, check whether may be not in the dictionary or it is mistake
def create_key_he(parts):  # level config, stat weight
    if parts[9] not in letter2config_he or parts[11] not in letter2config_he:
        return None
    return (letter2config_he[parts[9]], parts[10][2]), (letter2config_he[parts[11]], parts[12][2])


# TODO: change, check whether may be not in the dictionary or it is mistake
def create_key_li(parts):
    if parts[9] not in letter2config_li or parts[11] not in letter2config_li:
        return None
    return (letter2config_li[parts[9]], parts[10][2]), (letter2config_li[parts[11]], parts[12][2])


# TODO: check
COEFF_EINS_INDEX_IN_MS = 4

# TODO: check
COEFF_EINS_INDEX_IN_SPECTR = 5


def read_mz():
    path = os.path.dirname(__file__)
    if path == "" or path is None:
        path = "."
    mz_file_path = path + os.path.sep + "MZ.csv"
    he = {}
    li = {}
    with open(mz_file_path, "rb") as mz_file:
        mz_file.readline()
        for line in mz_file:
            parts = line.split(",")
            key_he = create_key_he(parts)
            key_li = create_key_li(parts)
            coeff_eins = parts[COEFF_EINS_INDEX_IN_MS]
            he[key_he] = coeff_eins
            li[key_li] = coeff_eins
    return he, li


def create_key_spectr(parts, search_table_in1):
    return search_table_in1[(parts[0], parts[1])], \
           search_table_in1[(parts[0], parts[2])]  # spectroscopic number,from level -   number, to level


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
                    parts[1], parts[2]  # TODO change - last part of level configuration and stat weight
    return levels


def adjust_eins_weight(python_path, el_num, out_dir):
    print "Creation of " + os.path.join(out_dir, "SPECTR.INP.UPD") + " with updated Einstein weights"
    search_table_he, search_table_li = read_mz()
    search_table_in1 = read_in1_inp(out_dir)
    with open(os.path.join(out_dir, "SPECTR.INP"), "rb") as inf:
        inf.readline()  # header
        with open(os.path.join(out_dir, "SPECTR.INP.UPD"), "wb") as outf:
            for line in inf:
                replaced = False
                parts = line.split()
                sp_num = int(parts[0])
                if sp_num == el_num - 2:  # He like
                    key = create_key_spectr(parts, search_table_in1)
                    if key in search_table_he:
                        parts[COEFF_EINS_INDEX_IN_SPECTR] = search_table_he[key]
                        replaced = True
                elif sp_num == el_num - 3:  # Li like
                    key = create_key_spectr(parts, search_table_in1)
                    if key in search_table_li:
                        parts[COEFF_EINS_INDEX_IN_SPECTR] = search_table_li[key]
                        replaced = True
                outf.write("%2s %4s %4s %7s %13s %12s"
                           % (parts[0], parts[1], parts[2], parts[3], parts[4], parts[5]))
                if replaced:
                    outf.write("  #replaced by key " + key)
                outf.write("\n")
