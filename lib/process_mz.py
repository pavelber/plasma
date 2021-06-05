import os
import shutil

from lib.utils import skip_n_lines

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

he_lines_names = {
    (('1s2p', '3'), ('1s2', '1')): "IC-line",
    (('1s2p', '3'), ('1s2', '1')): "He-alfa",
    (('1s3p', '3'), ('1s2', '1')): "He-beta IC",
    (('1s3p', '3'), ('1s2', '1')): "He-beta"
}

h_lines_names = {
    (('2p', '2'), ('1s', '2')): "Ly-alfa 1/2",
    (('2p', '4'), ('1s', '2')): "Ly-alfa 3/2",
    (('3p', '2'), ('1s', '2')): "Ly-beta 1/2",
    (('3p', '4'), ('1s', '2')): "Ly-beta 3/2"
}


def create_key(parts, letter_2_config):  # level config, stat weight
    if parts[9] not in letter_2_config or parts[11] not in letter_2_config:
        return None
    return (letter_2_config[parts[9]], parts[10][2]), (letter_2_config[parts[11]], parts[12][2])


COEFF_EINS_INDEX_IN_MS = 5
WAVE_LENGTH_INDEX_IN_MS = 3

# TODO: check
COEFF_EINS_INDEX_IN_SPECTR = 5
WAVE_LENGTH_INDEX_IN_SPECTR = 4


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
                wave_length = parts[WAVE_LENGTH_INDEX_IN_MS]
                if key is not None:
                    if key in mz:
                        print "Adding " + str(key) + " in " + table_name + " was " + \
                              str(mz[key]) + " - " + coeff_eins + '\n' + line
                    else:
                        mz[key] = []
                    mz[key].append((line, coeff_eins, wave_length))

    return mz


def create_key_spectr(parts, search_table_in1, spectr_num_index, from_index, to_index):
    # spectroscopic number,from level, stat_weight, energy -   number, to level, stat_weight, energy
    return search_table_in1[(parts[spectr_num_index], parts[from_index])][:2], search_table_in1[
                                                                                   (parts[to_index], parts[2])][:2]


def find_energy_for_spectr_line(parts, search_table_in1):
    return float(search_table_in1[(parts[0], parts[1])][2])


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
                level_num = parts[6]
                stat_weight = parts[2]
                energy = parts[3]
                levels[(spect_num, level_num)] = convert_level_configs(parts[0], parts[1]), stat_weight, energy
    return levels


def replace(table, key, parts, table_name):
    lines_with_einst = table[key]
    old_einstein = parts[COEFF_EINS_INDEX_IN_SPECTR]
    old_einstein_f = float(old_einstein)
    min_line_with_einst = min(lines_with_einst, key=lambda (x): abs(old_einstein_f - float(x[1])) / old_einstein_f)
    old_wave_length = parts[WAVE_LENGTH_INDEX_IN_SPECTR]
    parts[COEFF_EINS_INDEX_IN_SPECTR] = min_line_with_einst[1]
    parts[WAVE_LENGTH_INDEX_IN_SPECTR] = min_line_with_einst[2]
    print "Replaced for key " + str(key) + " from " + table_name
    return old_einstein, old_wave_length


def find_line_name(names, k):
    if k in names:
        return names[k]
    else:
        return "Unknown line"


def find_line_name_h(key):
    return find_line_name(h_lines_names, key)


def find_line_name_he(key):
    return find_line_name(he_lines_names, key)


def map_lines_names(el_num, old_spectr_path, search_table_h_iia, search_table_he_iib, search_table_in1,
                    spectr_num_index, from_index, to_index):
    line_names = {}
    he_lines = {(('1s2p', '3'), ('1s2', '1')): [], (('1s3p', '3'), ('1s2', '1')): []}
    with open(old_spectr_path, "rb") as inf:
        inf.readline()  # header
        for line in inf:
            parts = line.split()
            sp_num = int(parts[0])
            if sp_num == el_num:  # H - like
                key = create_key_spectr(parts, search_table_in1, spectr_num_index, from_index, to_index)
                line_name = find_line_name_h(key)
                line_names[str(parts)] = line_name
            elif sp_num == sp_num == el_num - 1:  # He - like
                key = create_key_spectr(parts, search_table_in1, spectr_num_index, from_index, to_index)
                energy = find_energy_for_spectr_line(parts, search_table_in1)
                if key in he_lines:
                    he_lines[key].append((parts, energy))
    ic_line = min(he_lines[(('1s2p', '3'), ('1s2', '1'))], key=lambda (x): x[1])
    he_alpha = max(he_lines[(('1s2p', '3'), ('1s2', '1'))], key=lambda (x): x[1])
    ic = min(he_lines[(('1s3p', '3'), ('1s2', '1'))], key=lambda (x): x[1])
    he_beta = max(he_lines[(('1s3p', '3'), ('1s2', '1'))], key=lambda (x): x[1])
    line_names[str(ic_line[0])] = 'IC-line'
    line_names[str(he_alpha[0])] = 'He-alpha'
    line_names[str(ic[0])] = 'IC'
    line_names[str(he_beta[0])] = 'He-beta'
    return line_names


def replace_values_spectr(el_num, old_spectr_path, search_table_h_iia, search_table_he_iib, search_table_in1,
                          spectr_path, warnings_file_path):
    replace_values(el_num, old_spectr_path, search_table_h_iia, search_table_he_iib, search_table_in1,
                   spectr_path, warnings_file_path, 0, 1, 2)


def replace_values(el_num, old_file_path, search_table_h_iia, search_table_he_iib, search_table_in1, file_path,
                   warnings_file_path, spectr_num_index, from_index, to_index):
    line_names = map_lines_names(el_num, old_file_path, search_table_h_iia, search_table_he_iib, search_table_in1,
                                 spectr_num_index, from_index, to_index)
    with open(warnings_file_path, 'ab') as warn_f:
        warn_f.write("Replaced in " + old_file_path + os.linesep)
        with open(old_file_path, "rb") as inf:
            inf.readline()  # header
            with open(file_path, "wb") as outf:
                for line in inf:
                    replaced = False
                    parts = line.split()
                    sp_num = int(parts[0])
                    if sp_num == el_num:  # H - like
                        key = create_key_spectr(parts, search_table_in1, spectr_num_index, from_index, to_index)
                        if key in search_table_h_iia:
                            line_name = line_names[str(parts)]
                            old_einstein, old_wavelength = \
                                replace(search_table_h_iia, key, parts, 'IIa')
                            replaced = True
                        else:
                            replaced = False
                    elif sp_num == sp_num == el_num - 1:  # He - like
                        key = create_key_spectr(parts, search_table_in1, spectr_num_index, from_index, to_index)
                        if key in search_table_he_iib:
                            line_name = line_names[str(parts)]
                            old_einstein, old_wavelength = \
                                replace(search_table_he_iib, key, parts, 'IIb')
                            replaced = True
                        else:
                            replaced = False
                    outf.write("%2s %4s %4s %7s %13s %12s"
                               % (parts[0], parts[1], parts[2], parts[3], parts[4], parts[5]))
                    if replaced:
                        outf.write("  # " + line_name)
                        warn_f.write(line.rstrip() + "#  " + line_name + " " +
                                     "einstein coefficient: " + old_einstein + " -> " + parts[
                                         COEFF_EINS_INDEX_IN_SPECTR] +
                                     ", wavelength :" + old_wavelength + " -> " + parts[WAVE_LENGTH_INDEX_IN_SPECTR] +
                                     os.linesep)
                    outf.write("\n")


def adjust_eins_weight(python_path, el_num, out_dir):
    spectr_path = os.path.join(out_dir, "SPECTR.INP")
    old_spectr_path = os.path.join(out_dir, "SPECTR.INP.UPD")

    print "Creation of " + spectr_path + " with updated Einstein weights"
    shutil.move(spectr_path, old_spectr_path)
    search_table_h_iia = read_mz("IIa", el_num, letter2config_h)
    search_table_he_iib = read_mz("IIb", el_num, letter2config_he)
    search_table_in1 = read_in1_inp(out_dir)
    warnings_file_path = os.path.join(out_dir, "WARNINGS.txt")
    replace_values_spectr(el_num, old_spectr_path, search_table_h_iia, search_table_he_iib, search_table_in1,
                          spectr_path,
                          warnings_file_path)
