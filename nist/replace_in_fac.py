################## MAIN ######################
import copy
import os
import sys
from os import listdir

from lib.utils import error, nist_strip, skip_n_lines


def compute_2j(j_str):
    if j_str[-1] == '?':
        j_str = j_str[0:-1]
    parts = j_str.split("/")
    if len(parts) == 1:
        return str(int(parts[0]) * 2)
    else:
        return parts[0]


def read_nist(levels_dir):
    configs_per_num = {}
    for nist_file in listdir(levels_dir):
        configs = {}
        num = os.path.splitext(nist_file)[0]
        configs_per_num[num] = configs
        with open(os.path.join(levels_dir, nist_file), 'rb') as levels_file:
            skip_n_lines(levels_file, 1)
            for line in levels_file:
                data = line.split(',')
                config = nist_strip(data[0]).split('.')
                if nist_strip(data[2]) == '---':
                    break
                j = compute_2j(nist_strip(data[2]))
                eV = nist_strip(data[3])
                c1 = config[0]
                if len(c1) > 0:
                    if len(config) < 2:
                        c2 = ''
                    else:
                        c2 = config[1]

                    if c1[-1].isalpha():
                        c1 = c1 + '1'
                    if len(c2) > 0 and c2[-1].isalpha():
                        c2 = c2 + '1'
                    if (c1, c2, j) not in configs:
                        configs[(c1, c2, j)] = []
                    configs[(c1, c2, j)].append(eV)
    return configs_per_num


def extract_config_and_j(data):
    j_2 = data[5]

    if '*' in data[7]:
        config_index = 8
    else:
        config_index = 7

    c1 = data[config_index]

    if '+' in data[config_index + 1] or '-' in data[config_index + 1]:
        c2 = ''
    else:
        c2 = data[config_index + 1]

    return c1, c2, j_2


def format_energy_for_fac(eV):
    l = len(eV)
    energy = float(eV[1: l - 1])
    return "%.8E" % energy


def clean_energy_for_fac(eV):
    l = len(eV)

    if l == 0:
        energy = '0.000'
    elif eV[0] == '[' or eV[0] == '(':
        energy = eV[1: l - 1]
    elif '+x' in eV:
        energy = eV.replace('+x', '')
    elif '?' in eV:
        energy = eV.replace('?', '')
    else:
        energy = eV
    return energy


def renumerate(energy_to_levels_list):
    i = 0
    old_level_to_new_level_list = []
    for energy_to_levels in energy_to_levels_list:
        old_level_to_new_level = {}
        old_level_to_new_level_list.append(old_level_to_new_level)
        energies = energy_to_levels.items()
        energies = sorted(energies, key=lambda x: float(x[0]))
        for e in energies:
            old_level_to_new_level[energy_to_levels[e[0]]] = str(i)
            i = i + 1
    return old_level_to_new_level_list


def no_4_in_config(config):
    return '4' not in config[0] and '4' not in config[1]


def recreate_fac_lev(old, new, levels, next_levels):
    current_levels = levels
    nele_counter = 0
    second_section = False

    energy_to_levels_list = []
    for line in old:
        data = line.split()
        if line.startswith("NELE"):
            nele_counter = nele_counter + 1
        if nele_counter == 2:
            second_section = True
            current_levels = copy.deepcopy(next_levels)
        if len(data) < 9:
            new.write(line)
            energy_to_levels = {}
            energy_to_levels_list.append(energy_to_levels)
        else:
            config = extract_config_and_j(data)

            level_num = data[0]
            energy = data[2]
            if config in current_levels and no_4_in_config(config) and len(current_levels[config]) > 0:
                eV = current_levels[config].pop(0)
                energy_str = "%.8E" % float(clean_energy_for_fac(eV))
                new.write(line.replace(energy, energy_str))
            else:
                new.write(line)
                energy_str = energy

            if energy_str in energy_to_levels:
                print("We replace " + energy_to_levels[
                    energy_str] + " for " + level_num + " with energy " + energy_str)
            if not second_section:
                energy_to_levels[energy_str] = level_num

    return energy_to_levels_list


def renumerate_fac_lev(old, new, old_to_new_level_list, old_to_new_level_list_next):
    current_old_to_new = old_to_new_level_list
    nele_counter = 0
    second_section = False

    lines = []
    i = -1
    for line in old:
        data = line.split()
        if line.startswith("NELE"):
            nele_counter = nele_counter + 1
        if nele_counter == 2:
            second_section = True
            current_old_to_new = old_to_new_level_list_next
        if current_old_to_new is None or second_section:
            new.write(line)
        elif len(data) < 9:
            i = i + 1
            old_to_new_level = current_old_to_new[i]
            sorted_lines = sorted(lines, key=lambda x: int(x[0:7]))
            for l in sorted_lines:
                new.write(l)
            lines = []
            new.write(line)
        else:
            level_num = data[0]

            if level_num in old_to_new_level:
                new_level = "%6d" % int(old_to_new_level[level_num])
            else:
                print("Can't find level " + level_num)
                new_level = level_num

            lines.append(line.replace(line[0: 6], new_level))
    sorted_lines = sorted(lines, key=lambda x: int(x[0:7]))
    for l in sorted_lines:
        new.write(l)


def recreate_fac_ai(old, new, level_to_energy):
    for line in old:
        data = line.split()
        if len(data) < 6:
            new.write(line)
        else:
            low = data[0]
            high = data[2]
            low_energy = float(level_to_energy[low])
            high_energy = float(level_to_energy[high])
            diff_energy = "%.4E" % (- high_energy + low_energy)
            new.write(line.replace(data[4], diff_energy))


def recreate_fac_ce_ci(old, new, level_to_energy):
    for line in old:
        data = line.split()
        if len(data) != 6:
            new.write(line)
        else:
            low = data[0]
            high = data[2]
            low_energy = float(level_to_energy[low])
            high_energy = float(level_to_energy[high])
            diff_energy = "%.4E" % (high_energy - low_energy)
            new.write(line.replace(data[4], diff_energy))


def recreate_fac_tr(old, new, level_to_energy):
    for line in old:
        data = line.split()
        if len(data) != 9:
            new.write(line)
        else:
            low = data[0]
            high = data[2]
            low_energy = float(level_to_energy[low])
            high_energy = float(level_to_energy[high])
            diff_energy = "%.6E" % (- high_energy + low_energy)
            new.write(line.replace(data[4], diff_energy))


################################# MAIN ################################################################

if len(sys.argv) < 3:
    error('\nUsage: ' + sys.argv[0] + ' file-with-nist-levels-csv directory-with-fac')

nist_file = os.path.abspath(sys.argv[1])
fac_nums_dir = os.path.abspath(sys.argv[2])
fac_nums_out_dir = os.path.abspath(sys.argv[3])

levels_per_num = read_nist(nist_file)
old_energy_to_levels = {}
old_2_new = {}

if not os.path.exists(fac_nums_out_dir):
    os.mkdir(fac_nums_out_dir)

for fac_dir_name in listdir(fac_nums_dir):
    num = fac_dir_name
    fac_dir = os.path.join(fac_nums_dir, fac_dir_name)
    fac_out_dir = os.path.join(fac_nums_out_dir, fac_dir_name)
    fac_lev = os.path.join(fac_dir, "fac.lev")
    fac_lev_tmp = os.path.join(fac_dir, "fac-tmp.lev")
    fac_out_lev = os.path.join(fac_out_dir, "fac.lev")

    if not os.path.exists(fac_out_dir):
        os.mkdir(fac_out_dir)

    if os.path.isdir(fac_lev) or not os.path.exists(fac_lev):
        error(fac_lev + " does not exists or is a directory")

    levels = levels_per_num[num]
    next_num = str(int(num) + 1)
    if next_num in levels_per_num:
        next_levels = levels_per_num[next_num]
    else:
        next_levels = []

    with open(fac_lev, 'rb') as fac_lev_file:
        with open(fac_lev_tmp, 'wb') as fac_lev_tmp_file:
            old_energy_to_levels[num] = recreate_fac_lev(fac_lev_file, fac_lev_tmp_file, levels, next_levels)

    old_2_new[num] = renumerate(old_energy_to_levels[num])

for fac_dir_name in listdir(fac_nums_dir):
    num = fac_dir_name
    next_num = str(int(num) + 1)
    fac_dir = os.path.join(fac_nums_dir, fac_dir_name)
    fac_lev_tmp = os.path.join(fac_dir, "fac-tmp.lev")
    fac_lev_new = os.path.join(fac_dir, "fac-new.lev")
    fac_out_dir = os.path.join(fac_nums_out_dir, fac_dir_name)
    fac_out_lev = os.path.join(fac_out_dir, "fac.lev")
    with open(fac_lev_tmp, 'rb') as fac_lev_tmp_file:
        with open(fac_out_lev, 'wb') as fac_lev_new_file:
            if next_num in old_2_new:
                old_to_new_next = old_2_new[next_num]
            else:
                old_to_new_next = None
            renumerate_fac_lev(fac_lev_tmp_file, fac_lev_new_file, old_2_new[num], old_to_new_next)
