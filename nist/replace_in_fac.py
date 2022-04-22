################## MAIN ######################
import os
import sys

from lib.utils import error, nist_strip


def read_nist(levels_file):
    configs = {}
    for line in levels_file:
        data = line.split(',')
        config = nist_strip(data[0]).split('.')
        j = nist_strip(data[2])
        eV = nist_strip(data[3])
        c1 = config[0]
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
    return configs


def extract_config_and_j(data):
    j2 = data[5]
    j = str(int(j2) / 2)

    if '*' in data[7]:
        config_index = 8
    else:
        config_index = 7

    c1 = data[config_index]

    if '+' in data[config_index + 1] or '-' in data[config_index + 1]:
        c2 = ''
    else:
        c2 = data[config_index + 1]

    return (c1, c2, j)


def format_energy_for_fac(eV):
    l = len(eV)
    energy = float(eV[1: l - 1])
    return "%.8E" % energy


def recreate_fac_lev(old, new, levels):
    last = 0.0
    levels_to_energy = {}
    for line in old:
        data = line.split()
        if len(data) < 9:
            new.write(line)
        else:
            config = extract_config_and_j(data)

            level_num = data[0]
            energy = data[2]
            if config in levels:
                eV = min(levels[config], key=lambda value: abs(float(format_energy_for_fac(value)) - float(energy)))
                energy_str = format_energy_for_fac(eV)
                new.write(line.replace(energy, energy_str))
            else:
                new.write(line)
                energy_str = energy

            levels_to_energy[level_num] = energy_str
            if last > float(energy_str):
                print("Low energy in level " + level_num)
            else:
                last = float(energy_str)

    return levels_to_energy


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

if len(sys.argv) < 2:
    error('\nUsage: ' + sys.argv[0] + ' file-with-nist-levels-csv directory-with-fac')

nist_file = os.path.abspath(sys.argv[1])
fac_dir = os.path.abspath(sys.argv[2])
fac_lev = os.path.join(fac_dir, "fac.lev")
fac_ai = os.path.join(fac_dir, "fac.ai")
fac_ce = os.path.join(fac_dir, "fac.ce")
fac_ci = os.path.join(fac_dir, "fac.ci")
fac_tr = os.path.join(fac_dir, "fac.tr")
fac_rr = os.path.join(fac_dir, "fac.rr")

fac_lev_new = os.path.join(fac_dir, "fac-new.lev")
fac_ai_new = os.path.join(fac_dir, "fac-new.ai")
fac_ce_new = os.path.join(fac_dir, "fac-new.ce")
fac_ci_new = os.path.join(fac_dir, "fac-new.ci")
fac_rr_new = os.path.join(fac_dir, "fac-new.rr")
fac_tr_new = os.path.join(fac_dir, "fac-new.tr")

if not os.path.isdir(fac_dir) or not os.path.exists(fac_dir):
    error(fac_dir + " does not exists or is not a directory")

if os.path.isdir(fac_lev) or not os.path.exists(fac_lev):
    error(fac_lev + " does not exists or is a directory")

if os.path.isdir(fac_ai) or not os.path.exists(fac_ai):
    error(fac_ai + " does not exists or is a directory")

if os.path.isdir(fac_ce) or not os.path.exists(fac_ce):
    error(fac_ce + " does not exists or is a directory")

if os.path.isdir(fac_ci) or not os.path.exists(fac_ci):
    error(fac_ci + " does not exists or is a directory")

if os.path.isdir(fac_tr) or not os.path.exists(fac_tr):
    error(fac_tr + " does not exists or is a directory")

if os.path.isdir(fac_rr) or not os.path.exists(fac_rr):
    error(fac_rr + " does not exists or is a directory")

if os.path.isdir(nist_file) or not os.path.exists(nist_file):
    error(nist_file + " does not exists or is a directory")

with open(nist_file, 'rb') as levels_file:
    levels = read_nist(levels_file)

with open(fac_lev, 'rb') as fac_lev_file:
    with open(fac_lev_new, 'wb') as fac_lev_new_file:
        old_level_to_energy = recreate_fac_lev(fac_lev_file, fac_lev_new_file, levels)

with open(fac_ai, 'rb') as fac_ai_file:
    with open(fac_ai_new, 'wb') as fac_ai_new_file:
        recreate_fac_ai(fac_ai_file, fac_ai_new_file, old_level_to_energy)

with open(fac_ce, 'rb') as fac_ce_file:
    with open(fac_ce_new, 'wb') as fac_ce_new_file:
        recreate_fac_ce_ci(fac_ce_file, fac_ce_new_file, old_level_to_energy)

with open(fac_ci, 'rb') as fac_ci_file:
    with open(fac_ci_new, 'wb') as fac_ci_new_file:
        recreate_fac_ce_ci(fac_ci_file, fac_ci_new_file, old_level_to_energy)

with open(fac_rr, 'rb') as fac_rr_file:
    with open(fac_rr_new, 'wb') as fac_rr_new_file:
        recreate_fac_ce_ci(fac_rr_file, fac_rr_new_file, old_level_to_energy)

with open(fac_tr, 'rb') as fac_tr_file:
    with open(fac_tr_new, 'wb') as fac_tr_new_file:
        recreate_fac_tr(fac_tr_file, fac_tr_new_file, old_level_to_energy)
