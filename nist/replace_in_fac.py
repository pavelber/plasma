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

        configs[(c1, c2, j)] = eV
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
    energy = float(eV[1 : l - 1])
    return "%.8E" % energy


def recreate_fac_lev(old_lev, new_lev, levels):
    for line in old_lev:
        data = line.split()
        if len(data) < 9:
            new_lev.write(line)
        else:
            config = extract_config_and_j(data)

            if config in levels:
                eV = levels[config]
                energy_str = format_energy_for_fac(eV)
                new_lev.write(line.replace(data[2],energy_str))
            else:
                new_lev.write(line)


if len(sys.argv) < 2:
    error('\nUsage: ' + sys.argv[0] + ' file-with-nist-levels-csv directory-with-fac')

nist_file = os.path.abspath(sys.argv[1])
fac_dir = os.path.abspath(sys.argv[2])
fac_lev = os.path.join(fac_dir, "fac.lev")
fac_lev_new = os.path.join(fac_dir, "fac-new.lev")

if not os.path.isdir(fac_dir) or not os.path.exists(fac_dir):
    error(fac_dir + " does not exists or is not a directory")

pass
if os.path.isdir(fac_lev) or not os.path.exists(fac_lev):
    pass

if os.path.isdir(nist_file) or not os.path.exists(nist_file):
    error(nist_file + " does not exists or is a directory")

with open(nist_file, 'rb') as levels_file:
    levels = read_nist(levels_file)

with open(fac_lev, 'rb') as fac_lev_file:
    with open(fac_lev_new, 'wb') as fac_lev_new_file:
        recreate_fac_lev(fac_lev_file, fac_lev_new_file, levels)
