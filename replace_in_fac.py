import copy
import os
import sys
import urllib
from os import listdir

from lib.get_ionization_energy import get_ionization_energy_ev
from lib.levels_string import create_levels_string
from lib.utils import error, nist_strip, dec_to_roman


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
            headers = levels_file.readline().strip().split(',')
            eVColumn = headers.index("Level (eV)")
            for line in levels_file:
                data = line.split(',')
                config = filter(lambda l: '(' not in l, nist_strip(data[0]).split('.'))
                if nist_strip(data[2]) == '---':
                    break
                j = compute_2j(nist_strip(data[2]))
                eV = nist_strip(data[eVColumn])
                if len(config) < 2:
                    c1 = config[0]
                    c2 = ''
                else:
                    c1 = config[-2]
                    c2 = config[-1]

                if len(c1) > 0 and c1[-1].isalpha():
                    c1 = c1 + '1'
                if len(c2) > 0 and c2[-1].isalpha():
                    c2 = c2 + '1'
                if (c1, c2, j) not in configs:
                    configs[(c1, c2, j)] = []
                if eV != '' and eV != '[':
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

    energy = eV
    if l == 0:
        energy = '0.000'
    else:
        if energy[0] == '[' or energy[0] == '(':
            energy = energy[1: l - 1]
        if '+x' in energy:
            energy = energy.replace('+x', '')
        if '?' in energy:
            energy = energy.replace('?', '')

    return energy


def renumerate(energy_levels):
    i = 0
    old_level_to_new_level_list = []
    old_level_to_new_level = {}
    old_level_to_new_level_list.append(old_level_to_new_level)
    energies = sorted(energy_levels, key=lambda x: float(x[0]))
    for e in energies:
        old_level_to_new_level[e[1]] = str(i)
        i = i + 1
    return old_level_to_new_level


def no_4_in_config(config, max_n):
    return len(config[0]) > 0 and int(config[0][0]) < max_n and (
            len(config[1]) == 0 or int(config[1][0]) < max_n)


def recreate_fac_lev(old, new, nist_level_to_energy, max_n, element, num):
    nele_counter = -1
    energy_levels = []
    ionization_energy = get_ionization_energy_ev(element, num)
    for line in old:
        data = line.split()
        if line.startswith("NELE"):
            nele_counter = nele_counter + 1
            current_levels = copy.deepcopy(nist_level_to_energy[nele_counter])
            electrons = int(data[2])
        if len(data) < 9:
            new.write(line)
        else:
            config = extract_config_and_j(data)

            level_num = data[0]
            energy = data[2]
            levels_adjusted = create_levels_string(electrons, line).split()
            if len(levels_adjusted) > 1:
                levels_config = (levels_adjusted[-2], levels_adjusted[-1], config[2])
            else:
                levels_config = (levels_adjusted[-1], '', config[2])
            if levels_config in current_levels:
                print(levels_config)
            if levels_config in current_levels and no_4_in_config(levels_config, max_n) and len(
                    current_levels[levels_config]) > 0:
                eV = current_levels[levels_config].pop(0)
                float_energy = float(clean_energy_for_fac(eV))
                if nele_counter == 1:
                    float_energy = float_energy + ionization_energy
                energy_str = "%.8E" % float_energy
                new.write(line.replace(energy, energy_str))
            else:
                new.write(line)
                energy_str = energy

            energy_levels.append((energy_str, level_num))

    return energy_levels


def create_fn(fac_lev_file, fn_file):
    nele_counter = 0
    second_section = False
    for line in fac_lev_file:
        data = line.split()
        if line.startswith("NELE"):
            nele_counter = nele_counter + 1
        if nele_counter == 2:
            second_section = True
        elif len(data) >= 9 and not second_section:
            conf = line[85:-1]
            energy = float(data[2])
            fn_file.write("%-40s  %4.4f\n" % (conf + ",", energy))


def renumerate_fac_lev(old, new, old_to_new_level):
    nele_counter = 0

    lines = []
    i = -1
    for line in old:
        data = line.split()
        if line.startswith("NELE"):
            new.write(line)
            nele_counter = nele_counter + 1
        elif len(data) < 9:
            i = i + 1
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


def extract_element(fac_nums_dir):
    any_num = listdir(fac_nums_dir)[0]
    any_fac_lev_name = os.path.join(fac_nums_dir, any_num, "fac.lev")
    with open(any_fac_lev_name, 'rb') as fac_lev_file:
        for i in range(0, 5):
            fac_lev_file.readline()
        el = fac_lev_file.readline().split()[0]
    return el


def download_nist(elemnt, spec_nums, nist_dir_name):
    for n in spec_nums:
        num = dec_to_roman(int(n))
        url = "https://physics.nist.gov/cgi-bin/ASD/energy1.pl?de=0&spectrum=" + elemnt + "+" + num + "&units=1&format=2&output=0&page_size=100&multiplet_ordered=0&conf_out=on&term_out=on&level_out=on&unc_out=1&j_out=on&lande_out=on&perc_out=on&biblio=on&temp=&submit=Retrieve+Data"
        testfile = urllib.URLopener()
        testfile.retrieve(url, os.path.join(nist_dir_name, n + ".csv"))


################################# MAIN ################################################################

if len(sys.argv) < 3:
    error('\nUsage: ' + sys.argv[0] + ' directory-with-fac output-directory')

fac_nums_dir = os.path.abspath(sys.argv[1])
fac_nums_out_dir = os.path.abspath(sys.argv[2])
max_n = 4
if len(sys.argv) >= 4:
    max_n = int(sys.argv[3])

elemnt = extract_element(fac_nums_dir)

spec_nums = listdir(fac_nums_dir)

nist_dir_name = os.path.join(fac_nums_out_dir, "nist-" + elemnt)

if not os.path.exists(nist_dir_name):
    os.mkdir(nist_dir_name)
    download_nist(elemnt, spec_nums, nist_dir_name)

nist_levels_per_num = read_nist(nist_dir_name)
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

    next_sp = str(int(num) + 1)
    if next_sp in nist_levels_per_num:
        next_sp_nist = nist_levels_per_num[next_sp]
    else:
        next_sp_nist = {}
    levels = [nist_levels_per_num[num], next_sp_nist]

    with open(fac_lev, 'rb') as fac_lev_file:
        with open(fac_lev_tmp, 'wb') as fac_lev_tmp_file:
            old_energy_to_levels[num] = recreate_fac_lev(fac_lev_file, fac_lev_tmp_file, levels, max_n, elemnt,
                                                         int(num))

    old_2_new[num] = renumerate(old_energy_to_levels[num])

for fac_dir_name in listdir(fac_nums_dir):
    num = fac_dir_name
    next_num = str(int(num) + 1)
    fac_dir = os.path.join(fac_nums_dir, fac_dir_name)
    fac_lev_tmp = os.path.join(fac_dir, "fac-tmp.lev")
    fac_lev_new = os.path.join(fac_dir, "fac-new.lev")
    fac_out_dir = os.path.join(fac_nums_out_dir, fac_dir_name)
    fac_out_lev = os.path.join(fac_out_dir, "fac.lev")
    fn = os.path.join(fac_out_dir, "fn.corr")
    with open(fac_lev_tmp, 'rb') as fac_lev_tmp_file:
        with open(fac_out_lev, 'wb') as fac_lev_new_file:
            renumerate_fac_lev(fac_lev_tmp_file, fac_lev_new_file, old_2_new[num])

    with open(fac_out_lev, 'rb') as fac_lev_file:
        with open(fn, 'wb') as fn_file:
            create_fn(fac_lev_file, fn_file)
