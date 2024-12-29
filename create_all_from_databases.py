import os
import shutil
import sys
from os.path import dirname, abspath

from lib.check_and_fix import copy_checks, check_and_fix_old_rr_version2, check_and_fix_rr_version2, check_fix, \
    create_rrec_inp
from lib.create_in1_from_databases import create_in1_excit_spectr__from_databases, parse_energy_limits
from lib.create_rrec_bcfp_from_in1 import create_rrec_from_in1, create_bcfp_from_in1
from lib.env import get_pathes, env
from lib.remove_lines_and_renumenrate import remove_unused_lines_and_renumerate
from lib.update_fits import create_new_fits_for_rrec2
from lib.utils import error, read_table, invert_replaces
from lib.verify_results import test_number_of_levels_inp1, files_not_empty

################## MAIN ######################

if len(sys.argv) < 7:
    error('\nUsage: ' + sys.argv[
        0] + ' out-dir element-name nmax energy-limits min_sp_num max_sp_num')

param_num = 1
out_dir = os.path.abspath(sys.argv[param_num])
param_num += 1
elem = sys.argv[param_num]
param_num += 1
nmax = sys.argv[param_num]
param_num += 1
energy_limits = parse_energy_limits(sys.argv[param_num])
param_num += 1
min_sp_num = int(sys.argv[param_num])
param_num += 1
max_sp_num = int(sys.argv[param_num])
param_num += 1
formula = sys.argv[param_num].lower()=='formula'
param_num += 1

(name_to_table, num_to_table) = read_table()

env_errors = env()
if env_errors is not None:
    error(env_errors)

old_path, fit_path, exc_fac_path, ph_fac_path, my_dir = get_pathes()

if not os.path.exists(out_dir):
    os.makedirs(out_dir)

elem_dir = os.path.join(out_dir, elem)

if not os.path.exists(elem_dir):
    os.makedirs(elem_dir)
my_dir = dirname(abspath(__file__))
levels_downloaded = os.path.join(my_dir, "db", elem, "levels")

lines_downloaded = os.path.join(my_dir, "db", elem, "lines")

levels_dir = os.path.join(elem_dir, "levels")
if os.path.exists(levels_dir):
    shutil.rmtree(levels_dir)
lines_dir = os.path.join(elem_dir, "lines")

if os.path.exists(lines_dir):
    shutil.rmtree(lines_dir)
shutil.copytree(levels_downloaded, levels_dir)
shutil.copytree(lines_downloaded, lines_dir)

(name_to_table, num_to_table) = read_table()

nucleus = int(name_to_table[elem]["AtomicNumber"]) + 1
sp_nums_dec = list(range(min_sp_num, max_sp_num + 1))
sp_nums_str = list(map(lambda x: str(x), range(min_sp_num, max_sp_num + 1)))
sp_nums_with_nucleus = sp_nums_dec[:]

if max_sp_num + 1 == nucleus:
    sp_nums_with_nucleus.append(nucleus)

create_in1_excit_spectr__from_databases(elem_dir, elem, nucleus, sp_nums_dec,
                                        energy_limits,
                                        nmax)
in1 = os.path.join(elem_dir, "IN1.INP")
create_rrec_from_in1(in1, elem, elem_dir, sp_nums_with_nucleus, nucleus, formula)
create_bcfp_from_in1(in1, elem_dir, sp_nums_with_nucleus, nucleus)

create_rrec_inp(elem_dir, ph_fac_path, sp_nums_with_nucleus)

dir_bad = {}

for sp in sp_nums_with_nucleus:
    sp_path = os.path.join(elem_dir, str(sp))
    rrec_path = os.path.join(sp_path, "RREC.INP")
    check_fix(sp_path, my_dir, rrec_path, dir_bad, sp)
rrec_path = os.path.join(elem_dir, "RREC.INP")
excit_path = os.path.join(elem_dir, "EXCIT.INP")
spectr_path = os.path.join(elem_dir, "SPECTR.INP")

bcfp_path = os.path.join(elem_dir, "BFCP.INP")

with open(rrec_path, "w") as rrec:
    for sp in sp_nums_with_nucleus:
        sp_path = os.path.join(elem_dir, str(sp))
        rrec_sp = os.path.join(sp_path, "RREC.INP")
        with open(rrec_sp, "r") as sp_rrec:
            for line in sp_rrec:
                rrec.write(line)

copy_checks(my_dir, elem_dir)
check_and_fix_rr_version2(elem_dir)
check_and_fix_old_rr_version2(elem_dir)

for sp in sp_nums_with_nucleus:
    print("**********************")
    print(sp)
    print("**********************")
    if sp in dir_bad:
        lines = dir_bad[sp]
        for l in lines:
            print(l)

replaces = remove_unused_lines_and_renumerate(elem_dir, nucleus)

from_new_to_old = invert_replaces(replaces)
create_new_fits_for_rrec2(elem_dir, 'powell', from_new_to_old, "RREC-fits.INP", "RREC.INP")
create_new_fits_for_rrec2(elem_dir, 'powell', from_new_to_old, "RREC-fits-2.INP", "RREC-fits.INP", True)
test_number_of_levels_inp1(in1)
files_not_empty(elem_dir)

# removed = remove_large(rrec_path, 0, [4, 5], 1.0e-4)
# print("Removed " + str(removed) + "from " + rrec_path)
# file_name = os.path.join(elem_dir, "RREC-fits.INP")
# removed = remove_large(file_name, 0, [4, 5], 1.0e-4)
# print("Removed " + str(removed) + "from " + file_name)
