import os
import shutil
import sys
from os.path import dirname, abspath

from lib.check_and_fix import run_check_old_rr, run_fix_old_rr, copy_checks, run_new_fix, \
    run_check_rr_return_lines
from lib.create_in1_from_databases import create_in1_from_databases, parse_energy_limits
from lib.create_rrec_bcfp_from_in1 import create_rrec_bcfp_from_in1
from lib.download_parse_pa_uky_levels import download_piter_levels
from lib.download_parse_pa_uky_lines import download_piter_lines
from lib.env import env
from lib.remove_lines_and_renumenrate import remove_unused_lines_and_renumerate, remove_large
from lib.update_fits import create_new_fits_for_rrec2
from lib.utils import error, runcommand
from lib.verify_results import test_number_of_levels_inp1, files_not_empty


def create_rrec_inp(elem_dir):
    i_spectro = sorted(filter(lambda f: f.isdigit(), os.listdir(elem_dir)))
    for sn in i_spectro:
        rrec_dir = os.path.join(elem_dir, sn)
        create_rrec_inp_from_dir(rrec_dir, ph_fac_path, sn)


def create_rrec_inp_from_dir(rrec_dir, ph_fac_path, sn):
    code, std_out, std_err = runcommand(ph_fac_path, rrec_dir, sn)
    print(std_out + " " + std_err)
    if code != 0:
        error("Exit code = " + str(code))
    print("Running " + "type output_ph.dat|sort>RREC.INP" + " in " + rrec_dir)
    code, std_out, std_err = runcommand("type output_ph.dat|sort>RREC.INP", rrec_dir)
    print(std_out + " " + std_err)
    if code != 0:
        error("Exit code = " + str(code))


def check_and_fix_rr(dir):
    print("Fixing RREC in " + dir + "\n")
    max_iter = 10
    i = 0
    good_checks = 0
    bad = 0
    while i < max_iter:
        bad_lines = run_check_rr_return_lines(dir)
        bad = len(bad_lines)
        if bad == 0:
            good_checks = good_checks + 1
        else:
            # run_fix_rr(dir)
            run_new_fix(dir, bad_lines, False)
        i = i + 1
    run_new_fix(dir, bad_lines, True)
    return bad_lines


def check_and_fix_old_rr(dir):
    print("Fixing RREC in " + dir + "\n")
    max_iter = 100
    i = 0
    good_checks = 0
    while good_checks < 10 and i < max_iter:
        bad = run_check_old_rr(dir)
        if bad == 0:
            good_checks = good_checks + 1
        else:
            run_fix_old_rr(dir)
        i = i + 1


def invert_replaces(replaces):
    new_to_old = {}
    for sp_num in replaces:
        n_o = {}
        per_sp_num = replaces[sp_num]
        for old in per_sp_num:
            n_o[per_sp_num[old]] = old
        new_to_old[sp_num] = n_o
    return new_to_old


def check_fix(rrec_path):
    rrec_path = os.path.join(sp_path, "RREC.INP")
    backup_rrec_name = os.path.join(sp_path, "RREC.INP.BACK")
    shutil.copyfile(rrec_path, backup_rrec_name)
    if os.path.getsize(rrec_path) > 0:
        copy_checks(my_dir, sp_path)
        bad = check_and_fix_rr(sp_path)
        # check_and_fix_old_rr(sp_path)
        dir_bad[sp] = bad
        if len(bad) > 1:
            bad = check_and_fix_rr(sp_path)
            if len(bad) > 0:
                error("Still have bad " + str(bad))


################## MAIN ######################

if len(sys.argv) < 5:
    error('\nUsage: ' + sys.argv[
        0] + ' out-dir element-name nmax osc energy-limits ')
out_dir = os.path.abspath(sys.argv[1])

elem = sys.argv[2]
nmax = sys.argv[3]
osc = sys.argv[4]
energy_limits = parse_energy_limits(sys.argv[5])

python_path, perl_path, old_path, fit_path, exc_fac_path, ph_fac_path, qsege_path, wc_path, fac_in1_path, my_dir = env(
    "perl")
download = False

if len(sys.argv) == 7 and sys.argv[6] == 'True':
    download = True

if not os.path.exists(out_dir):
    os.makedirs(out_dir)

elem_dir = os.path.join(out_dir, elem)

if not os.path.exists(elem_dir):
    os.makedirs(elem_dir)
my_dir = dirname(abspath(__file__))
levels_downloaded = os.path.join(my_dir, "db", elem, "levels")

lines_downloaded = os.path.join(my_dir, "db", elem, "lines")

if download:
    download_piter_levels(elem, levels_downloaded, nmax)
    download_piter_lines(elem, lines_downloaded, nmax, osc)
levels_dir = os.path.join(elem_dir, "levels")
if os.path.exists(levels_dir):
    shutil.rmtree(levels_dir)
lines_dir = os.path.join(elem_dir, "lines")

if os.path.exists(lines_dir):
    shutil.rmtree(lines_dir)
shutil.copytree(levels_downloaded, levels_dir)
shutil.copytree(lines_downloaded, lines_dir)

sp_nums = create_in1_from_databases(elem_dir, elem, energy_limits)
in1 = os.path.join(elem_dir, "IN1.INP")
create_rrec_bcfp_from_in1(in1, elem_dir, sp_nums)

create_rrec_inp(elem_dir)

dir_bad = {}

for sp in sp_nums:
    sp_path = os.path.join(elem_dir, str(sp))
    rrec_path = os.path.join(sp_path, "RREC.INP")
    check_fix(rrec_path)
rrec_path = os.path.join(elem_dir, "RREC.INP")
excit_path = os.path.join(elem_dir, "EXCIT.INP")
spectr_path = os.path.join(elem_dir, "SPECTR.INP")

bcfp_path = os.path.join(elem_dir, "BFCP.INP")

with open(rrec_path, "w") as rrec:
    for sp in sp_nums:
        sp_path = os.path.join(elem_dir, str(sp))
        rrec_sp = os.path.join(sp_path, "RREC.INP")
        with open(rrec_sp, "r") as sp_rrec:
            for line in sp_rrec:
                rrec.write(line)

copy_checks(my_dir, elem_dir)
check_and_fix_rr(elem_dir)
check_and_fix_old_rr(elem_dir)

for sp in sp_nums:
    print("**********************")
    print(sp)
    print("**********************")
    if sp in dir_bad:
        lines = dir_bad[sp]
        for l in lines:
            print(l)

replaces = remove_unused_lines_and_renumerate(elem_dir)

from_new_to_old = invert_replaces(replaces)
create_new_fits_for_rrec2(elem_dir, 'powell', from_new_to_old, "RREC-fits.INP", "RREC.INP")
create_new_fits_for_rrec2(elem_dir, 'powell', from_new_to_old, "RREC-fits-2.INP", "RREC-fits.INP")
test_number_of_levels_inp1(in1)
files_not_empty(elem_dir)

# removed = remove_large(rrec_path, 0, [4, 5], 1.0e-4)
# print("Removed " + str(removed) + "from " + rrec_path)
# file_name = os.path.join(elem_dir, "RREC-fits.INP")
# removed = remove_large(file_name, 0, [4, 5], 1.0e-4)
# print("Removed " + str(removed) + "from " + file_name)
