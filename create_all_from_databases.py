import os
import shutil
import sys
from os.path import dirname, abspath

from lib.check_and_fix import run_check_old_rr, run_fix_old_rr, copy_checks, run_new_fix, \
    run_check_rr_return_lines
from lib.create_in1_from_databases import create_in1_from_databases, parse_energy_limits
from lib.create_rrec_from_in1 import create_rrec_from_in1
from lib.download_parse_pa_uky import download_piter
from lib.env import env
from lib.nist import download_nist_for_in1
from lib.utils import error, runcommand


def create_rrec_inp(elem_dir):
    my_dir = dirname(abspath(__file__))

    python_path, perl_path, old_path, fit_path, exc_fac_path, ph_fac_path, qsege_path, wc_path, fac_in1_path, my_dir = env(
        "perl")

    i_spectro = sorted(filter(lambda f: f.isdigit(), os.listdir(elem_dir)))
    for sn in i_spectro:
        dir = os.path.join(elem_dir, sn)
        code, std_out, std_err = runcommand(ph_fac_path, dir, sn)
        print(std_out + " " + std_out)
        if code != 0:
            error("Exit code = " + str(code))

        print("Running " + "sort  output_ph.dat>RREC.INP" + " in " + dir)
        code, std_out, std_err = runcommand("sort  output_ph.dat>RREC.INP", dir)
        print(std_out + " " + std_out)
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


################## MAIN ######################
if len(sys.argv) < 3:
    error('\nUsage: ' + sys.argv[
        0] + ' out-dir element-name energy-limits')

out_dir = os.path.abspath(sys.argv[1])
elem = sys.argv[2]

energy_limits = parse_energy_limits(sys.argv[3])

download = False
if len(sys.argv) == 5 and sys.argv[4] == 'True':
    download = True

if not os.path.exists(out_dir):
    os.makedirs(out_dir)

elem_dir = os.path.join(out_dir, elem)

if not os.path.exists(elem_dir):
    os.makedirs(elem_dir)

if download:
    download_nist_for_in1(elem, out_dir)
    download_piter(elem, out_dir)

my_dir = dirname(abspath(__file__))

nist = os.path.join(elem_dir, "NIST")
if os.path.exists(nist):
    shutil.rmtree(nist)
piter = os.path.join(elem_dir, "piter")
if os.path.exists(piter):
    shutil.rmtree(piter)

shutil.copytree(os.path.join(my_dir, "db", elem, "NIST"), nist)
shutil.copytree(os.path.join(my_dir, "db", elem, "piter"), piter)

sp_nums = create_in1_from_databases(elem_dir, elem, energy_limits)
create_rrec_from_in1(os.path.join(elem_dir, "IN1.INP"), elem_dir, sp_nums)
create_rrec_inp(elem_dir)

with open(os.path.join(elem_dir, "RREC.INP"), "w") as rrec:
    dir_bad = {}
    for sp in sp_nums:
        sp_path = os.path.join(elem_dir, str(sp))
        rrec_path = os.path.join(sp_path, "RREC.INP")
        backup_rrec_name = os.path.join(sp_path, "RREC.INP.BACK")
        shutil.copyfile(rrec_path, backup_rrec_name)

        if os.path.getsize(rrec_path) > 0:
            copy_checks(my_dir, sp_path)
            bad = check_and_fix_rr(sp_path)
            # check_and_fix_old_rr(sp_path)
            dir_bad[sp] = bad
            with open(rrec_path, "r") as sp_rrec:
                for line in sp_rrec:
                    rrec.write(line)
for sp in sp_nums:
    print("**********************")
    print(sp)
    print("**********************")
    if sp in dir_bad:
        lines = dir_bad[sp]
        for l in lines:
            print(l)

# copy_checks(my_dir, elem_dir)
# check_and_fix_rr(elem_dir)
# check_and_fix_old_rr(elem_dir)
