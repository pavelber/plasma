import os
import sys
from os.path import dirname, abspath

from lib.check_and_fix import run_check_old_rr, run_fix_old_rr, run_fix_rr, run_check_rr, copy_checks
from lib.create_in1_from_databases import create_in1_from_databases, parse_energy_limits
from lib.create_rrec_from_in1 import create_rrec_from_in1
from lib.env import env
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

        code, std_out, std_err = runcommand("sort  output_ph.dat>RREC.INP", dir)
        print(std_out + " " + std_out)
        if code != 0:
            error("Exit code = " + str(code))


def check_and_fix_rr(dir):
    print("Fixing RREC in " + dir + "\n")
    max_iter = 100
    i = 0
    bad = run_check_rr(dir)
    while bad > 0 and i < max_iter:
        run_fix_rr(dir)
        bad = run_check_rr(dir)
        i = i + 1


def check_and_fix_old_rr(dir):
    print("Fixing RREC in " + dir + "\n")
    max_iter = 100
    i = 0
    bad = run_check_old_rr(dir)
    while bad > 0 and i < max_iter:
        run_fix_old_rr(dir)
        bad = run_check_old_rr(dir)
        i = i + 1


################## MAIN ######################
if len(sys.argv) < 3:
    error('\nUsage: ' + sys.argv[
        0] + ' out-dir element-name energy-limits')

out_dir = os.path.abspath(sys.argv[1])
elem = sys.argv[2]

energy_limits = parse_energy_limits(sys.argv[3])

download = True
if len(sys.argv) == 5 and sys.argv[4] == 'False':
    download = False

if not os.path.exists(out_dir):
    os.makedirs(out_dir)

elem_dir = os.path.join(out_dir, elem)

if not os.path.exists(elem_dir):
    os.makedirs(elem_dir)

sp_nums = create_in1_from_databases(elem_dir, elem, energy_limits, download)
create_rrec_from_in1(os.path.join(elem_dir, "IN1.INP"), elem_dir, sp_nums)
create_rrec_inp(elem_dir)

with open(os.path.join(elem_dir, "RREC.INP"), "w") as rrec:
    for sp in sp_nums:
        with open(os.path.join(elem_dir, str(sp), "RREC.INP"), "r") as sp_rrec:
            for line in sp_rrec:
                rrec.write(line)

my_dir = dirname(abspath(__file__))
copy_checks(my_dir, elem_dir)

check_and_fix_rr(elem_dir)
check_and_fix_old_rr(elem_dir)
