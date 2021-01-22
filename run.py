import os
import sys

from env import env
from utils import error, copy_and_run
from utils import runcommand


def check_dirs(i_dir, o_dir):
    print("Input directory: " + i_dir)
    if not os.path.isdir(i_dir) or not os.path.exists(in_dir):
        error(i_dir + " does not exists or is not a directory")
    i_spectro = sorted(filter(lambda f: f.isdigit(), os.listdir(i_dir)))
    print("Spectroscopic numbers in " + i_dir + ": " + str(i_spectro))
    print("Output directory: " + o_dir)
    if not os.path.exists(o_dir):
        os.makedirs(o_dir)
    if len(os.listdir(o_dir)) > 0:
        error("Directory " + o_dir + " should not exist or be empty")


def run_for_one_number(spn, in_dir_spn, out_dir_spn):
    run_old_fac(in_dir_spn, out_dir_spn)
    run_fit(spn, out_dir_spn)
    run_ph_fac(spn, out_dir_spn)
    run_exc_fac(spn, out_dir_spn)


def run_ph_fac(spn, out_dir_spn):
    rec_dir = out_dir_spn + os.path.sep + "REC"
    code, std_out, std_err = copy_and_run(ph_fac_path, "", rec_dir, out_dir_spn, spn)
    print(std_out + " " + std_out)


def run_exc_fac(spn, out_dir_spn):
    exc_dir = out_dir_spn + os.path.sep + "EXC"
    code, std_out, std_err = copy_and_run(exc_fac_path, "", exc_dir, out_dir_spn, spn)
    print(std_out + " " + std_out)


def run_fit(spn, out_dir_spn):
    fac_dir = out_dir_spn
    code, std_out, std_err = copy_and_run(fit_path, perl_path, fac_dir, out_dir_spn, spn)
    print(std_out + " " + std_out)


def run_old_fac(in_dir_spn, out_dir_spn):
    cmd = python_path + " " + old_path + " " + in_dir_spn + " " + out_dir_spn
    print(cmd)
    code, std_out, std_err = runcommand(cmd)
    print(std_out + " " + std_out)


def run_for_all_numbers():
    i_spectro = sorted(filter(lambda f: f.isdigit(), os.listdir(in_dir)))
    for spn in i_spectro:
        in_dir_spn = in_dir + os.path.sep + spn
        out_dir_spn = out_dir + os.path.sep + spn
        run_for_one_number(spn, in_dir_spn, out_dir_spn)


################## MAIN ######################
if len(sys.argv) < 3:
    error('\nUsage: ' + sys.argv[
        0] + 'directory-with-cFAC-1.6.3-files-per-spectroscopic-charge output-directory [path-to-perl-executable]')

in_dir = sys.argv[1]
out_dir = sys.argv[2]

if len(sys.argv) > 4:
    perl_exe = sys.argv[3]
else:
    perl_exe = None

python_path, perl_path, old_path, fit_path, exc_fac_path, ph_fac_path = env(perl_exe)
check_dirs(in_dir, out_dir)

run_for_all_numbers()
