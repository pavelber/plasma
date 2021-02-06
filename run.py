import os
import sys

from lib.env import env
from lib.files_union import create_bcfp, create_excit, create_rrec
from lib.inp1 import create_inp
from lib.renumer import create_tables
from lib.spect import create_spectr
from lib.utils import error, copy_and_run
from lib.utils import runcommand


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


def read_nlev(fac_lev):
    if os.path.exists(fac_lev):
        with open(fac_lev, 'rb') as inf:
            for line in inf:
                columns = line.split()
                if len(columns) == 3 and columns[0] == 'NLEV':
                    return columns[2]
            error("NLEV not found in " + fac_lev)
    else:
        error(fac_lev + " does not exist!")


def run_for_one_number(spn, in_dir_spn, out_dir_spn):
    run_old_fac(in_dir_spn, out_dir_spn)
    levels = read_nlev(out_dir_spn + os.path.sep + "fac.lev")
    print("Levels from fac.lev: " + levels)
    run_fit(spn, levels, out_dir_spn)
    run_ph_fac(spn, out_dir_spn)
    run_exc_fac(spn, out_dir_spn)


def run_ph_fac(spn, out_dir_spn):
    rec_dir = out_dir_spn + os.path.sep + "REC"
    code, std_out, std_err = copy_and_run(ph_fac_path, "", rec_dir, rec_dir, spn)
    print(std_out + " " + std_out)


def run_exc_fac(spn, out_dir_spn):
    exc_dir = out_dir_spn + os.path.sep + "EXC"
    code, std_out, std_err = copy_and_run(exc_fac_path, "", exc_dir, exc_dir, spn)
    print(std_out + " " + std_out)


def run_fit(spn, levels, out_dir_spn):
    fac_dir = out_dir_spn
    code, std_out, std_err = copy_and_run(fit_path, perl_path, fac_dir, out_dir_spn, spn, " -t " + levels)
    print(std_out + " " + std_out)


def run_old_fac(in_dir_spn, out_dir_spn):
    cmd = python_path + " " + old_path + " " + in_dir_spn + " " + out_dir_spn
    print(cmd)
    code, std_out, std_err = runcommand(cmd)
    print(std_out + " " + std_out)


def run_for_all_numbers():
    i_spectro = map(lambda x: str(x), sorted(map(lambda x: int(x), filter(lambda f: f.isdigit(), os.listdir(in_dir)))))
    for spn in i_spectro:
        in_dir_spn = in_dir + os.path.sep + spn
        out_dir_spn = out_dir + os.path.sep + spn
        if not dont_run_all_tools:
            run_for_one_number(spn, in_dir_spn, out_dir_spn)
    return i_spectro


################## MAIN ######################
if len(sys.argv) < 3:
    error('\nUsage: ' + sys.argv[
        0] + 'directory-with-cFAC-1.6.3-files-per-spectroscopic-charge output-directory min_eins_coeff [run exe]')

in_dir = os.path.abspath(sys.argv[1])
out_dir = os.path.abspath(sys.argv[2])
min_eins_coef = float(sys.argv[3])
dont_run_all_tools = len(sys.argv) > 4 and sys.argv[4] == "false"

if len(sys.argv) > 5:
    perl_exe = sys.argv[5]
else:
    perl_exe = None

python_path, perl_path, old_path, fit_path, exc_fac_path, ph_fac_path = env(perl_exe)
if not dont_run_all_tools:
    check_dirs(in_dir, out_dir)

spec_numbers = run_for_all_numbers()
ionization_potential, translation_table = create_tables(out_dir)
next_spec_number = str(int(spec_numbers[len(spec_numbers) - 1]) + 1)
translation_table[next_spec_number] = {"1": "1"}
create_bcfp(out_dir, spec_numbers, translation_table)
create_excit(out_dir, spec_numbers, translation_table)
create_rrec(out_dir, spec_numbers, translation_table)
create_inp(out_dir, spec_numbers, translation_table, ionization_potential)
create_spectr(out_dir, spec_numbers, translation_table, ionization_potential, min_eins_coef)
