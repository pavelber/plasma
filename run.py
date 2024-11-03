import os
import shutil
import sys
from os.path import isdir

from lib.check_and_fix import check_and_fix_rr, check_and_fix_old_rr, copy_checks
from lib.create_aiw import create_aiw
from lib.create_files_union import create_bcfp, create_excit, create_rrec
from lib.create_inp1 import create_inp
from lib.create_spect import create_spectr
from lib.env import env, get_pathes
from lib.exceptions import GenericPlasmaException
from lib.fisher import run_for_fisher
from lib.process_mz import replace_from_mz
from lib.renumer import create_tables
from lib.utils import error, copy_and_run, runcommand_print
from lib.utils import runcommand

MAX_LINES = 80000


def check_dirs(in_dir, o_dir):
    print("Input directory: " + in_dir)
    if not os.path.isdir(in_dir) or not os.path.exists(in_dir):
        error(in_dir + " does not exists or is not a directory")
    i_spectro = sorted(filter(lambda f: f.isdigit(), os.listdir(in_dir)))
    print("Spectroscopic numbers in " + in_dir + ": " + str(i_spectro))
    print("Output directory: " + o_dir)
    if not os.path.exists(o_dir):
        os.makedirs(o_dir)
    if len(os.listdir(o_dir)) > 0:
        raise GenericPlasmaException("Directory " + o_dir + " should not exist or be empty")


def read_nlev(fac_lev):
    if os.path.exists(fac_lev):
        with open(fac_lev, 'r') as inf:
            for line in inf:
                columns = line.split()
                if len(columns) == 3 and columns[0] == 'NLEV':
                    return columns[2]
            error("NLEV not found in " + fac_lev)
    else:
        error(fac_lev + " does not exist!")


def run_for_one_number(spn, in_dir_spn, out_dir_spn, old_path, exc_fac_path, ph_fac_path):
    run_old_fac(in_dir_spn, out_dir_spn, old_path)
    levels = read_nlev(out_dir_spn + os.path.sep + "fac.lev")
    print("Levels from fac.lev: " + levels)

    run_fac_in1(spn, out_dir_spn, exc_fac_path, ph_fac_path)
    # run_fit(spn, levels, out_dir_spn)


def split_and_run(run_dir, max_files, exe_path, files_list_file, merge_file, spn, wc_path):
    count = 0
    should_create_new = True
    new_list_file = None

    line_count = 0
    orig_file = os.path.join(run_dir, files_list_file)
    bak_file = os.path.join(run_dir, files_list_file + ".bak")
    shutil.move(orig_file, bak_file)

    with open(bak_file, 'r') as inf:
        for line in inf:
            if should_create_new:
                if new_list_file:
                    close_and_run(count, exe_path, files_list_file, merge_file, new_list_file, run_dir, spn, wc_path)
                print("SPLIT: opening " + files_list_file)
                new_list_file = open(orig_file, 'w')
                count += 1
            new_list_file.write(line)
            line_count += 1
            if line_count % max_files == 0:
                should_create_new = True
            else:
                should_create_new = False

    close_and_run(count, exe_path, files_list_file, merge_file, new_list_file, run_dir, spn, wc_path)
    code, std_out, std_err = runcommand("cat " + merge_file + ".* >" + merge_file, run_dir)
    print(std_out + " " + std_err)


def close_and_run(count, exe_path, files_list_file, merge_file, new_list_file, run_dir, spn, wc_path):
    new_list_file.close()
    print("SPLIT: closing " + files_list_file)
    code, std_out, std_err = copy_and_run(exe_path, "", run_dir, run_dir, spn)
    print(std_out + " " + std_err)
    code, std_out, std_err = runcommand(wc_path + " -l *.dat", run_dir)
    print("SPLIT wc.exe " + std_out + " " + std_err)
    merge_file_path = os.path.join(run_dir, merge_file)
    print("SPLIT: move " + merge_file_path + " to " + merge_file_path + "." + str(count))
    shutil.move(merge_file_path, merge_file_path + "." + str(count))


def copy_ph_fac(out_dir_spn, ph_fac_path):
    rec_dir = out_dir_spn + os.path.sep + "REC"
    os.mkdir(rec_dir)

    shutil.copy(ph_fac_path, rec_dir)


def copy_exc_fac(out_dir_spn, exc_fac_path):
    exc_dir = out_dir_spn + os.path.sep + "EXC"
    os.mkdir(exc_dir)
    shutil.copy(exc_fac_path, exc_dir)


def run_fit(spn, levels, out_dir_spn, fit_path):
    fac_dir = out_dir_spn
    code, std_out, std_err = copy_and_run(fit_path, "perl", fac_dir, out_dir_spn, spn, " -t " + levels)
    print(std_out + " " + std_out)
    if code != 0:
        error("Exit code = " + str(code))


def run_fac_in1(spn, out_dir_spn, exc_fac_path, ph_fac_path):
    fac_dir = out_dir_spn
    code, std_out, std_err = copy_and_run("fac_IN1.pl", "perl", fac_dir, out_dir_spn, spn,
                                          "-exc " + exc_fac_path + " -ph " + ph_fac_path)
    print(std_out + " " + std_out)
    if code != 0:
        error("Exit code = " + str(code))


def check_and_fix(my_dir, out_dir):
    print("check_and_fix")
    copy_checks(my_dir, out_dir)
    print("start check all in " + out_dir)
    code, std_out, std_err = runcommand_print("perl check_all.pl -d", out_dir)
    if code != 0:
        error("Exit code = " + str(code))

    # for spn in os.listdir(out_dir):
    #
    #     number_dir = os.path.join(out_dir, spn)
    #     if isdir(number_dir):
    #         check_and_fix_rr(number_dir)


def check_and_fix_in_main_dir(out_dir):
    code, std_out, std_err = runcommand("perl check_all.pl", out_dir)
    #check_and_fix_rr(out_dir)
    #check_and_fix_old_rr(out_dir)
    #if code != 0:
    #    error("Exit code = " + str(code))


def run_old_fac(in_dir_spn, out_dir_spn, old_path):
    cmd = "python " + old_path + " " + in_dir_spn + " " + out_dir_spn
    print(cmd)
    code, std_out, std_err = runcommand(cmd)
    print(std_out + " " + std_out)
    if code != 0:
        error("Exit code = " + str(code))


def run_for_all_numbers(in_dir, out_dir, old_path, dont_run_all_tools, exc_fac_path, ph_fac_path):
    i_spectro = list(
        map(lambda x: str(x), sorted(map(lambda x: int(x), filter(lambda f: f.isdigit(), os.listdir(in_dir))))))
    for spn in i_spectro:
        in_dir_spn = in_dir + os.path.sep + spn
        out_dir_spn = out_dir + os.path.sep + spn
        if not dont_run_all_tools:
            run_for_one_number(spn, in_dir_spn, out_dir_spn, old_path, exc_fac_path, ph_fac_path)
    return i_spectro


################## MAIN ######################
def main():
    if len(sys.argv) < 3:
        error('\nUsage: ' + sys.argv[
            0] + 'directory-with-cFAC-1.6.3-files-per-spectroscopic-charge output-directory min_eins_coeff')

    in_dir = os.path.abspath(sys.argv[1])
    out_dir = os.path.abspath(sys.argv[2])
    min_eins_coef = float(sys.argv[3])
    dont_run_all_tools = len(sys.argv) > 4 and sys.argv[4] == "false"

    run_main(in_dir, out_dir, min_eins_coef, dont_run_all_tools)


def run_main(in_dir, out_dir, min_eins_coef, dont_run_all_tools):
    try:
        env()
        old_path, fit_path, exc_fac_path, ph_fac_path, my_dir = get_pathes()
        if not dont_run_all_tools:
            check_dirs(in_dir, out_dir)
        warnings_file_path = os.path.join(out_dir, "WARNINGS.txt")
        if os.path.exists(warnings_file_path):
            os.remove(warnings_file_path)
        spec_numbers = run_for_all_numbers(in_dir, out_dir, old_path, dont_run_all_tools, exc_fac_path, ph_fac_path)
        check_and_fix(my_dir, out_dir)
        #ionization_potential, translation_table = create_tables(out_dir)
        translation_table = {}
        next_spec_number = str(int(spec_numbers[len(spec_numbers) - 1]) + 1)
        if int(next_spec_number) - int(spec_numbers[0]) != len(spec_numbers):
            error("Missing or redundant spec numbers directories: " + str(spec_numbers))
        translation_table[next_spec_number] = {"1": "1"}
        create_aiw(out_dir, spec_numbers, translation_table)
        #create_bcfp(out_dir, spec_numbers, translation_table)
        create_excit(out_dir, spec_numbers, translation_table)
        #create_rrec(out_dir, spec_numbers, translation_table)
        element, el_num, number_of_electrons = create_inp(out_dir, spec_numbers, translation_table, ionization_potential)
        create_spectr(out_dir, spec_numbers, translation_table, min_eins_coef)
        #run_for_fisher(dont_run_all_tools, element, out_dir)
        replace_from_mz(el_num, out_dir)
        check_and_fix_in_main_dir(out_dir)
    except GenericPlasmaException as e:
        error(e.message)

if __name__ == "__main__":
    main()
