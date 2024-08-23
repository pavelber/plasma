import os
import re
import shutil

from lib.exceptions import GenericPlasmaException
from lib.utils import runcommand_print, runcommand


def copy_checks(my_dir, out_dir):
    check_dir = os.path.join(my_dir, "check")
    print("copy check and fix utils")
    for filename in os.listdir(check_dir):
        check_file = os.path.join(check_dir, filename)
        shutil.copy(check_file, out_dir)
        for spn in os.listdir(out_dir):
            number_dir = os.path.join(out_dir, spn)
            if os.path.isdir(number_dir):
                shutil.copy(check_file, number_dir)


def check_and_fix_rr_version2(rec_dir):
    print("Fixing RREC in " + rec_dir + "\n")
    max_iter = 10
    i = 0
    good_checks = 0
    bad_lines = []
    while i < max_iter:
        bad_lines = run_check_rr_return_lines(rec_dir)
        bad = len(bad_lines)
        if bad == 0:
            good_checks = good_checks + 1
        else:
            run_new_fix(rec_dir, bad_lines, False)
        i = i + 1
    run_new_fix(rec_dir, bad_lines, True)
    return bad_lines


def check_and_fix_rr(dir):
    print("Fixing RREC in " + dir + "\n")
    max_iter = 100
    i = 0
    bad = run_check_rr(dir)
    while bad > 0 and i < max_iter:
        run_fix_rr(dir)
        bad = run_check_rr(dir)
        i = i + 1


def check_and_fix_old_rr_version2(rec_dir):
    print("Fixing RREC in " + rec_dir + "\n")
    max_iter = 100
    i = 0
    good_checks = 0
    while good_checks < 10 and i < max_iter:
        bad = run_check_old_rr(rec_dir)
        if bad == 0:
            good_checks = good_checks + 1
        else:
            run_fix_old_rr(rec_dir)
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


def increase(p, k):
    if "E" in p:
        E = p.index("E")
        n = "0.002" + p[E:]
    else:
        n = "0.002"
    return float(p) + k * float(n)


def run_new_fix(dir, bad_lines, just_remove_and_not_fix):
    bad_hash = {}
    for bad in bad_lines:
        bad_hash[(bad[0], bad[1], bad[2])] = bad

    old_rrec_name = os.path.join(dir, "RREC.INP")
    new_rrec_name = os.path.join(dir, "RREC.INP.NEW")
    with open(old_rrec_name, "r") as old_rrec:
        with open(new_rrec_name, "w") as new_rrec:
            for line in old_rrec:
                parts = line.split()
                level = (parts[0], parts[1], parts[2])
                if level in bad_hash:
                    new_4 = increase(parts[4], +1)
                    new_5 = increase(parts[5], -1)
                    line = line[0:22] + "% .3e" % new_4 + "  " + "% .3e" % new_5 + line[44:]
                    if not just_remove_and_not_fix:
                        new_rrec.write(line)
                else:
                    new_rrec.write(line)

    shutil.move(new_rrec_name, old_rrec_name)


def run_fix_rr(dir):
    code, std_out, std_err = runcommand_print("perl fix_rr.pl", dir)


def run_check_rr(dir):
    code, std_out, std_err = runcommand_print("check_rr.exe", dir)
    p = re.compile('with (.*) bad lines')
    num_of_bad_lines = int(p.findall(std_out)[0])
    return num_of_bad_lines


def run_check_rr_return_lines(dir):
    code, std_out, std_err = runcommand_print("check_rr.exe", dir)
    lines = std_out.split("\n")[:-2]
    return list(map(lambda l: l.split(), lines))


def run_fix_old_rr(dir):
    code, std_out, std_err = runcommand_print("perl old_fix_rr.pl", dir)


def run_check_old_rr(dir):
    code, std_out, std_err = runcommand_print("old_check_rr.exe", dir)
    p = re.compile('with (.*) bad lines')
    bad_lines = int(p.findall(std_out)[0])
    return bad_lines


def check_fix(sp_path, my_dir, rrec_path, dir_bad, sp):
    backup_rrec_name = os.path.join(sp_path, "RREC.INP.BACK")
    shutil.copyfile(rrec_path, backup_rrec_name)
    if os.path.getsize(rrec_path) > 0:
        copy_checks(my_dir, sp_path)
        bad = check_and_fix_rr_version2(sp_path)
        dir_bad[sp] = bad
        if len(bad) > 1:
            bad = check_and_fix_rr_version2(sp_path)
            if len(bad) > 0:
                raise GenericPlasmaException("Still have bad " + str(bad))


def create_rrec_inp(elem_dir, ph_fac_path, i_spectro):
    for sn in i_spectro:
        rrec_dir = os.path.join(elem_dir, str(sn))
        create_rrec_inp_from_dir(rrec_dir, ph_fac_path, str(sn))


def create_rrec_inp_from_dir(rrec_dir, ph_fac_path, sn):
    code, std_out, std_err = runcommand(ph_fac_path, rrec_dir, sn)
    print(std_out + " " + std_err)
    if code != 0:
        raise GenericPlasmaException("Exit code = " + str(code))
    print("Running " + "type output_ph.dat|sort>RREC.INP" + " in " + rrec_dir)
    code, std_out, std_err = runcommand("type output_ph.dat|sort>RREC.INP", rrec_dir)
    print(std_out + " " + std_err)
    if code != 0:
        raise GenericPlasmaException("Exit code = " + str(code))
