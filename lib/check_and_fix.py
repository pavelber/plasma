import os
import re
import shutil

from lib.utils import runcommand_print



def copy_checks(my_dir, out_dir):
    check_dir = os.path.join(my_dir, "check")
    print "copy check and fix utils"
    for filename in os.listdir(check_dir):
        check_file = os.path.join(check_dir, filename)
        shutil.copy(check_file, out_dir)
        for spn in os.listdir(out_dir):
            number_dir = os.path.join(out_dir, spn)
            if os.path.isdir(number_dir):
                shutil.copy(check_file, number_dir)

def check_and_fix_rr(dir):
    print("Fixing RREC in "+dir+"\n")
    max_iter = 100
    i = 0
    bad = run_check_rr(dir)
    while bad > 0 and i < max_iter:
        run_fix_rr(dir)
        bad = run_check_rr(dir)
        i = i + 1


def check_and_fix_old_rr(dir):
    print("Fixing RREC in "+dir+"\n")
    max_iter = 100
    i = 0
    bad = run_check_old_rr(dir)
    while bad > 0 and i < max_iter:
        run_fix_old_rr(dir)
        bad = run_check_old_rr(dir)
        i = i + 1


def run_fix_rr(dir):
    code, std_out, std_err = runcommand_print("perl fix_rr.pl", dir)


def run_check_rr(dir):
    code, std_out, std_err = runcommand_print("check_rr.exe", dir)
    p = re.compile('with (.*) bad lines')
    bad_lines = int(p.findall(std_out)[0])
    return bad_lines


def run_fix_old_rr(dir):
    code, std_out, std_err = runcommand_print("perl old_fix_rr.pl", dir)


def run_check_old_rr(dir):
    code, std_out, std_err = runcommand_print("old_check_rr.exe", dir)
    p = re.compile('with (.*) bad lines')
    bad_lines = int(p.findall(std_out)[0])
    return bad_lines
