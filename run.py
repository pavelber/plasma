import os
import sys
from os.path import abspath, dirname
from subprocess import Popen, PIPE


def error(s):
    sys.stderr.write(s + '\n')
    exit(1)


def runcommand(cmd):
    proc = Popen(cmd,
                 stdout=PIPE,
                 stderr=PIPE,
                 shell=True,
                 universal_newlines=True)
    std_out, std_err = proc.communicate()
    return proc.returncode, std_out, std_err


def check_file(p):
    if os.path.exists(p):
        print("\t" + p + " ... OK")
    else:
        error(p + " expected, but not exists")


def env(perl_path, i_dir, o_dir):
    my_dir = dirname(abspath(__file__))
    print("Script path: " + my_dir)
    os_name = sys.platform
    print("Current OS: " + os_name)
    os_path = my_dir + os.path.sep + os_name
    print("Current OS executable path: " + os_path)
    python_path = sys.executable
    print("Path to python: " + python_path)

    codep, outp, errp = runcommand(python_path + ' --version')
    # print('Python version:' + errp)
    # if not perl_path:
    #     print("No perl path passed, searching in PATH")
    #     perl_path = "perl"
    # else:
    #     print("Searching for perl in: " + perl_path)
    # code, out, err = runcommand(perl_path + " -MMoose -e \"print $Moose::VERSION\"")
    #
    # if code != 0:
    #     error('No perl found')
    # else:
    #     print('Perl version: ' + out)

    print("Searching for components:")
    if os_name == "win32":
        exc_fac_name = "exc_fac.exe"
        ph_fac_name = "ph_fac.exe"
    else:
        exc_fac_name = "exc_fac"
        ph_fac_name = "ph_fac"

    exc_fac_path = os_path + os.path.sep + exc_fac_name
    ph_fac_path = os_path + os.path.sep + ph_fac_name
    fit_path = my_dir + os.path.sep + "fit.pl"
    old_path = my_dir + os.path.sep + "old_fac.py"
    check_file(old_path)
    check_file(fit_path)
    check_file(exc_fac_path)
    check_file(ph_fac_path)
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

    return python_path, perl_path, old_path, fit_path, exc_fac_path, ph_fac_path


def run_old_fac(in_dir, out_dir, python_path, old_path):
    i_spectro = sorted(filter(lambda f: f.isdigit(), os.listdir(in_dir)))
    for spn in i_spectro:
        cmd = python_path + " " + old_path + " " + in_dir + os.path.sep + spn + " " + out_dir + os.path.sep + spn
        print(cmd)
        code, std_out, std_err = runcommand(cmd)
        if code != 0:
            error("**** Failed.\n"+std_out+"\n"+std_err)


################## MAIN ######################
if len(sys.argv) < 3:
    error('Usage: ' + sys.argv[
        0] + 'directory-with-cFAC-1.6.3-files-per-spectroscopic-charge output-directory [path-to-perl-executable]')

in_dir = sys.argv[1]
out_dir = sys.argv[2]
if len(sys.argv) > 4:
    perl_exe = sys.argv[3]
else:
    perl_exe = None

python_path, perl_path, old_path, fit_path, exc_fac_path, ph_fac_path = env(perl_exe, in_dir, out_dir)

run_old_fac(in_dir, out_dir, python_path, old_path)
