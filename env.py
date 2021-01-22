import os
import sys
from os.path import abspath, dirname

from utils import error
from utils import runcommand


def check_file(p):
    if os.path.exists(p):
        print("\t" + p + " ... OK")
    else:
        error(p + " expected, but not exists")


def env(perl_path):
    my_dir = dirname(abspath(__file__))
    print("Script path: " + my_dir)
    os_name = sys.platform
    print("Current OS: " + os_name)
    os_path = my_dir + os.path.sep + os_name
    print("Current OS executable path: " + os_path)
    python_path = sys.executable
    print("Path to python: " + python_path)

    codep, outp, errp = runcommand(python_path + ' --version')
    print('Python version:' + errp)
    if not perl_path:
        print("No perl path passed, searching in PATH")
        perl_path = "perl"
    else:
        print("Searching for perl in: " + perl_path)
    code, out, err = runcommand(perl_path + " -MMoose -e \"print $Moose::VERSION\"")

    if code != 0:
        error('No perl found')
    else:
        print('Perl version: ' + out)

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

    return python_path, perl_path, old_path, fit_path, exc_fac_path, ph_fac_path

