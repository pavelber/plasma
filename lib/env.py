import os
import sys
from os.path import abspath, dirname

from lib.exceptions import GenericPlasmaException
from lib.utils import runcommand


def check_file(p):
    if os.path.exists(p):
        print("\t" + p + " ... OK")
    else:
        return p + " expected, but not exists"
    return None


def check_version(name, cmd):
    code, out, err = runcommand(cmd)
    print("Checking version of " + name + " ...")
    if code != 0:
        return name + ' not found'
    else:
        split = out.split("\n")
        if len(split) > 0:
            v = split[0]
        else:
            v = ""
        print(name + ' version: ' + v)
    return None


def env():
    my_dir = dirname(abspath(__file__)) + os.path.sep + ".."
    print("Script path: " + my_dir)
    os_name = sys.platform
    print("Current OS: " + os_name)
    os_path = my_dir + os.path.sep + os_name
    print("Current OS executable path: " + os_path)
    python_path = sys.executable
    print("Path to python: " + python_path)

    env_error = check_version("wc", "wc --version")
    if env_error is not None:
        raise GenericPlasmaException(env_error)
    env_error = check_version("gzip", "gzip --version")
    if env_error is not None:
        raise GenericPlasmaException(env_error)

    env_error = check_version("Python", python_path+" --version")
    if env_error is not None:
        raise GenericPlasmaException(env_error)

    env_error = check_version("Perl", "perl -MMoose -e \"print $Moose::VERSION\"")
    if env_error is not None:
        raise GenericPlasmaException(env_error)

    print("Searching for components:")
    if os_name == "win32":
        exc_fac_name = "exc_fac_static.exe"
        ph_fac_name = "ph_fac_static.exe"
    else:
        exc_fac_name = "exc_fac"
        ph_fac_name = "ph_fac"

    exc_fac_path = os_path + os.path.sep + exc_fac_name
    ph_fac_path = os_path + os.path.sep + ph_fac_name
    fit_path = my_dir + os.path.sep + "fit.pl"
    fac_in1_path = my_dir + os.path.sep + "fac_IN1.pl"
    old_path = my_dir + os.path.sep + "old_fac.py"
    env_error = check_file(old_path)
    if env_error is not None:
        raise GenericPlasmaException(env_error)

    env_error = check_file(fit_path)
    if env_error is not None:
        raise GenericPlasmaException(env_error)

    env_error = check_file(exc_fac_path)
    if env_error is not None:
        raise GenericPlasmaException(env_error)

    env_error = check_file(ph_fac_path)
    if env_error is not None:
        raise GenericPlasmaException(env_error)

    env_error = check_file(fac_in1_path)
    if env_error is not None:
        raise GenericPlasmaException(env_error)

    return None


def get_pathes():
    my_dir = dirname(abspath(__file__)) + os.path.sep + ".."
    os_name = sys.platform
    os_path = my_dir + os.path.sep + os_name
    if os_name == "win32":
        exc_fac_name = "exc_fac_static.exe"
        ph_fac_name = "ph_fac_static.exe"
    else:
        exc_fac_name = "exc_fac"
        ph_fac_name = "ph_fac"
    exc_fac_path = os_path + os.path.sep + exc_fac_name
    ph_fac_path = os_path + os.path.sep + ph_fac_name
    fit_path = my_dir + os.path.sep + "fit.pl"
    old_path = my_dir + os.path.sep + "old_fac.py"
    return old_path, fit_path, exc_fac_path, ph_fac_path, my_dir
