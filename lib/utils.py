import os
import sys
from shutil import copy
from subprocess import Popen, PIPE


def error(s):
    sys.stderr.write(s + '\n')
    exit(1)


def runcommand(cmd, cwd=".", cmd_in=None):
    proc = Popen(cmd,
                 stdout=PIPE,
                 stderr=PIPE,
                 stdin=PIPE,
                 shell=True,
                 cwd=cwd,
                 universal_newlines=True)
    std_out, std_err = proc.communicate(cmd_in)
    code = proc.returncode
    if code != 0:
        error("**** Failed.\n" + std_out + "\n" + std_err)
    return code, std_out, std_err


def copy_and_run(exe, prefix, out_dir, cwd=".", cmd_in=None, args=""):
    if not os.path.exists(out_dir):
        error(out_dir + " not exists")
    copy(exe, out_dir)
    path_to, file_name = os.path.split(exe)
    cmd = prefix + " " + out_dir + os.path.sep + file_name+" "+args
    print(cmd)
    code, std_out, std_err = runcommand(cmd, cwd, cmd_in)
    return code, std_out, std_err