import os

from lib.exceptions import GenericPlasmaException


def check_file(f_path, required_lines):
    with open(f_path, "r") as f:
        num = len(f.readlines())
        if num < required_lines:
            raise GenericPlasmaException("In file " + f_path + " less than " + str(required_lines) + " lines")


def files_not_empty(elem_dir):
    check_file(os.path.join(elem_dir, "RREC.INP"), 20)
    check_file(os.path.join(elem_dir, "EXCIT.INP"), 20)
    check_file(os.path.join(elem_dir, "SPECTR.INP"), 20)
    check_file(os.path.join(elem_dir, "BFCP.INP"), 20)
    check_file(os.path.join(elem_dir, "IN1.INP"), 20)
