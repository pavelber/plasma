import os
import sys

from lib.in1 import IN1


def merge(fac_dir, nist_dir):
    fac_in1 = IN1(os.path.join(fac_dir, "IN1.INP"))
    nist_in1 = IN1(os.path.join(nist_dir, "IN1.INP"))
    fac_in1.add_or_replace_sp_data("5", nist_in1)


if __name__ == "__main__":
    fac_dir = sys.argv[1]
    nist_dir = sys.argv[2]
    renumeration_table = merge(fac_dir, nist_dir)
