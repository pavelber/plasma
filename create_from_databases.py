import os
import sys

from lib.create_in1_from_nist import create_in1_inp_from_nist
from lib.create_spectr_from_piter_match_energy import create_spectr_from_piter_match_energy
from lib.nist import download_nist_for_in1
from lib.utils import error


def parse_energy_limits(limits_str):
    limits = {}
    sp_nums = limits_str.split(",")
    for spn in sp_nums:
        limit = spn.split(":")
        limits[limit[0].strip()] = float(limit[1].strip())

    return limits


################## MAIN ######################
if len(sys.argv) < 2:
    error('\nUsage: ' + sys.argv[
        0] + ' out-dir element-name')

out_dir = os.path.abspath(sys.argv[1])
elem = sys.argv[2]

energy_limits = {}

if len(sys.argv) > 3:
    energy_limits = parse_energy_limits(sys.argv[3])

print("Got energy limits:")
print(energy_limits)

if not os.path.exists(out_dir):
    os.makedirs(out_dir)

elem_dir = os.path.join(out_dir,elem)
download_nist_for_in1(elem, out_dir)
create_in1_inp_from_nist(elem_dir, elem, energy_limits)
create_spectr_from_piter_match_energy(elem_dir, elem)
