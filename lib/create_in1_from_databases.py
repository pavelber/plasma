import os

from lib.create_in1_from_nist import create_in1_inp_from_nist
from lib.create_spectr_from_piter_match_energy import create_spectr_from_piter_match_energy
from lib.download_parse_pa_uky import download_piter
from lib.nist import download_nist_for_in1


def parse_energy_limits(limits_str):
    limits = {}
    sp_nums = limits_str.split(",")
    for spn in sp_nums:
        limit = spn.split(":")
        limits[limit[0].strip()] = float(limit[1].strip())

    return limits


def create_in1_from_databases(out_dir, elem, energy_limits, download):
    if download:
        download_nist_for_in1(elem, out_dir)
        download_piter(elem, out_dir)
    sp_nums = create_in1_inp_from_nist(out_dir, elem, energy_limits)
    create_spectr_from_piter_match_energy(out_dir, elem)
    return sp_nums