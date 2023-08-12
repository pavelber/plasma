import os

from lib.create_in1_from_piter import create_in1_inp_from_piter
from lib.create_spectr_from_piter_match_config_stat_weight_energy import create_spectr_and_excit_from_piter_match_config


def parse_energy_limits(limits_str):
    limits = {}
    sp_nums = limits_str.split(",")
    for spn in sp_nums:
        limit = spn.split(":")
        limits[limit[0].strip()] = float(limit[1].strip())

    return limits


def create_in1_from_databases(out_dir, elem, energy_limits):

    sp_nums = create_in1_inp_from_piter(out_dir, elem, energy_limits)
    create_spectr_and_excit_from_piter_match_config(out_dir, elem)
    return sp_nums
