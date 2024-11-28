from lib.create_in1_from_piter import create_in1_inp_from_piter
from lib.create_spectr_and_excit_from_piter_match_config import create_spectr_and_excit_from_piter_match_config


def parse_energy_limits(limits_str):
    limits = {}
    sp_nums = limits_str.split(",")
    for spn in sp_nums:
        limit = spn.split(":")
        limits[limit[0].strip()] = float(limit[1].strip())

    return limits


def create_in1_excit_spectr__from_databases(out_dir, elem, nucleus, i_spectro, energy_limits, nmax):
    sp_nums = create_in1_inp_from_piter(out_dir, elem, nucleus, i_spectro, energy_limits, nmax)
    create_spectr_and_excit_from_piter_match_config(out_dir, elem, i_spectro)
    return sp_nums
