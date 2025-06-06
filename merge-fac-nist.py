import os
import argparse

from lib.bcfp import BCFP
from lib.excit import EXCIT
from lib.in1 import IN1
from lib.spectr import SPECTR


def merge(fac_dir, nist_dir, replace_starting_from):
    fac_in1 = IN1(os.path.join(fac_dir, "IN1.INP"))
    nist_in1 = IN1(os.path.join(nist_dir, "IN1.INP"))
    fac_excit = EXCIT(os.path.join(fac_dir, "EXCIT.INP"))
    nist_excit = EXCIT(os.path.join(nist_dir, "EXCIT.INP"))
    fac_bcfp = BCFP(os.path.join(fac_dir, "BCFP.INP"))
    nist_bcfp = BCFP(os.path.join(nist_dir, "BCFP.INP"))
    fac_spectr = SPECTR(os.path.join(fac_dir, "SPECTR.INP"))
    nist_spectr = SPECTR(os.path.join(nist_dir, "SPECTR.INP"))
    fac_rrec = SPECTR(os.path.join(fac_dir, "RREC.INP"))
    nist_rrec = SPECTR(os.path.join(nist_dir, "RREC.INP"))

    renumeration_table = nist_in1.add_or_replace_sp_data(str(replace_starting_from), fac_in1)
    excitation_cross_section_directory = "C:\\work2\\plasma\\db\\O\\excitation-crosssection\\"
    ionization_cross_section_directory = "C:\\work2\\plasma\\db\\O\\ionization-crosssection\\"



    nist_excit.replace_fits_from_cross_sections(nist_in1,excitation_cross_section_directory)

    nist_bcfp.create_fac_fitting_params(
        in1_data=nist_in1,
        cross_section_directory=ionization_cross_section_directory,
        create_tabulation_files=False,
        remove_bad_fits=False
    )

    fac_excit.replace_transitions(start_sp_num=replace_starting_from, other=nist_excit, renumeration_table=renumeration_table)
    fac_bcfp.replace_transitions(start_sp_num=replace_starting_from, other=nist_bcfp, renumeration_table=renumeration_table)
    fac_spectr.replace_transitions(start_sp_num=replace_starting_from, other=nist_spectr, renumeration_table=renumeration_table)
    fac_rrec.replace_transitions(start_sp_num=replace_starting_from, other=nist_rrec, renumeration_table=renumeration_table)

    nist_in1.dump_to_file("C:\\work2\\plasma\\db\\O\\merged\\IN1.INP")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Merge FAC and NIST data')
    parser.add_argument('fac_dir', type=str, help='FAC directory')
    parser.add_argument('nist_dir', type=str, help='NIST directory')
    parser.add_argument('--replace-starting-from', type=int, default=5, help='Spectroscopical number to start using FAC (default: 5)')
    args = parser.parse_args()
    merge(args.fac_dir, args.nist_dir, args.replace_starting_from)
