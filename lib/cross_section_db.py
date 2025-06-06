import os

from lib.bcfp import BCFP
from lib.excit import EXCIT


def add_cross_section_from_db(db_path, in1, excit_path, bcfp_path):

    excitation_cross_section_directory = os.path.join(db_path, "excitation-crosssections")
    ionization_cross_section_directory = os.path.join(db_path, "ionization-crosssections")
    if os.path.exists(excitation_cross_section_directory):
        excit = EXCIT(excit_path)
        excit.replace_fits_from_cross_sections(in1, excitation_cross_section_directory)
        excit.dump_to_file(excit_path)
    if os.path.exists(ionization_cross_section_directory):
        bcfp = BCFP(bcfp_path)
        bcfp.create_fac_fitting_params(
            in1_data=in1,
            cross_section_directory=ionization_cross_section_directory,
            create_tabulation_files=False,
            remove_bad_fits=False
        )
        bcfp.dump_to_file(bcfp_path)
