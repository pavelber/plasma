import argparse
import configparser
import os
import shutil
from os.path import dirname, abspath

from lib.bcfp import BCFP
from lib.check_and_fix import copy_checks, check_and_fix_old_rr_version2, check_and_fix_rr_version2, check_fix, \
    create_rrec_inp
from lib.create_in1_from_databases import create_in1_excit_spectr__from_databases, parse_energy_limits
from lib.create_rrec_bcfp_from_in1 import create_rrec_from_in1, create_bcfp_from_in1
from lib.cross_section_db import add_cross_section_from_db
from lib.env import get_pathes, env
from lib.excit import EXCIT
from lib.fisher import run_for_fisher
from lib.in1 import IN1
from lib.remove_lines_and_renumenrate import remove_unused_lines_and_renumerate
from lib.rrec import RREC
from lib.spectr import SPECTR
from lib.update_fits import create_new_fits_for_rrec2
from lib.utils import error, read_table, invert_replaces
from lib.verify_results import files_not_empty


################## MAIN ######################

def parse_args_and_config():
    parser = argparse.ArgumentParser(description='Create all from databases')
    parser.add_argument('--out-dir', type=str, help='Output directory')
    parser.add_argument('--element', type=str, help='Element name')
    parser.add_argument('--nmax', type=int, help='Nmax value')
    parser.add_argument('--energy-limits', type=str, help='Energy limits string')
    parser.add_argument('--min-sp-num', type=int, help='Minimum sp number')
    parser.add_argument('--max-sp-num', type=int, help='Maximum sp number')
    parser.add_argument('--formula', action='store_true', help='Use formula')
    parser.add_argument('--config', type=str, help='Config file path (INI format)')
    parser.add_argument('--replace-starting-from', type=int, help='Spectroscopical number to start using FAC (optional)')
    args = parser.parse_args()

    # If --config is provided, load all params from file, else require all named params
    if args.config:
        config = configparser.ConfigParser()
        config.read(args.config)
        cfg = config['FormValues'] if 'FormValues' in config else {}

        def get_param(name, conv=str, default=None):
            return conv(cfg[name]) if name in cfg else default

        out_dir = str(get_param('out_dir'))
        elem = str(get_param('elem'))
        nmax = int(get_param('nmax', int))
        energy_limits = str(get_param('energy_limits'))
        min_sp_num = int(get_param('spmin', int))
        max_sp_num = int(get_param('spmax', int))
        formula = (cfg.get('formula', '').lower() == 'formula')
        replace_starting_from = get_param('replace_starting_from',str,  None) if 'replace_starting_from' in cfg else None
    else:
        # All params must be provided as named args
        missing = [p for p in ['out_dir', 'element', 'nmax', 'energy_limits', 'min_sp_num', 'max_sp_num'] if
                   getattr(args, p) is None]
        if missing:
            parser.error(f"Missing required arguments: {', '.join('--' + m.replace('_', '-') for m in missing)}")
        out_dir = str(args.out_dir)
        elem = str(args.element)
        nmax = int(args.nmax)
        energy_limits = str(args.energy_limits)
        min_sp_num = int(args.min_sp_num)
        max_sp_num = int(args.max_sp_num)
        formula = args.formula
        replace_starting_from = args.replace_starting_from if args.replace_starting_from is not None else None
    return out_dir, elem, nmax, energy_limits, min_sp_num, max_sp_num, formula, replace_starting_from


if __name__ == "__main__":
    out_dir, elem, nmax, energy_limits, min_sp_num, max_sp_num, use_formula, replace_starting_from = parse_args_and_config()
    if not all([out_dir, elem, nmax, energy_limits, min_sp_num, max_sp_num]):
        error('Missing required parameters. Use --help for details or check your config file.')
    energy_limits = parse_energy_limits(energy_limits)
    out_dir = os.path.abspath(out_dir)
    use_formula = (use_formula == 'formula')

    (name_to_table, num_to_table) = read_table()

    env_errors = env()
    if env_errors is not None:
        error(env_errors)

    old_path, fit_path, exc_fac_path, ph_fac_path, my_dir = get_pathes()

    if not os.path.exists(out_dir):
        os.makedirs(out_dir)

    elem_dir = os.path.join(out_dir, elem)

    if not os.path.exists(elem_dir):
        os.makedirs(elem_dir)
    my_dir = dirname(abspath(__file__))
    levels_downloaded = os.path.join(my_dir, "db", elem, "levels")

    lines_downloaded = os.path.join(my_dir, "db", elem, "lines")

    levels_dir = os.path.join(elem_dir, "levels")
    if os.path.exists(levels_dir):
        shutil.rmtree(levels_dir)
    lines_dir = os.path.join(elem_dir, "lines")

    if os.path.exists(lines_dir):
        shutil.rmtree(lines_dir)
    shutil.copytree(levels_downloaded, levels_dir)
    shutil.copytree(lines_downloaded, lines_dir)

    (name_to_table, num_to_table) = read_table()

    nucleus = int(name_to_table[elem]["AtomicNumber"]) + 1
    sp_nums_dec = list(range(min_sp_num, max_sp_num + 1))
    sp_nums_str = list(map(lambda x: str(x), range(min_sp_num, max_sp_num + 1)))
    sp_nums_with_nucleus = sp_nums_dec[:]

    if max_sp_num + 1 == nucleus:
        sp_nums_with_nucleus.append(nucleus)

    create_in1_excit_spectr__from_databases(elem_dir, elem, nucleus, sp_nums_dec,
                                            energy_limits,
                                            nmax)
    in1_path = os.path.join(elem_dir, "IN1.INP")
    create_rrec_from_in1(in1_path, elem, elem_dir, sp_nums_with_nucleus, nucleus, use_formula)
    create_bcfp_from_in1(in1_path, elem_dir, sp_nums_with_nucleus, nucleus)

    create_rrec_inp(elem_dir, ph_fac_path, sp_nums_with_nucleus)

    dir_bad = {}

    for sp in sp_nums_with_nucleus:
        sp_path = os.path.join(elem_dir, str(sp))
        rrec_path = os.path.join(sp_path, "RREC.INP")
        check_fix(sp_path, my_dir, rrec_path, dir_bad, sp)
    rrec_path = os.path.join(elem_dir, "RREC.INP")
    excit_path = os.path.join(elem_dir, "EXCIT.INP")
    spectr_path = os.path.join(elem_dir, "SPECTR.INP")

    bcfp_path = os.path.join(elem_dir, "BCFP.INP")

    with open(rrec_path, "w") as rrec:
        for sp in sp_nums_with_nucleus:
            sp_path = os.path.join(elem_dir, str(sp))
            rrec_sp = os.path.join(sp_path, "RREC.INP")
            with open(rrec_sp, "r") as sp_rrec:
                for line in sp_rrec:
                    rrec.write(line)

    copy_checks(my_dir, elem_dir)
    check_and_fix_rr_version2(elem_dir)
    check_and_fix_old_rr_version2(elem_dir)

    for sp in sp_nums_with_nucleus:
        print("**********************")
        print(sp)
        print("**********************")
        if sp in dir_bad:
            lines = dir_bad[sp]
            for l in lines:
                print(l)

    replaces = remove_unused_lines_and_renumerate(elem_dir, nucleus)
    #
    # from_new_to_old = invert_replaces(replaces)
    # create_new_fits_for_rrec2(elem_dir, 'powell', from_new_to_old, "RREC-fits.INP", "RREC.INP")
    # create_new_fits_for_rrec2(elem_dir, 'powell', from_new_to_old, "RREC-fits-2.INP", "RREC-fits.INP", True)
    # # Rename new fits file to RREC.INP
    # rrec_fits_path = os.path.join(elem_dir, "RREC-fits-2.INP")
    # if os.path.exists(rrec_fits_path):
    #     shutil.move(rrec_path, rrec_path + ".orig")
    #     shutil.move(rrec_fits_path, rrec_path)

    ######### TESTS ##########
    nist_in1 = IN1(in1_path)  # it will test for correctness
    files_not_empty(elem_dir)

    ##################################################
    db_path = os.path.join(my_dir, "db", elem)
    ionization_cross_section_directory = os.path.join(db_path, "ionization-crosssection")
    excitation_cross_section_directory = os.path.join(db_path, "excitation-crosssection")
    nist_bcfp = BCFP(bcfp_path)
    nist_excit = EXCIT(excit_path)

    nist_bcfp.create_fac_fitting_params(
        in1_data=nist_in1,
        cross_section_directory=ionization_cross_section_directory,
        create_tabulation_files=False,
        remove_bad_fits=True #TODO: поговорить
    )
    nist_excit.replace_fits_from_cross_sections(nist_in1, excitation_cross_section_directory)

    #На малых числах Нист
    if replace_starting_from is not None:
        fac_dir = os.path.join(db_path, "fac")
        if not os.path.exists(fac_dir):
            error(f"FAC directory {fac_dir} does not exist, while replace_starting_from exists in parameters.")


        nist_spectr = SPECTR(spectr_path)
        nist_rrec = RREC(rrec_path)
        fac_in1 = IN1(os.path.join(fac_dir, "IN1.INP"))
        fac_bcfp = BCFP(os.path.join(fac_dir, "BCFP.INP"))
        fac_excit = EXCIT(os.path.join(fac_dir, "EXCIT.INP"))
        fac_spectr = SPECTR(os.path.join(fac_dir, "SPECTR.INP"))
        fac_rrec = RREC(os.path.join(fac_dir, "RREC.INP"))

        renumeration_table = nist_in1.add_or_replace_sp_data(str(replace_starting_from), fac_in1)
        nist_excit.replace_transitions(start_sp_num=replace_starting_from, other=fac_excit,
                                  renumeration_table=renumeration_table)
        nist_bcfp.replace_transitions(start_sp_num=replace_starting_from, other=fac_bcfp,
                                 renumeration_table=renumeration_table)
        nist_spectr.replace_transitions(start_sp_num=replace_starting_from, other=fac_spectr,
                                   renumeration_table=renumeration_table)
        nist_rrec.replace_transitions(start_sp_num=replace_starting_from, other=fac_rrec,
                                 renumeration_table=renumeration_table)

        shutil.copy(in1_path,in1_path+".db")
        shutil.copy(bcfp_path,bcfp_path+".db")
        shutil.copy(excit_path,excit_path+".db")
        shutil.copy(spectr_path,spectr_path+".db")
        shutil.copy(rrec_path,rrec_path+".db")

        nist_in1.dump_to_file(in1_path)
        nist_bcfp.dump_to_file(bcfp_path)
        nist_excit.dump_to_file(excit_path)
        nist_spectr.dump_to_file(spectr_path)
        nist_rrec.dump_to_file(rrec_path)

    #####################
    levels_num = run_for_fisher(False, min_sp_num, max_sp_num, elem, elem_dir, "BCFP.INP", False)

    # removed = remove_large(rrec_path, 0, [4, 5], 1.0e-4)
    # print("Removed " + str(removed) + "from " + rrec_path)
    # file_name = os.path.join(elem_dir, "RREC-fits.INP")
    # removed = remove_large(file_name, 0, [4, 5], 1.0e-4)
    # print("Removed " + str(removed) + "from " + file_name)
