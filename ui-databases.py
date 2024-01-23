import os
import shutil
from multiprocessing.pool import ThreadPool
from os.path import dirname, abspath

from lib import current_data
from lib.check_and_fix import create_rrec_inp, check_fix, copy_checks, check_and_fix_rr_version2, \
    check_and_fix_old_rr_version2
from lib.create_in1_from_databases import parse_energy_limits, create_input_from_databases
from lib.create_rrec_bcfp_from_in1 import create_rrec_bcfp_from_in1
from lib.download_parse_pa_uky_levels import download_piter_levels
from lib.download_parse_pa_uky_lines import download_piter_lines
from lib.env import env, get_pathes
from lib.exceptions import GenericPlasmaException
from lib.remove_lines_and_renumenrate import remove_unused_lines_and_renumerate
from lib.update_fits import create_new_fits_for_rrec2
from lib.utils import read_table, dec_to_roman, invert_replaces
from lib.verify_results import test_number_of_levels_inp1, files_not_empty
from ui.create_from_databases_ui import CreateFromDataBasesUI


class Runner:
    def __init__(self):
        self.good = True
        self.ui = CreateFromDataBasesUI()
        self.pool = ThreadPool(processes=1)

    def ui_message(self, message):
        self.ui.append_to_errors("\n" + message)
        self.ui.update()

    def ui_error(self, message):
        self.good = False
        self.ui_message("\n" + message)

    def run_and_set_good(self, func, text):
        self.ui.append_to_errors(text + " ... ")
        self.ui.update()
        if self.good:
            try:
                val = func()
                self.ui.append_to_errors(" Ok\n")
                self.ui.update()
                return val
            except GenericPlasmaException as e:
                self.ui_error(e.message)
                return None
            except Exception as e:
                self.ui_error(str(e))

    def run_async(self):
        try:
            self.ui.disable()
            self.run_and_set_good(env, "Check environment")
            old_path, fit_path, exc_fac_path, ph_fac_path, my_dir = get_pathes()
            out_dir = self.ui.get_out_dir()

            elem = self.ui.get_elem()
            nmax = self.ui.get_nmax()
            osc = self.ui.get_osc()
            energy_limits = parse_energy_limits("1:70.8,2:150,3:250,4:350,5:450,6:550,7:750,8:1000")
            min_sp_num = self.ui.get_spmin()
            max_sp_num = self.ui.get_spmax()

            (name_to_table, num_to_table) = read_table()

            current_data.NUCLEUS = int(name_to_table[elem]["AtomicNumber"]) + 1
            current_data.SPNUMS_TO_USE = list(map(dec_to_roman, range(min_sp_num, max_sp_num + 1)))

            if not os.path.exists(out_dir):
                os.makedirs(out_dir)

            elem_dir = os.path.join(out_dir, elem)

            if not os.path.exists(elem_dir):
                os.makedirs(elem_dir)
            my_dir = dirname(abspath(__file__))

            download = not os.path.exists(os.path.join(my_dir, "db"))

            levels_downloaded = os.path.join(my_dir, "db", elem, "levels")

            lines_downloaded = os.path.join(my_dir, "db", elem, "lines")

            if download:
                self.run_and_set_good(lambda: download_piter_levels(elem, levels_downloaded, nmax), "Download levels")
                self.run_and_set_good(lambda: download_piter_lines(elem, lines_downloaded, nmax, osc), "Download lines")
            else:
                self.ui.append_to_errors("Levels and lines downloaded.")

            levels_dir = os.path.join(elem_dir, "levels")
            if os.path.exists(levels_dir):
                shutil.rmtree(levels_dir)
            lines_dir = os.path.join(elem_dir, "lines")

            if os.path.exists(lines_dir):
                shutil.rmtree(lines_dir)
            shutil.copytree(levels_downloaded, levels_dir)
            shutil.copytree(lines_downloaded, lines_dir)

            sp_nums = self.run_and_set_good(lambda: create_input_from_databases(elem_dir, elem, energy_limits),
                                            "Create IN1, SPECTR, EXCIT")

            in1 = os.path.join(elem_dir, "IN1.INP")
            self.run_and_set_good(lambda: create_rrec_bcfp_from_in1(in1, elem_dir, sp_nums),
                                  "Create rrec per spectroscopic number, BCFP")

            self.run_and_set_good(lambda: create_rrec_inp(elem_dir, ph_fac_path), "Create RREC.INP")

            self.run_and_set_good(lambda: self.check_fix(elem_dir, my_dir, sp_nums), "Check and fix RREC")

            replaces = self.run_and_set_good(lambda: remove_unused_lines_and_renumerate(elem_dir),
                                             "Remove unused lines and renumerate")

            self.run_and_set_good(lambda: self.create_new_fits(elem_dir, replaces), "Create new fits")
            self.run_and_set_good(lambda: self.verification(elem_dir, in1)," Verification");
        except Exception as e:
            self.ui_error(str(e))

        self.ui.enable()

    def verification(self, elem_dir, in1):
        test_number_of_levels_inp1(in1)
        files_not_empty(elem_dir)

    def create_new_fits(self, elem_dir, replaces):
        from_new_to_old = invert_replaces(replaces)
        create_new_fits_for_rrec2(elem_dir, 'powell', from_new_to_old, "RREC-fits.INP", "RREC.INP")
        create_new_fits_for_rrec2(elem_dir, 'powell', from_new_to_old, "RREC-fits-2.INP", "RREC-fits.INP")

    def check_fix(self, elem_dir, my_dir, sp_nums):

        dir_bad = {}

        for sp in sp_nums:
            sp_path = os.path.join(elem_dir, str(sp))
            rrec_path = os.path.join(sp_path, "RREC.INP")
            check_fix(sp_path, my_dir, rrec_path, dir_bad, sp)
        rrec_path = os.path.join(elem_dir, "RREC.INP")
        excit_path = os.path.join(elem_dir, "EXCIT.INP")
        spectr_path = os.path.join(elem_dir, "SPECTR.INP")

        bcfp_path = os.path.join(elem_dir, "BFCP.INP")

        with open(rrec_path, "w") as rrec:
            for sp in sp_nums:
                sp_path = os.path.join(elem_dir, str(sp))
                rrec_sp = os.path.join(sp_path, "RREC.INP")
                with open(rrec_sp, "r") as sp_rrec:
                    for line in sp_rrec:
                        rrec.write(line)

        copy_checks(my_dir, elem_dir)
        check_and_fix_rr_version2(elem_dir)
        check_and_fix_old_rr_version2(elem_dir)
        for sp in sp_nums:
            print("**********************")
            print(sp)
            print("**********************")
            if sp in dir_bad:
                lines = dir_bad[sp]
                for l in lines:
                    print(l)

    def run_it(self):
        self.ui.save_config()
        self.ui.clean_errors()
        self.pool.apply_async(self.run_async)


runner = Runner()
runner.ui.create_ui(runner.run_it)
