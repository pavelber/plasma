import contextlib
import os
import sys
import traceback
from io import StringIO
from multiprocessing.pool import ThreadPool
from os.path import exists

from lib.check_and_fix import check_and_fix_rr
from lib.create_aiw import create_aiw
from lib.create_files_union import create_bcfp, create_excit, create_rrec
from lib.create_inp1 import create_inp
from lib.create_spect import create_spectr
from lib.env import env, get_pathes
from lib.exceptions import GenericPlasmaException
from lib.fisher import run_for_fisher
from lib.process_mz import replace_from_mz
from lib.renumer import create_tables
from lib.utils import remove_files_and_dirs, string_to_bool
from run import check_dirs, run_for_all_numbers, check_and_fix
from ui.create_input_from_fac_ui import RunFacUI


@contextlib.contextmanager
def capture_stderr():
    old_stderr = sys.stderr
    sys.stderr = StringIO()
    try:
        yield sys.stderr
    finally:
        sys.stderr = old_stderr


class Runner:
    def __init__(self):
        self.good = True
        self.ui = RunFacUI()
        self.pool = ThreadPool(processes=1)

    def ui_message(self, message):
        self.ui.append_to_errors("\n" + message)
        self.ui.update()

    def ui_error(self, message):
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
            input_dir = self.ui.get_input_dir()
            out_dir = self.ui.get_out_dir()
            if exists(out_dir):
                remove_files_and_dirs(out_dir)
            min_eins_coef = self.ui.get_min_eins()
            mz = string_to_bool(self.ui.get_run_mz())
            self.run_and_set_good(lambda: check_dirs(input_dir, out_dir), "Check dirs")
            warnings_file_path = os.path.join(out_dir, "WARNINGS.txt")
            if os.path.exists(warnings_file_path):
                os.remove(warnings_file_path)
            spec_numbers = self.run_and_set_good(
                lambda: run_for_all_numbers(input_dir, out_dir, old_path, False, exc_fac_path, ph_fac_path),
                "Run fac for all numbers")
            self.run_and_set_good(lambda: check_and_fix(my_dir, out_dir), "Check and fix RREC")
            ionization_potential, translation_table = create_tables(out_dir)

            next_spec_number = str(int(spec_numbers[len(spec_numbers) - 1]) + 1)
            if int(next_spec_number) - int(spec_numbers[0]) != len(spec_numbers):
                self.ui_error("Missing or redundant spec numbers directories: " + str(spec_numbers))
                return

            translation_table[next_spec_number] = {"1": "1"}
            self.run_and_set_good(lambda: create_aiw(out_dir, spec_numbers, translation_table), "Create AIW")
            self.run_and_set_good(lambda: create_bcfp(out_dir, spec_numbers, translation_table), "Create BCFP")
            self.run_and_set_good(lambda: create_excit(out_dir, spec_numbers, translation_table), "Create EXCIT")
            self.run_and_set_good(lambda: create_rrec(out_dir, spec_numbers, translation_table), "Create RREC")
            element, el_num, number_of_electrons = self.run_and_set_good(
                lambda: create_inp(out_dir, spec_numbers, translation_table, ionization_potential), "Create IN1")
            self.run_and_set_good(
                lambda: create_spectr(out_dir, spec_numbers, translation_table, ionization_potential, min_eins_coef),
                "Create SPECTR")
            self.run_and_set_good(lambda: run_for_fisher(False, spec_numbers[0], spec_numbers[-1], element, out_dir),
                                  "Create RT-Code files")
            self.run_and_set_good(lambda: check_and_fix_rr(out_dir), "Check and fix RREC on top")

            if mz:
                self.run_and_set_good(lambda: replace_from_mz(el_num, out_dir), "Replace from MZ")
            self.ui_message("Done")
            self.ui.enable()
        except Exception as e:
            self.ui_error(str(e))
            traceback.print_exc()

        self.ui.enable()

    def run_it(self):
        self.ui.save_config()
        self.ui.clean_errors()
        self.pool.apply_async(self.run_async)


runner = Runner()
runner.ui.create_ui(runner.run_it)
# runner.ui = UI(runner.run_it)
