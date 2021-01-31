import os

from lib.inp1 import read_element


def create_spectr_header(out_dir, spec_numbers):
    last_spect_number = spec_numbers[len(spec_numbers) - 1]
    el, el_nu, num_of_electrons = read_element(out_dir + os.path.sep + last_spect_number)


def create_spectr_inp(out_dir, spec_numbers, translation_table):
    file_path = out_dir + os.path.sep + "SPECTR.INP"
    print("Creation of " + file_path)
    header = create_spectr_header(out_dir, spec_numbers)
    with open(file_path, 'wb') as outf:
        outf.write(header)