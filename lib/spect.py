import os

from lib.utils import read_table, skip_n_lines


def read_element(in_dir):
    in_path = in_dir + os.path.sep + "fac.ci"
    line_num = 1
    with open(in_path, 'rb') as inf:
        for line in inf:
            parts = line.split()
            if line_num == 6:
                el = parts[0]
                el_num = int(float(parts[3]))
            if len(parts) > 0 and parts[0] == "NELE":
                num_of_electrons = int(parts[2])
                return el, el_num, num_of_electrons
            line_num += 1


def create_spectr_header(out_dir, spec_numbers):
    last_spect_number = spec_numbers[len(spec_numbers) - 1]
    el, el_nu, num_of_electrons = read_element(os.path.join(out_dir, last_spect_number))
    (name_to_table, num_to_table) = read_table()
    return str(name_to_table[el]['AtomicMass']) + " 100.0 200.0 1000\n"


def copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table):
    in_path = os.path.join(out_dir, "EXCIT.INP")
    with open(in_path, 'rb') as inf:
        skip_n_lines(inf, 2)
        for line in inf:
            parts = line.split()
            new_line = "%2s %4s %4s   1.000\n" % (parts[0],parts[1],parts[2])
            outf.write(new_line)


def create_spectr(out_dir, spec_numbers, translation_table, ionization_potential):
    file_path = out_dir + os.path.sep + "SPECTR.INP"
    print("Creation of " + file_path)
    header = create_spectr_header(out_dir, spec_numbers)
    with open(file_path, 'wb') as outf:
        outf.write(header)
        copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table)
