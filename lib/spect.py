import os
from decimal import Decimal

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


def copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table, from_fac_tr):
    in_path = os.path.join(out_dir, "EXCIT.INP")
    with open(in_path, 'rb') as inf:
        skip_n_lines(inf, 2)
        for line in inf:
            parts = line.split()
            tr = (parts[1], parts[2])
            if tr in from_fac_tr:
                transition_data = from_fac_tr[tr]
                einstein = transition_data[0]
                wave_len = transition_data[1]
                new_line = "%2s %4s %4s   1.000   %s   %s\n" % (parts[0], parts[1], parts[2], einstein, wave_len)
                outf.write(new_line)


def read_fac_tr(out_dir, spec_numbers):
    transition_to_line = {}
    for n in spec_numbers:
        in_path = os.path.join(out_dir, n, "fac.tr")
        with open(in_path, 'rb') as inf:
            for line in inf:
                parts = line.split()
                if len(parts) == 8:
                    tr = (str(int(parts[1]) + 1), str(int(parts[0]) + 1))
                    einstein = parts[6]
                    e = float(parts[4])
                    wave_length = Decimal(float(12398.318 / e)).normalize().to_eng_string()
                    if tr in transition_to_line:
                        old = transition_to_line[tr]
                        if float(old[0]) < float(einstein):
                            transition_to_line[tr] = (einstein, wave_length)
                    else:
                        transition_to_line[tr] = (einstein, wave_length)
    return transition_to_line


def create_spectr(out_dir, spec_numbers, translation_table, ionization_potential):
    from_fac_tr = read_fac_tr(out_dir, spec_numbers)
    file_path = out_dir + os.path.sep + "SPECTR.INP"
    print("Creation of " + file_path)
    header = create_spectr_header(out_dir, spec_numbers)
    with open(file_path, 'wb') as outf:
        outf.write(header)
        copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table, from_fac_tr)
