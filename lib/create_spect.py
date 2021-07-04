import os

from lib.utils import read_table, skip_n_lines, read_element


def create_spectr_header(out_dir, spec_numbers):
    last_spect_number = spec_numbers[len(spec_numbers) - 1]
    el, el_nu, num_of_electrons = read_element(os.path.join(out_dir, last_spect_number))
    (name_to_table, num_to_table) = read_table()
    return str(name_to_table[el]['AtomicMass']) + " 100.0 200.0 1000"+os.linesep


def copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table, from_fac_tr, min_eins_coef):
    in_path = os.path.join(out_dir, "EXCIT.INP")
    last_sp_num = None
    line_data = []
    with open(in_path, 'rb') as inf:
        skip_n_lines(inf, 2)
        for line in inf:
            parts = line.split()
            sp_num = parts[0]
            if sp_num != last_sp_num:
                line_data.sort(key=sorting_key)
                write_lines_to_file(line_data, outf)
                line_data = []
                last_sp_num = sp_num
            tr = (sp_num, parts[1], parts[2])
            if tr in from_fac_tr:
                transition_data = from_fac_tr[tr]
                einstein = transition_data[0]
                wave_len = transition_data[1]
                if einstein > min_eins_coef:
                    line_data.append((sp_num, parts[2], parts[1], wave_len, einstein))
        write_lines_to_file(line_data, outf)


def sorting_key(x):
    c = int(x[1])
    d = int(x[2])
    if c > 0:
        n = c
    else:
        n = 1000 - c
    if d > 0:
        k = d
    else:
        k = 1000 - d
    return 10000 * n + k


def write_lines_to_file(line_data, outf):
    for l in line_data:
        new_line = ("%2s %4s %4s   1.000   %11.4f  %.5e"+os.linesep) % l
        outf.write(new_line)


def read_fac_tr(out_dir, spec_numbers, translation_table):
    transition_to_line = {}
    for n in spec_numbers:
        in_path = os.path.join(out_dir, n, "fac.tr")
        with open(in_path, 'rb') as inf:
            for line in inf:
                parts = line.split()
                if len(parts) == 8:
                    tr1 = str(int(parts[2]) + 1)
                    tr2 = str(int(parts[0]) + 1)
                    if tr1 in translation_table[n] and tr2 in translation_table[n]:
                        tr = (n, translation_table[n][tr1], translation_table[n][tr2])
                        einstein = float(parts[6])
                        e = float(parts[4])
                        wave_length = 12398.318 / e
                        if tr in transition_to_line:
                            old = transition_to_line[tr]
                            if old[0] < einstein:
                                transition_to_line[tr] = (einstein, wave_length)
                        else:
                            transition_to_line[tr] = (einstein, wave_length)
                    else:
                        print("No transition " + n + " " + tr1 + " " + tr2 + " in translation table")
    return transition_to_line


def create_spectr(out_dir, spec_numbers, translation_table, ionization_potential, min_eins_coef):
    from_fac_tr = read_fac_tr(out_dir, spec_numbers, translation_table)
    file_path = out_dir + os.path.sep + "SPECTR.INP"
    print("Creation of " + file_path)
    header = create_spectr_header(out_dir, spec_numbers)
    with open(file_path, 'wb') as outf:
        outf.write(header)
        copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table, from_fac_tr, min_eins_coef)
