import os
import sys

from lib.utils import error, read_table, skip_n_lines


def nist_strip(s):
    if s.startswith('"=""'):
        return s[4:-3].strip()
    else:
        return s.strip()


def clean_num(s):
    return s.rstrip("?")


def format_configuration(configuration, max_len):
    configuration_split = configuration.split(".")
    s = " ".join(configuration_split)
    if len(s) > max_len:
        return " ".join(filter(lambda c: c[0:1] != '(', configuration_split))
    else:
        return " ".join(configuration_split)


def read_energies(dir):
    eng = {}
    with open(os.path.join(dir, "IN1.csv"), "r") as inf:
        for line in inf:
            parts = line.split(",")
            sp_n = parts[0]
            level = parts[1]
            energy = parts[2].strip()
            if sp_n not in eng:
                eng[sp_n] = {}
            rounded = normalize_energy(energy)
            eng[sp_n][rounded] = level
    return eng


def normalize_energy(energy):
    return str(round(float(energy), 3))


def create_header(table, elem, file):
    file.write("%2s 7.10 7.90 2000\n" % table[0][elem]["AtomicNumber"])


def create_header_ecxit(table, elem, file):
    file.write(
        "  SS   #1   #2   Mthd        A          B            C            D            E            F          Osc.Strngth\n" +
        "------------------------------------------------------------------------------------------------------------------\n")


def write_spectr_section(outf, spec_num, energy_table, spec_num_file):
    with open(spec_num_file, "rb") as inf:
        skip_n_lines(inf, 18)
        for line in inf:
            if not line.startswith('***'):
                parts = line.strip().split()
                if len(parts) == 14:
                    ek = normalize_energy(parts[12])
                    ei = normalize_energy(parts[10])
                    wave = parts[0]
                    eins = parts[8]
                    osc = '0.0'
                    if ek in energy_table[spec_num] and ei in energy_table[spec_num]:
                        up_level = energy_table[spec_num][ek]
                        low_level = energy_table[spec_num][ei]
                        outf.write("%3s %3s %3s 1 %8.3f %8.3e %s\n" % (
                            spec_num, up_level, low_level, float(wave), float(eins), osc))


def write_excit_section(outf, spec_num, lines):
    for line in lines:
        low_level = line[0]
        up_level = line[1]
        osc = line[2]
        outf.write(
            "%3s   %3s  %3s    00     0.000E+00    0.000E+00    0.000E+00    0.000E+00    0.000E+00    0.000E+00      -%s\n" % (
                spec_num, low_level, up_level, osc))


def read_section(spec_num, energy_table, spec_num_file):
    lines = []
    with open(spec_num_file, "rb") as inf:
        for line in inf:
            if line.startswith('"'):
                parts = line.strip().split(',')
                ek = nist_strip(parts[5])
                ei = nist_strip(parts[4])
                wave = nist_strip(parts[0])
                eins = nist_strip(parts[1])
                osc = nist_strip(parts[2])
                if ek in energy_table[spec_num] and ei in energy_table[spec_num]:
                    up_level = energy_table[spec_num][ek]
                    low_level = energy_table[spec_num][ei]
                    lines.append((low_level, up_level, osc))
    return lines


def create_spectr_from_piter_match_energy(out_dir, elem):
    with open(os.path.join(out_dir, "SPECTR.INP"), 'wb') as spectr_inp:
        with open(os.path.join(out_dir, "EXCIT.INP"), 'wb') as exit_inp:
            i_spectro = sorted(map(lambda x: int(os.path.splitext(x)[0]),
                                   filter(lambda f:
                                          os.path.splitext(
                                              os.path.basename(f))[0].isdigit() and
                                          os.path.splitext(os.path.basename(f))[
                                              1] == '.txt',
                                          os.listdir(out_dir))))
            print("Got spectroscopic numbers " + str(i_spectro))
            table = read_table()
            energies = read_energies(out_dir)

            create_header(table, elem, spectr_inp)
            create_header_ecxit(table, elem, exit_inp)

            for f in i_spectro:
                write_spectr_section(spectr_inp, str(f), energies, os.path.join(out_dir, str(f) + '.txt'))
                section_lines = read_section(str(f), energies, os.path.join(out_dir, str(f) + '.txt'))
                sorted_lines = sorted(section_lines, key=lambda l: "%04d,%04d" % (int(l[0]), int(l[1])))
                write_excit_section(exit_inp, str(f), sorted_lines)

