import os

from lib.exceptions import GenericPlasmaException
from lib.utils import read_table, normalize_energy


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


def create_header(table, elem, file):
    file.write("%2s 7.10 7.90 2000\n" % table[0][elem]["AtomicNumber"])


def create_header_ecxit(table, elem, file):
    file.write(
        "  SS   #1   #2   Mthd        A          B            C            D            E            F          Osc.Strngth\n" +
        "------------------------------------------------------------------------------------------------------------------\n")


def write_spectr_section_from_piter(outf, spec_num, energy_table, spec_num_file):
    lines = []
    with open(spec_num_file, "r") as inf:
        # skip_n_lines(inf, 18)
        for line in inf:
            if not line.startswith('***'):
                parts = line.strip().split()
                if len(parts) == 15:
                    ek = normalize_energy(parts[13])
                    ei = normalize_energy(parts[11])
                    wave = parts[0]
                    if wave[-1] == '?':
                        wave = wave[:-1]
                    eins = parts[8]
                    osc = parts[9]
                    if ek in energy_table[spec_num] and ei in energy_table[spec_num]:
                        up_level = energy_table[spec_num][ek]
                        low_level = energy_table[spec_num][ei]
                        outf.write("%3s %3s %3s 1 %8.3f %8.3e %8.3e\n" % (
                            spec_num, up_level, low_level, float(wave), float(eins), float(osc)))
                        lines.append((low_level, up_level, osc))
        return lines


def write_excit_section(outf, spec_num, lines):
    for line in lines:
        low_level = line[0]
        up_level = line[1]
        osc = line[2]
        outf.write(
            "%3s   %3s  %3s    0     0.000E+00    0.000E+00    0.000E+00    0.000E+00    0.000E+00    0.000E+00      -%s\n" % (
                spec_num, low_level, up_level, osc))


def create_spectr_and_excit_from_piter_match_energy(out_dir, elem,i_spectro):
    piter_dir = os.path.join(out_dir, "piter")
    with open(os.path.join(out_dir, "SPECTR.INP"), 'w') as spectr_inp:
        with open(os.path.join(out_dir, "EXCIT.INP"), 'w') as exit_inp:
            print("Got spectroscopic numbers " + str(i_spectro))
            table = read_table()
            energies = read_energies(out_dir)

            create_header(table, elem, spectr_inp)
            create_header_ecxit(table, elem, exit_inp)

            for f in i_spectro:
                sp_num_str = str(f)
                section_lines = write_spectr_section_from_piter(spectr_inp, sp_num_str, energies,
                                                                os.path.join(piter_dir, sp_num_str + '.txt'))
                if len(section_lines) == 0:
                    raise GenericPlasmaException("No lines for " + elem + " " + sp_num_str)
                sorted_lines = sorted(section_lines, key=lambda l: "%04d,%04d" % (int(l[0]), int(l[1])))
                write_excit_section(exit_inp, sp_num_str, sorted_lines)
