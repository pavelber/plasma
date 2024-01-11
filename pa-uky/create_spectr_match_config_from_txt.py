import os
import sys

from lib.utils import error, read_table, skip_n_lines


def normalize_energy(energy):
    return str(round(float(energy), 4))


def nist_strip(s):
    if s.startswith('"=""'):
        return s[4:-3].strip()
    else:
        return s.strip()


def clean_num(s):
    return s.rstrip("?")


def add1(c):
    if not c[-1].isdigit():
        return c + "1"
    else:
        return c


def format_configuration(configuration):
    configuration_split = configuration.split(".")
    removed_braces = filter(lambda c: c[0:1] != '(', configuration_split)
    removed_braces = map(add1, removed_braces)
    if len(removed_braces) > 1:
        return removed_braces[-2] + "." + removed_braces[-1]
    else:
        return removed_braces[0]


def strip_temp(t):
    if t[-1] == '*':
        return t[:-1]
    else:
        return t


def read_configs(dir):
    confs = {}
    with open(os.path.join(dir, "IN1.csv"), "r") as inf:
        for line in inf:
            parts = line.split(",")
            sp_n = parts[0]
            level = parts[1]
            config = (parts[3].strip(), parts[4].strip())
            if sp_n not in confs:
                confs[sp_n] = {}
            confs[sp_n][config] = level
    return confs


def create_header(table, elem, file):
    file.write("%2s 7.10 7.90 2000\n" % table[0][elem]["AtomicNumber"])


def create_header_ecxit(table, elem, file):
    file.write(
        "  SS   #1   #2   Mthd        A          B            C            D            E            F          Osc.Strngth\n" +
        "------------------------------------------------------------------------------------------------------------------\n")


def write_spectr_section(outf, spec_num, config_table, spec_num_file):
    with open(spec_num_file, "rb") as inf:
        skip_n_lines(inf, 18)
        for line in inf:
            if not line.startswith('***'):
                parts = line.strip().split('|')
                if len(parts) == 14:
                    conf = parts[3].strip().split(" - ")
                    terms = parts[4].strip().split(" - ")
                    ek = normalize_energy(parts[12])
                    ei = normalize_energy(parts[10])
                    wave = parts[0]
                    eins = parts[8]
                    osc = '0.0'
                    if len(conf) == 2:
                        conf_low = format_configuration(conf[0].strip())
                        conf_up = format_configuration(conf[1].strip())
                    if len(terms) == 2:
                        term_low = strip_temp(terms[0].strip())
                        term_up = strip_temp(terms[1].strip())
                    else:
                        continue

                    if (conf_low, term_low) in config_table[spec_num] and (conf_up, term_up) in config_table[spec_num] \
                            and wave != '' and eins != '':
                        up_level = config_table[spec_num][(conf_up, term_up)]
                        low_level = config_table[spec_num][(conf_low, term_low)]
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
        skip_n_lines(inf, 5)

        for line in inf:
            if not line.startswith('***'):
                parts = line.strip().split('|')
                if len(parts) == 14:
                    ek = normalize_energy(parts[12])
                    ei = normalize_energy(parts[10])
                    osc = '0.0'
                    if ek in energy_table[spec_num] and ei in energy_table[spec_num]:
                        up_level = energy_table[spec_num][ek]
                        low_level = energy_table[spec_num][ei]
                        lines.append((low_level, up_level, osc))
    return lines


################## MAIN ######################
if len(sys.argv) < 3:
    error('\nUsage: ' + sys.argv[
        0] + ' directory-with-nist-csv out-dir element-name')

in_dir = os.path.abspath(sys.argv[1])
out_dir = os.path.abspath(sys.argv[2])
elem = sys.argv[3]

if not os.path.isdir(in_dir) or not os.path.exists(in_dir):
    error(in_dir + " does not exists or is not a directory")

if not os.path.exists(out_dir):
    os.makedirs(out_dir)

with open(os.path.join(out_dir, "SPECTR.INP"), 'w') as spectr_inp:
    with open(os.path.join(out_dir, "EXCIT.INP"), 'w') as exit_inp:
        i_spectro = sorted(map(lambda x: int(os.path.splitext(x)[0]),
                               filter(lambda f:
                                      os.path.splitext(
                                          os.path.basename(f))[0].isdigit() and os.path.splitext(os.path.basename(f))[
                                          1] == '.txt',
                                      os.listdir(in_dir))))
        table = read_table()
        configs = read_configs(out_dir)

        create_header(table, elem, spectr_inp)
        create_header_ecxit(table, elem, exit_inp)

        for f in i_spectro:
            write_spectr_section(spectr_inp, str(f), configs, os.path.join(in_dir, str(f) + '.txt'))
            section_lines = read_section(str(f), configs, os.path.join(in_dir, str(f) + '.txt'))
            sorted_lines = sorted(section_lines, key=lambda l: "%04d,%04d" % (int(l[0]), int(l[1])))
            write_excit_section(exit_inp, str(f), sorted_lines)
