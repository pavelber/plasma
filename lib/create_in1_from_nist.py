import os

from lib.levels_string import levels_order, level_to_electrons
from lib.utils import read_table, error


def nist_strip(s):
    if s.startswith('"=""'):
        return s[4:-3].strip()
    else:
        return s.strip()


def clean_num(s):
    return s.rstrip("?").strip('[]')


def format_configuration(configuration, max_len):
    configuration_split = configuration.replace(" ", ".").split(".")
    s = " ".join(configuration_split)
    if len(s) > max_len:
        return filter(lambda c: c[0:1] != '(', configuration_split)
    else:
        return configuration_split


def format_term(s):
    p = s.split(" ")
    return p[-1]


def find_previous(c):
    if c[-1].isdigit():
        config_wo_electrons = c[0:-1]
    else:
        config_wo_electrons = c
    index = levels_order.index(config_wo_electrons)
    return levels_order[index + 1] + str(level_to_electrons[levels_order[index + 1]])


def write_section(elem, outf, spec_num, spec_num_file, data_file, energy_limits):
    with open(spec_num_file, "rb") as inf:
        headers = inf.readline().strip().split(',')
        eV_column = headers.index("Level (eV)")
        n = 1
        outf.write(spec_num + '\n')
        for line in inf:
            if not line.startswith('"=""' + elem):
                parts = line.strip().split(',')
                configuration = nist_strip(parts[0])
                if len(configuration) == 0:
                    continue
                configs = format_configuration(configuration, 10)
                if len(configs) == 1:
                    configs = [find_previous(configs[0]), configs[0]]
                term = format_term(nist_strip(parts[1]))
                g = nist_strip(parts[3])
                energy_str = clean_num(nist_strip(parts[eV_column]))
                level = float(energy_str)
                if configuration.startswith(elem):
                    outf.write("\n")
                    return level
                else:
                    if energy_limits > float(energy_str):
                        outf.write("%4s %4s  %-8s%3s%12.3f    0.00e+00 0.00e+00% 6d\n" % (configs[0], configs[1], term, g, level, n))
                        data_file.write("%s,%d,%s\n" % (spec_num, n, energy_str))
                        n = n + 1


def read_section(elem, spec_num_file):
    with open(spec_num_file, "rb") as inf:
        headers = inf.readline().split(",")
        index = 0
        while index < len(headers):
            if headers[index] == 'Level (eV)':
                break
            index += 1
        if index == len(headers):
            error("Can't find energy column in headers " + str(headers) + " in file " + spec_num_file)
        for line in inf:
            if not line.startswith('"=""' + elem):
                parts = line.strip().split(',')
                level = float(clean_num(nist_strip(parts[index])))
        return level


def read_section_pass2(elem, spec_num_file, energy, max_energy):
    with open(spec_num_file, "rb") as inf:
        inf.readline()
        levels = 0
        ai_levels = 0
        for line in inf:
            if not line.startswith('"=""' + elem) and not line.startswith('"="""""'):
                parts = line.strip().split(',')
                level_energy = float(clean_num(nist_strip(parts[5])))
                if level_energy < max_energy:
                    if level_energy < energy:
                        levels += 1
                    else:
                        ai_levels += 1
        return levels, ai_levels


def create_header(i_spectro, elem, table, in1_inp, spec_number_energy, levels_data):
    in1_inp.write("%2s %2s %2d %2d" % (elem, table[0][elem]["AtomicNumber"], min(i_spectro), max(i_spectro)))
    in1_inp.write("102 2 0-1 2 0 1e+50 0 000 0  0 0 1.0e-02 1 0 0 0.0e+00 1.0    2.0  000010   1.4e-04 0.0e+00 100.0\n")
    in1_inp.write("Tolerances: I/FInt = 1.D-03: SystInt = 1.D-09: StMatr = 1.D-13\n")
    in1_inp.write("El Temp (eV) = a + b*t + c*t*t : a = 3.00000D+02; b = 0.00000D+10; c = 0.00000D+15\n")
    in1_inp.write("2d Temp (eV) = a + b*t + c*t*t : a = 0.00000D+00; b = 0.00000D+00; c = 0.00000D+15\n")
    in1_inp.write("% of 2nd T   = a + b*t + c*t*t : a = 0.00000D-02; b = 0.00000D+00; c = 0.00000D+15\n")
    in1_inp.write("Beam at (eV) = a + b*t + c*t*t : a = 0.00000D+00; b = 0.00000D+00; c = 0.00000D+15\n")
    in1_inp.write("FWHM (eV)    = a + b*t + c*t*t : a = 0.00000D+00; b = 0.00000D+00; c = 0.00000D+15\n")
    in1_inp.write("% of beam    = a + b*t + c*t*t : a = 0.00000D-00; b = 0.00000D+00; c = 0.00000D+15\n")
    in1_inp.write("IDens (cm-3) = A + B*t + C*t*t : a = 1.00000D+00; B = 0.00000D+00; C = 0.00000D+15\n")
    in1_inp.write("   or     A / (B + C*t + D*t*t)  D = 0.00000D+00\n")
    in1_inp.write("EDens (cm-3) = A + B*t + C*t*t : A = 5.00000D+20; B = 0.00000D+00; C = 0.00000D+15\n")
    in1_inp.write("   or     A / (B + C*t + D*t*t)  D = 0.00000D-03\n")
    in1_inp.write("Step= 1.0D-09 sec:No of steps=" + str(len(i_spectro)) + "\n")

    for n in i_spectro:
        in1_inp.write("%3d %4d %4d  0 %8.2f    12  %8.2f   0.0000   0.0000   0.000\n" % (
            n, levels_data[n][0], levels_data[n][1], spec_number_energy[n], spec_number_energy[n]))


def create_in1_inp_from_nist(dir, elem, energy_limits):
    print("Create IN1 from NIST in " + dir)
    with open(os.path.join(dir, "IN1.INP"), 'wb') as in1_inp:
        with open(os.path.join(dir, "IN1.csv"), 'wb') as in1_csv:
            i_spectro = sorted(map(lambda x: int(os.path.splitext(x)[0]),
                                   filter(lambda f:
                                          os.path.splitext(
                                              os.path.basename(f))[0].isdigit() and
                                          os.path.splitext(os.path.basename(f))[
                                              1] == '.csv',
                                          os.listdir(dir))))
            print("Got spectroscopic numbers " + str(i_spectro))
            table = read_table()

            spec_number_energy = {}
            levels_data = {}
            for f in i_spectro:
                csv_path = os.path.join(dir, str(f) + '.csv')
                spec_number_energy[f] = read_section(elem, csv_path)

            for f in i_spectro:
                levels_data[f] = read_section_pass2(elem, os.path.join(dir, str(f) + '.csv'), spec_number_energy[f],
                                                    energy_limits[str(f)])

            create_header(i_spectro, elem, table, in1_inp, spec_number_energy, levels_data)

            for f in i_spectro:
                write_section(elem, in1_inp, str(f), os.path.join(dir, str(f) + '.csv'), in1_csv, energy_limits[str(f)])
            return i_spectro
