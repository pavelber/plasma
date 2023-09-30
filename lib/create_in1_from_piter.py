import os

from lib.utils import read_table, add_one_to_config_in_missing


def clean_num(s):
    return s.rstrip("?").strip('[]').strip("+x")


def format_configuration(configuration, max_len):
    configuration_split = configuration.replace(" ", ".").split(".")
    s = " ".join(configuration_split)
    if len(s) > max_len:
        return list(filter(lambda c: c[0:1] != '(', configuration_split))
    else:
        return configuration_split


def remove_braces(param):
    return param.replace("(", "").replace(")", "")


def should_include_level(energy, energy_limit, configs):
    last_config_part = configs[-1]
    first_letter_index = last_config_part.find(next(filter(str.isalpha, last_config_part)))
    n = int(last_config_part[0:first_letter_index])
    return energy_limit > float(energy)


def extract_configs(spec_num, configuration):
    configs = format_configuration(configuration, 10)
    configs = list(filter(lambda x: len(x) > 0, configs))
    if len(configs) == 1:
        print(spec_num + " ... " + configs[0])
        configs.insert(0, "")

    for i in range(len(configs)):
        configs[i] = add_one_to_config_in_missing(configs[i])

    return configs


def write_section(outf, spec_num, spec_num_file, data_file, energy_limits):
    with open(spec_num_file, "r") as inf:
        n = 1
        outf.write(spec_num + '\n')
        prev_energy_str = None
        for line in inf:
            parts = line.split()
            if len(parts) == 5:
                configs = normalize_config(parts[0])
                configs_copy_for_csv = configs.copy()
                if configs_copy_for_csv[0] == "":
                    configs_copy_for_csv.pop(0)
                if len(configs_copy_for_csv) == 3:
                    configs_copy_for_csv.pop(0)

                configs_one_string = ".".join(configs_copy_for_csv)

                if len(configs) == 1:
                    configs.insert(0, "")


                term = parts[1]
                g = parts[2]
                energy_str = parts[3]
                energy = float(clean_num(energy_str))
                energy_str = "%10.3f" % energy

                if prev_energy_str is not None and float(energy_str) <= float(prev_energy_str):
                    energy = energy + float(prev_energy_str) - float(energy_str) + 0.001
                    energy_str = "%10.3f" % energy

                if len(term) > 6:
                    term = term[0:6]

                if should_include_level(energy, energy_limits, configs):
                    outf.write("%4s %4s %-8s%3s%15.3f    0.00e+00 0.00e+00  % 6d\n" % (
                        remove_braces(configs[-2]), remove_braces(configs[-1]), term, g, energy, n))
                    data_file.write("%s,%d,%f,%s,%s,%s\n" % (spec_num, n, energy, configs_one_string, g, term))
                    n = n + 1
                    prev_energy_str = energy_str


def normalize_config(conf):
    m = list(map(lambda c: add_one_to_config_in_missing(c), filter(lambda c: c[0:1] != '(', conf.split("."))))
    if len(m) == 3:
        m.pop(0)
    return m


def count_levels(spec_num_file, max_energy):
    with open(spec_num_file, "r") as inf:
        levels = 0
        for line in inf:
            parts = line.split()
            if len(parts) == 5:
                level_energy = clean_num(parts[3])
                configs = normalize_config(parts[0])
                if should_include_level(level_energy, max_energy, configs):
                    levels += 1
        return levels, 0  # autoion levels = 0


def find_ionization_potenzial(spec_num_file, max_energy):
    with open(spec_num_file, "r") as inf:
        for line in inf:
            parts = line.split()
            if parts[1] == "Limit":
                return float(clean_num(parts[2]))


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


def create_in1_inp_from_piter(dir, elem, energy_limits):
    print("Create IN1 from Piter in " + dir)
    levels_dir = os.path.join(dir, "levels")
    table = read_table()
    with open(os.path.join(dir, "IN1.INP"), 'w') as in1_inp:
        with open(os.path.join(dir, "IN1.csv"), 'w') as in1_csv:
            i_spectro = sorted(list(map(lambda x: int(os.path.splitext(x)[0]),
                                        filter(lambda f:
                                               os.path.splitext(
                                                   os.path.basename(f))[0].isdigit() and
                                               os.path.splitext(os.path.basename(f))[
                                                   1] == '.txt',
                                               os.listdir(levels_dir)))))
            print("Got spectroscopic numbers " + str(i_spectro))

            spec_number_max_energy = {}
            levels_number = {}
            for f in i_spectro:
                spec_number_max_energy[f] = find_ionization_potenzial(os.path.join(levels_dir, str(f) + '.txt'),
                                                                      energy_limits[str(f)])
            for f in i_spectro:
                levels_number[f] = count_levels(os.path.join(levels_dir, str(f) + '.txt'), energy_limits[str(f)])

            create_header(i_spectro, elem, table, in1_inp, spec_number_max_energy, levels_number)

            # Nuclear
            in1_inp.write("  7    1    0  0     0.00     0      0.00   0.0000   0.0000   0.000\n")

            for f in i_spectro:
                write_section(in1_inp, str(f), os.path.join(levels_dir, str(f) + '.txt'), in1_csv,
                              energy_limits[str(f)])

            in1_inp.write("7\n")
            in1_inp.write(" Nucleus                               0.00e+00 0.00e+00\n")

            i_spectro.append(7)  # TODO C
            return i_spectro
