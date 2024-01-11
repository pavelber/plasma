import os

from lib.utils import read_table, error, add_one_to_config_in_missing, normalize_energy, dec_to_roman


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


def read_configs(dir):
    confs = {}
    with open(os.path.join(dir, "IN1.csv"), "r") as inf:
        for line in inf:
            parts = line.split(",")
            sp_n = parts[0]
            level = parts[1]
            energy = parts[2].strip()
            config = parts[3].strip()
            stat_weight = parts[4].strip()
            term = parts[5].strip()
            if sp_n not in confs:
                confs[sp_n] = {}

            key = (config, stat_weight, term)
            if key not in confs[sp_n]:
                confs[sp_n][key] = []
            confs[sp_n][key].append((level, float(energy)))
    return confs


def create_header_spectr(table, elem, file):
    file.write("%2s 7.10 7.90 2000\n" % table[0][elem]["AtomicNumber"])


def create_header_ecxit(table, elem, file):
    file.write(
        "  SS   #1   #2   Mthd        A          B            C            D            E            F          Osc.Strngth\n" +
        "------------------------------------------------------------------------------------------------------------------\n")


def normalize_config(conf):
    m = list(map(lambda c: add_one_to_config_in_missing(c), filter(lambda c: c[0:1] != '(', conf.split("."))))
    if m[0] == "":
        m.pop(0)
    if len(m) == 3:
        m.pop(0)
    return ".".join(m)


def write_spectr_section_from_piter(outf, spec_num, config_table, spec_num_file):
    lines = []
    stat_good = 0
    stat_bad = 0
    with open(spec_num_file, "r") as inf:
        # skip_n_lines(inf, 18)
        for line in inf:
            if not line.startswith('***'):
                parts = line.strip().split()
                if len(parts) == 16:
                    config = parts[4].split("-")
                    config_k = normalize_config(config[1])
                    config_i = normalize_config(config[0])
                    stat_w_k = parts[8]
                    stat_w_i = parts[6]
                    ek = float(normalize_energy(parts[14]))
                    ei = float(normalize_energy(parts[12]))
                    terms = parts[5].split("-")
                    term_k = terms[1]
                    term_i = terms[0]
                    wave = parts[0]
                    if wave[-1] == '?':
                        wave = wave[:-1]
                    eins = parts[9]
                    osc = parts[10]
                    key_k = (config_k, stat_w_k, term_k)
                    key_i = (config_i, stat_w_i, term_i)
                    if key_k in config_table[spec_num] and key_i in config_table[spec_num]:
                        #    levels_k = list(filter(lambda x: abs(x[1] - ek) < 5e-3, config_table[spec_num][key_k]))
                        #    levels_i = list(filter(lambda x: abs(x[1] - ei) < 5e-3, config_table[spec_num][key_i]))
                        levels_k = min(config_table[spec_num][key_k], key=lambda x: abs(x[1] - ek))
                        levels_i = min(config_table[spec_num][key_i], key=lambda x: abs(x[1] - ei))
                        #   levels_k =  config_table[spec_num][key_k]
                        #   levels_i =  config_table[spec_num][key_i]
                        #       if len(levels_i) == 1 and len(levels_k) == 1:
                        up_level = levels_k
                        low_level = levels_i
                        outf.write("%3s %3s %3s 1 %9.3f %8.3e %8.3e\n" % (
                            spec_num, up_level[0], low_level[0], float(wave), float(eins), float(osc)))
                        lines.append((low_level, up_level, osc))
                        stat_good += 1
                    #      else:
                    #         stat_bad += 1
                    # print("Not found " + line)
                    else:
                        stat_bad += 1
                # print("Not found " + line)
    print("C " + dec_to_roman(int(spec_num)) + "   нашли:   " + str(stat_good) + ",   не нашли:  " + str(stat_bad))
    return lines


def write_excit_section(outf, spec_num, lines):
    for line in lines:
        low_level = line[0]
        up_level = line[1]
        osc = line[2]
        outf.write(
            "%3s   %3s  %3s    0     0.000E+00    0.000E+00    0.000E+00    0.000E+00    0.000E+00    0.000E+00      -%s\n" % (
                spec_num, low_level[0], up_level[0], osc))


def create_spectr_and_excit_from_piter_match_config(out_dir, elem):
    lines_dir = os.path.join(out_dir, "lines")
    with open(os.path.join(out_dir, "SPECTR.INP"), 'w') as spectr_inp:
        with open(os.path.join(out_dir, "EXCIT.INP"), 'w') as exit_inp:
            i_spectro = list(sorted(map(lambda x: int(os.path.splitext(x)[0]),
                                        filter(lambda f:
                                               os.path.splitext(
                                                   os.path.basename(f))[0].isdigit() and
                                               os.path.splitext(os.path.basename(f))[
                                                   1] == '.txt',
                                               os.listdir(lines_dir)))))
            print("Got spectroscopic numbers " + str(i_spectro))
            table = read_table()
            configs = read_configs(out_dir)

            create_header_spectr(table, elem, spectr_inp)
            create_header_ecxit(table, elem, exit_inp)

            for f in i_spectro:
                sp_num_str = str(f)
                section_lines = write_spectr_section_from_piter(spectr_inp, sp_num_str, configs,
                                                                os.path.join(lines_dir, sp_num_str + '.txt'))
                if len(section_lines) == 0:
                    error("No lines for " + elem + " " + sp_num_str)
                sorted_lines = sorted(section_lines, key=lambda l: "%04d,%04d" % (int(l[0][0]), int(l[1][0])))
                write_excit_section(exit_inp, sp_num_str, sorted_lines)
