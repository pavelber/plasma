import os

from lib.levels_string import create_levels_string
from lib.utils import read_element, skip_n_lines

HEADER1 = "%-2s %d %2s %2s102 2 0-1 2 0 1e+50 0 000 0  0 0 1.0e-02 1 0 0 0.0e+00 1.0    2.0  000010   1.4e-04 0.0e+00 100.0\n"
HEADER = "Tolerances: I/FInt = 1.D-03: SystInt = 1.D-09: StMatr = 1.D-13\n" + \
         "El Temp (eV) = a + b*t + c*t*t : a = 3.00000D+02; b = 0.00000D+10; c = 0.00000D+15\n" + \
         "2d Temp (eV) = a + b*t + c*t*t : a = 0.00000D+00; b = 0.00000D+00; c = 0.00000D+15\n" + \
         "% of 2nd T   = a + b*t + c*t*t : a = 0.00000D-02; b = 0.00000D+00; c = 0.00000D+15\n" + \
         "Beam at (eV) = a + b*t + c*t*t : a = 0.00000D+00; b = 0.00000D+00; c = 0.00000D+15\n" + \
         "FWHM (eV)    = a + b*t + c*t*t : a = 0.00000D+00; b = 0.00000D+00; c = 0.00000D+15\n" + \
         "% of beam    = a + b*t + c*t*t : a = 0.00000D-00; b = 0.00000D+00; c = 0.00000D+15\n" + \
         "IDens (cm-3) = A + B*t + C*t*t : a = 1.00000D+00; B = 0.00000D+00; C = 0.00000D+15\n" + \
         "   or     A / (B + C*t + D*t*t)  D = 0.00000D+00\n" + \
         "EDens (cm-3) = A + B*t + C*t*t : A = 5.00000D+20; B = 0.00000D+00; C = 0.00000D+15\n" + \
         "   or     A / (B + C*t + D*t*t)  D = 0.00000D-03\n" + \
         "Step= 1.0D-09 sec:No of steps=6\n"


def read_nele(in_dir):
    in_path = in_dir + os.path.sep + "fac.lev"

    with open(in_path, 'rb') as inf:
        for line in inf:
            parts = line.split()
            if len(parts) > 0 and parts[0] == "NELE":
                num_of_electrons = int(parts[2])
                return num_of_electrons


def create_inp_header(out_dir, spec_numbers, el, el_nu, num_of_electrons):
    last_spect_number = spec_numbers[len(spec_numbers) - 1]

    sp_min = int(spec_numbers[0])
    sp_max = int(last_spect_number)
    if num_of_electrons == 1:
        sp_max = sp_max + 1
        added_num = True
    else:
        added_num = False

    return added_num, sp_max, HEADER1 % (el, el_nu, sp_min, sp_max,) + HEADER


def get_num_of_levels(translation_table_for_sp_num):
    num_of_levels, num_of_ai_levels = 0, 0
    for level in translation_table_for_sp_num:
        if int(translation_table_for_sp_num[level]) > 0:
            num_of_levels += 1
        else:
            num_of_ai_levels += 1
    return num_of_levels, num_of_ai_levels


def lines_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table, ionization_potential,
                                    should_add_next_spect_num, nucleus):
    for n in spec_numbers:
        #  5  15   0   0  123.35    12  126.22  	0.000  0.0000	0.000
        num_of_levels, num_of_ai_levels = get_num_of_levels(translation_table[n])
        outf.write(
            "%3s %3d %3d   0 %8.2f    12 %8.2f   0.0000   0.0000   0.000\n" % (n, num_of_levels, num_of_ai_levels,
                                                                               ionization_potential[n],
                                                                               ionization_potential[n]))

    if should_add_next_spect_num:
        outf.write(
            "%3s   1   0   0     0.00     0     0.00   0.0000   0.0000   0.000\n" % (nucleus))


def read_fac_lev(out_dir, n):
    fac_name = os.path.join(out_dir, n, "fac.lev")
    level_22_line = {}
    with open(fac_name, 'rb') as inf:
        for line in inf:
            parts = line.split()
            if len(parts) > 7:
                level_22_line[int(parts[0]) + 1] = line
    return level_22_line


def choose_better_levels_string(old_levels_string, new_levels_string):
    old_levels_string_parts = old_levels_string.split()
    new_levels_string_parts = new_levels_string.split()
    levels_string_parts = old_levels_string_parts
    if len(new_levels_string_parts) > len(old_levels_string_parts):
        levels_string_parts = new_levels_string_parts
    if len(levels_string_parts) < 2:
        if levels_string_parts[0].startswith("1s"):
            levels_string_parts.append("2s0")
        else:
            levels_string_parts.insert(0, "1s0")

    num = len(levels_string_parts)
    return " %3s  %3s" % (levels_string_parts[num - 2], levels_string_parts[num - 1])


def copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table,
                                   should_add_next_spect_num, nucleus):
    for n in spec_numbers:
        outf.write(str(n) + "\n")
        level_to_line = read_fac_lev(out_dir, n)
        in_path = out_dir + os.path.sep + n + os.path.sep + "IN1.INP"
        num_of_electrons = read_nele(os.path.join(out_dir, n))
        with open(in_path, 'rb') as inf:
            count = 1
            last_num = 1
            first_ai = True
            prev_energy = None
            energy_increment = 0.001
            skip_n_lines(inf, 2)
            for line in inf:
                if line.startswith('Autoionizing states'):
                    continue
                energy = line[24:36]
                if (not prev_energy is None) and (
                        ("%12.3f" % float(energy)) == ("%12.3f" % float(prev_energy)) or float(energy.strip()) < float(
                        prev_energy.strip())):
                    energy_with_increment = float(prev_energy.strip()) + energy_increment
                    use_energy = "%12.3f" % energy_with_increment
                else:
                    use_energy = "%12.3f" % float(energy)
                # if n == "8":
                #    print(n + " " + str(level_num) + " " + energy + " " + use_energy)
                levels_string = line[:11]
                level_num = int(translation_table[n][str(count)])
                if level_num < 0:
                    last_num += 1
                    level_for_line = last_num
                    if first_ai:
                        outf.write("Autoionizating states\n")
                        first_ai = False
                else:
                    last_num = level_num
                    level_for_line = level_num
                new_levels_string = create_levels_string(num_of_electrons, level_to_line[level_for_line])
                levels_string = choose_better_levels_string(levels_string, new_levels_string)
                new_line = levels_string + "       " + line[18:25] + \
                           "" + use_energy + "  " + line[36:len(line) - 2] + (
                                   " %6s\n" % level_num)
                outf.write(new_line)
                prev_energy = use_energy
                count += 1
    if should_add_next_spect_num:
        outf.write("%d\n  Nucleus           0         0.000     0.00e+00 0.00e+00      1\n" % nucleus)


def create_inp(out_dir, spec_numbers, translation_table, ionization_potential):
    last_spect_number = spec_numbers[len(spec_numbers) - 1]
    el, el_nu, num_of_electrons = read_element(os.path.join(out_dir, last_spect_number))
    file_path = out_dir + os.path.sep + "IN1.INP"
    print("Creation of " + file_path)
    with open(file_path, 'wb') as outf:
        should_add_next_spect_num, nucleus, header = create_inp_header(out_dir, spec_numbers, el, el_nu,
                                                                       num_of_electrons)
        outf.write(header)
        lines_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table, ionization_potential,
                                        should_add_next_spect_num, nucleus)
        copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table, should_add_next_spect_num,
                                       nucleus)
    return el, el_nu, num_of_electrons
