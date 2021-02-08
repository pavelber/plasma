import os

HEADER1 = "%s %d %2s %2s102 2 0-1 2 0 1e+50 0 000 0  0 0 1.0e-02 1 0 0 0.0e+00 1.0    2.0  000010   1.4e-04 0.0e+00 100.0\n"
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


# Take in1.inp, add last column, translation from row number according to the translation table
# Drop 3rd column

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


def create_inp_header(out_dir, spec_numbers):
    last_spect_number = spec_numbers[len(spec_numbers) - 1]
    el, el_nu, num_of_electrons = read_element(os.path.join(out_dir, last_spect_number))
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
            "%3s %3d %3d   0 %4.2f    12 %4.2f  	0.000  0.0000	0.000\n" % (n, num_of_levels, num_of_ai_levels,
                                                                                 ionization_potential[n],
                                                                                 ionization_potential[n]))

    if should_add_next_spect_num:
        outf.write(
            "%3s   1   0   0    0.00     0    0.00   0.000  0.0000	0.000\n" % (nucleus))


def copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table, should_add_next_spect_num, nucleus):
    for n in spec_numbers:
        outf.write(str(n) + "\n")

        in_path = out_dir + os.path.sep + n + os.path.sep + "IN1.INP"
        with open(in_path, 'rb') as inf:
            count = 1
            prev_energy = None
            for line in inf:
                energy = line[22:34]
                if energy == prev_energy:
                    energy = float(energy.strip()) + 0.001
                    energy = "%12.3f" % energy
                new_line = line[:11] + line[17:22] + energy + line[34:len(line) - 2] + (
                            "%6s\n" % translation_table[n][str(count)])
                outf.write(new_line)
                prev_energy = energy
                count += 1
    if should_add_next_spect_num:
        outf.write("%d\nNucleus     0          0.000    0.00e+00 0.00e+00      1\n" % nucleus)


def create_inp(out_dir, spec_numbers, translation_table, ionization_potential):
    file_path = out_dir + os.path.sep + "IN1.INP"
    print("Creation of " + file_path)
    with open(file_path, 'wb') as outf:
        should_add_next_spect_num, nucleus, header = create_inp_header(out_dir, spec_numbers)
        outf.write(header)
        lines_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table, ionization_potential,
                                        should_add_next_spect_num, nucleus)
        copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table, should_add_next_spect_num,
                                       nucleus)
