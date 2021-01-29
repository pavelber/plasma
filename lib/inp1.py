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
    el, el_nu, num_of_electrons = read_element(out_dir + os.path.sep + spec_numbers[0])
    sp_min = int(spec_numbers[0])
    sp_max = int(spec_numbers[len(spec_numbers) - 1])
    if num_of_electrons == 1:
        sp_max = sp_max + 1
    return HEADER1 % (el, el_nu, sp_min, sp_max,) + HEADER


def create_inp(out_dir, spec_numbers):
    file_path = out_dir + os.path.sep + "IN1.INP"
    print("Creation of " + file_path)
    header = create_inp_header(out_dir, spec_numbers)
    with open(file_path, 'wb') as outf:
        outf.write(header)
