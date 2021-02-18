import os

from lib.utils import skip_n_lines

BCFP_HEADER = "  iSS  iQS  fSS  fQS           D              -A               B               C\n" + \
              "-----------------------------------------------------------------------------------\n"
EXCIT_HEADER = "  SS   #1   #2   Mthd        A          B            C            D            E            F          Osc.Strngth\n" + \
               "------------------------------------------------------------------------------------------------------------------\n"


def create_bcfp(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, BCFP_HEADER, "BCFP.INP", "BCFP.INP",
                 {8: lambda n: translation_table[n], 20: lambda n: translation_table[str(int(n) + 1)]})
    copy_from_aiw(out_dir)


def create_rrec(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, "", "RREC.INP", "output_ph.dat",
                 {5: lambda n: translation_table[n], 10: lambda n: translation_table[str(int(n) + 1)]},
                 "REC")


def create_excit(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, BCFP_HEADER, "EXCIT.INP", "outpp.dat",
                 {6: lambda n: translation_table[n], 11: lambda n: translation_table[n]}, "EXC")


def copy_from_aiw(out_dir):
    in_filename = os.path.join(out_dir, "AIW.INP")
    out_filename = os.path.join(out_dir, "BCFP.INP")
    print("Append to " + out_filename)

    with open(out_filename, 'ab') as outf:
        with open(in_filename, 'rb') as inf:
            skip_n_lines(inf, 1)
            for line in inf:
                parts = line.split()
                outf.write("%4s  %5s %5s %5s %15s 	 0.0000e+00 	 0.0000e+00      0.0000e+00\n" %
                           (parts[0], parts[1], parts[2], parts[3], parts[4]))


def create_union(out_dir, spec_numbers, header, out_file_name, in_file_name, position_3_chars_to_translation_table,
                 in_file_dir=None):
    file_path = out_dir + os.path.sep + out_file_name
    print("Creation of " + file_path)

    with open(file_path, 'wb') as outf:
        outf.write(header)
        for n in spec_numbers:
            if in_file_dir:
                in_path = out_dir + os.path.sep + n + os.path.sep + in_file_dir + os.path.sep + in_file_name
            else:
                in_path = out_dir + os.path.sep + n + os.path.sep + in_file_name
            with open(in_path, 'rb') as inf:
                for line in inf:
                    new_line = ''
                    pred_pos = 0
                    positions = sorted(position_3_chars_to_translation_table.keys())
                    for pos in positions:
                        new_line += line[pred_pos: pos]
                        num = line[pos: pos + 3]
                        new_num = position_3_chars_to_translation_table[pos](n)[num.strip()]
                        new_line += "%3s" % new_num
                        pred_pos = pos + 3
                    new_line += line[pred_pos:]
                    outf.write(new_line)
