import os

BCFP_HEADER = "  iSS  iQS  fSS  fQS           D              -A               B               C\n" + \
              "-----------------------------------------------------------------------------------\n"
EXCIT_HEADER = "  SS   #1   #2   Mthd        A          B            C            D            E            F          Osc.Strngth\n" + \
               "------------------------------------------------------------------------------------------------------------------\n"


# Take tranlation table, 2nd column from this cpectr. number, 4rd columnd from the table of next sperct number
def create_bcfp(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, BCFP_HEADER, "BCFP.INP", "BCFP.INP",
                 {8: lambda n: translation_table[n], 20: lambda n: translation_table[str(int(n) + 1)]})


# Take tranlation table, 2nd column from this cpectr. number, 3rd columnd from the table of next sperct number
def create_rrec(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, "", "RREC.INP", "output_ph.dat",
                 {5: lambda n: translation_table[n], 10: lambda n: translation_table[str(int(n) + 1)]},
                 "REC")


# Take tranlation table, 2nd column and 3rd column from this cpectr. number,
def create_excit(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, BCFP_HEADER, "EXCIT.INP", "outpp.dat",
                 {6: lambda n: translation_table[n], 11: lambda n: translation_table[n]}, "EXC")


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
                    for pos in position_3_chars_to_translation_table:
                        num = line[pos: pos + 3]
                        new_num = position_3_chars_to_translation_table[pos](n)[num.strip()]
                        #print(num+"->"+new_num)
                    outf.write(line)
