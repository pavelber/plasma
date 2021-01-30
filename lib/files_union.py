import os

BCFP_HEADER = "  iSS  iQS  fSS  fQS           D              -A               B               C\n" + \
              "-----------------------------------------------------------------------------------\n"
EXCIT_HEADER = "  SS   #1   #2   Mthd        A          B            C            D            E            F          Osc.Strngth\n" + \
               "------------------------------------------------------------------------------------------------------------------\n"

#Take tranlation table, 2nd column from this cpectr. number, 4rd columnd from the table of next sperct number
def create_bcfp(out_dir, spec_numbers):
    create_union(out_dir, spec_numbers, BCFP_HEADER, "BCFP.INP", "BCFP.INP")

#Take tranlation table, 2nd column from this cpectr. number, 3rd columnd from the table of next sperct number
def create_rrec(out_dir, spec_numbers):
    create_union(out_dir, spec_numbers, "", "RREC.INP", "output_ph.dat", "REC")

#Take tranlation table, 2nd column and 3rd column from this cpectr. number,
def create_excit(out_dir, spec_numbers):
    create_union(out_dir, spec_numbers, BCFP_HEADER, "EXCIT.INP", "outpp.dat", "EXC")


def create_union(out_dir, spec_numbers, header, out_file_name, in_file_name, in_file_dir=None):
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
                    outf.write(line)
