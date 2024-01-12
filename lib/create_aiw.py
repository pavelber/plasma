import os

from lib.utils import sort_file_by_levels, error


def copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table):
    for n in spec_numbers:
        n_next = str(int(n) + 1)
        if not n_next in translation_table:
            error("No renumeration table for spectroscopic number " + n_next)
        max_level = max(map(lambda x: int(x), translation_table[n].keys()))
        in_path = os.path.join(out_dir, n, "fac.ai")
        if os.path.exists(in_path):
            with open(in_path, 'r') as inf:
                for line in inf:
                    parts = line.split()
                    if len(parts) == 7:
                        lev1_search = str(int(parts[0]) + 1)
                        lev2_search = str(int(parts[2]) + 1 - max_level)
                        if lev1_search in translation_table[n]:
                            lev1 = translation_table[n][lev1_search]
                            if not lev2_search in translation_table[n_next]:
                                error("No level " + lev2_search + " in renumeration table for spectroscopic number " + n_next)
                            lev2 = translation_table[n_next][lev2_search]
                            outf.write(
                                ("%5s%5s%5s%5s%15s%13.3f\n") % (n, lev1, n_next, lev2, parts[5], float(parts[4])))
                        else:
                            print("No " + lev1_search + " or " + lev2_search)


def create_aiw(out_dir, spec_numbers, translation_table):
    file_path = os.path.join(out_dir, "AIW.INP")
    print("Creation of " + file_path)
    with open(file_path, 'w') as outf:
        outf.write("  SSi AIQS#  SSf  QSf#       WAI          DE(eV)\n")
        copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table)
    sort_file_by_levels(out_dir, "AIW.INP", 0, 1, 3, 1)
