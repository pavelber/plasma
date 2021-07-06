import os

from lib.utils import sort_file_by_levels, error


def copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table, max_levels, min_levels):
    for n in spec_numbers:
        n_next = str(int(n) + 1)
        max_level = max(map(lambda x: int(x), translation_table[n].keys()))
        in_path = os.path.join(out_dir, n, "fac.ai")
        if os.path.exists(in_path):
            with open(in_path, 'rb') as inf:
                for line in inf:
                    parts = line.split()
                    if len(parts) == 7:
                        lev1_search = str(int(parts[0]) + 1)
                        lev2_search = str(int(parts[2]) + 1)
                        if lev1_search in translation_table[n]:
                            lev1 = translation_table[n][lev1_search]
                            lev2 = str(int(lev2_search) - max_level)
                            if int(lev1) > max_levels[n] or int(lev1) < min_levels[n]:
                                error(
                                    "AIW: for spectroscopic number " + n + " not existing level " + lev1 + " in line " + line)
                            if int(lev2) > max_levels[n_next] or int(lev2) < min_levels[n_next]:
                                error(
                                    "AIW: for spectroscopic number " + n_next + " not existing level " + lev2 + " in line " + line)
                            outf.write(
                                ("%5s%5s%5s%5s%15s%15s" + os.linesep) % (n, lev1, n_next, lev2, parts[5], parts[4]))
                        else:
                            print ("No " + lev1_search + " or " + lev2_search)


def create_aiw(out_dir, spec_numbers, translation_table, max_levels, min_levels):
    file_path = os.path.join(out_dir, "AIW.INP")
    print("Creation of " + file_path)
    with open(file_path, 'wb') as outf:
        outf.write("  SSi AIQS#  SSf  QSf#       WAI          DE(eV)" + os.linesep)
        copy_for_spectroscopic_numbers(outf, out_dir, spec_numbers, translation_table, max_levels, min_levels)
    sort_file_by_levels(out_dir, "AIW.INP", 0, 1, 3, 1)
