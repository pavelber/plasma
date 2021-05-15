import os
import shutil

from lib.utils import skip_n_lines

BCFP_HEADER = "  iSS  iQS  fSS  fQS           D              -A               B               C\n" + \
              "-----------------------------------------------------------------------------------\n"
EXCIT_HEADER = "  SS   #1   #2   Mthd        A          B            C            D            E            F          Osc.Strngth\n" + \
               "------------------------------------------------------------------------------------------------------------------\n"


def excit_line_improver(l):
    s = l.split()
    return "%4s %4s %4s %4s  %12s %12s %12s %12s %12s %12s   %13s\n" % (
        s[0], s[1], s[2], s[3], s[4], s[5], s[6], s[7], s[8], s[9], s[10])


def rrec_line_improver(l):
    s = l.split()
    return " %2s   %4s   %4s   %2s   %10s   %11s   %11s   %11s   %11s   %11s   %11s   %11s   %11s   %11s\n" % (
        s[0], s[1], s[2], s[3], s[4], s[5], s[6], s[7], s[8], s[9], s[10], s[11], s[12], s[13])


def create_level_key(level):
    l_int = int(level)
    if l_int > 0:
        return level.zfill(5)
    else:
        return "z" + str(abs(l_int)).zfill(5)


def sort_file_by_levels(out_dir, file_name):
    file_path = os.path.join(out_dir, file_name)
    file_path_not_sorted = os.path.join(out_dir, file_name + ".notsorted")
    shutil.copyfile(file_path, file_path_not_sorted)
    lines = {}
    keys = []
    with open(file_path, 'wb') as outf:
        with open(file_path_not_sorted, 'rb') as inf:
            # read headers
            outf.write(inf.readline())
            outf.write(inf.readline())
            # read lines to dict
            for line in inf:
                parts = line.split()
                key = parts[0].zfill(5) + "-" + create_level_key(parts[1]) + "-" + create_level_key(parts[2])
                if key in lines:
                    print "WARNING: " + file_name + " duplicate line:\n\t" + line
                lines[key] = line
                keys.append(key)
        keys.sort()
        for k in keys:
            outf.write(lines[k])


def create_bcfp(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, BCFP_HEADER, "BCFP.INP", "BCFP.INP",
                 {8: lambda n: translation_table[n], 20: lambda n: translation_table[str(int(n) + 1)]})
    shutil.copyfile(os.path.join(out_dir, "BCFP.INP"), os.path.join(out_dir, "BCFP.INP.before.AIW"))
    copy_from_aiw(out_dir)


def create_rrec(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, "", "RREC.INP", "output_ph.dat",
                 {5: lambda n: translation_table[n], 10: lambda n: translation_table[str(int(n) + 1)]},
                 "REC", rrec_line_improver)


def create_excit(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, EXCIT_HEADER, "EXCIT.INP", "outpp.dat",
                 {6: lambda n: translation_table[n], 11: lambda n: translation_table[n]}, "EXC", excit_line_improver)
    sort_file_by_levels(out_dir, "EXCIT.INP")


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
                 in_file_dir=None, final_line_converter=lambda x: x):
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
                    outf.write(final_line_converter(new_line))
