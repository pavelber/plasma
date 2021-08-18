import os
import shutil

from lib.utils import skip_n_lines, sort_file_by_levels, warn, error

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


def create_bcfp(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, BCFP_HEADER, "BCFP.INP", "BCFP.INP",
                 {9: lambda n: translation_table.get(n), 15: lambda n: translation_table.get(str(int(n) + 1))},
                 None,lambda x: x,False, 4)
    sort_file_by_levels(out_dir, "BCFP.INP", 0, 1, 3, 2)
    shutil.copyfile(os.path.join(out_dir, "BCFP.INP"), os.path.join(out_dir, "BCFP.INP.before.AIW"))
    copy_from_aiw(out_dir)


def create_rrec(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, "", "RREC.INP", "output_ph.dat",
                 {6: lambda n: translation_table.get(n), 13: lambda n: translation_table.get(str(int(n) + 1))},
                 "REC", rrec_line_improver, False, 4)
    sort_file_by_levels(out_dir, "RREC.INP", 0, 1, 2, 0)


def create_excit(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, EXCIT_HEADER, "EXCIT.INP", "outpp.dat",
                 {8: lambda n: translation_table.get(n), 15: lambda n: translation_table[n]}, "EXC",
                 excit_line_improver)
    sort_file_by_levels(out_dir, "EXCIT.INP", 0, 1, 2, 2, True)


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
                 in_file_dir=None, final_line_converter=lambda x: x, renumerate=True, field_width=3):
    positions = sorted(position_3_chars_to_translation_table.keys())
    file_path = os.path.join(out_dir, out_file_name)
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
                    if len(line) > 10:
                        new_line = create_output_line(line, n, position_3_chars_to_translation_table, positions,
                                                      out_dir, out_file_name, renumerate, field_width)
                        outf.write(final_line_converter(new_line))


def create_output_line(line, n, position_3_chars_to_translation_table, positions, out_dir, out_file_name,
                       renumerate=True, field_width=3):
    new_line = ''
    pred_pos = 0
    for pos in positions:
        new_line += line[pred_pos: pos]
        num = line[pos: pos + field_width]
        num_stripped = num.strip()
        transition_table = position_3_chars_to_translation_table[pos](n)
        if not num_stripped in transition_table:
            if renumerate:
                warn(out_dir,
                     "ERROR while creating " + out_file_name + ": " + " Can't find level " + num_stripped +
                     " from the line " + line)
                error("ERROR while creating " + out_file_name + ": " + " Can't find level " + num_stripped +
                      " from the line " + line)
            else:
                new_num = num_stripped
        else:
            new_num = transition_table[num_stripped]

        new_line += "%3s" % new_num
        pred_pos = pos + field_width
    new_line += line[pred_pos:]
    return new_line
