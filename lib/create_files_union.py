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


def sort_file_by_levels(out_dir, file_name, s_num_index, from_level_index, to_level_index, skip_lines):
    file_path = os.path.join(out_dir, file_name)
    file_path_not_sorted = os.path.join(out_dir, file_name + ".notsorted")
    shutil.copyfile(file_path, file_path_not_sorted)
    lines = {}
    keys = []
    spect_num_data_min_level = {}
    spect_num_data_max_level = {}
    spect_num_data_num_of_lines = {}
    warnings_file_path = os.path.join(out_dir, "WARNINGS.txt")
    with open(warnings_file_path, 'ab') as warn_f:
        with open(file_path, 'wb') as outf:
            with open(file_path_not_sorted, 'rb') as inf:
                # read headers
                for _ in range(skip_lines):
                    outf.write(inf.readline())
                # read lines to dict
                for line in inf:
                    parts = line.split()
                    s_num = parts[s_num_index]
                    from_level = parts[from_level_index]
                    to_level = parts[to_level_index]
                    key = s_num.zfill(5) + "-" + create_level_key(
                        from_level) + "-" + create_level_key(to_level)
                    if key in lines:
                        print "WARNING: " + file_name + " duplicate line:\n\t" + line
                        warn_f.write("WARNING: " + file_name + " duplicate line:\n\t" + line)
                    lines[key] = line
                    keys.append(key)

                    if s_num in spect_num_data_num_of_lines:
                        spect_num_data_num_of_lines[s_num] = spect_num_data_num_of_lines[s_num] + 1
                        spect_num_data_min_level[s_num] = min(spect_num_data_min_level[s_num], int(from_level),
                                                              int(to_level))
                        spect_num_data_max_level[s_num] = max(spect_num_data_max_level[s_num], int(from_level),
                                                              int(to_level))
                    else:
                        spect_num_data_num_of_lines[s_num] = 1
                        spect_num_data_min_level[s_num] = min(int(from_level), int(to_level))
                        spect_num_data_max_level[s_num] = max(int(from_level), int(to_level))

            keys.sort()
            for k in keys:
                outf.write(lines[k])

            for s_num in spect_num_data_num_of_lines:
                num_of_levels = spect_num_data_max_level[s_num]
                if spect_num_data_min_level[s_num] < 0:
                    num_of_levels = num_of_levels + abs(spect_num_data_min_level[s_num])
                max_num_of_levels = num_of_levels * (num_of_levels - 1) / 2
                print(file_name + " Spectroscopic number " + s_num + ": max possible transitions:" + str(
                    max_num_of_levels) + ", actually: " + str(spect_num_data_num_of_lines[s_num]))
                if max_num_of_levels < spect_num_data_num_of_lines[s_num]:
                    print("ERROR")
                    warn_f.write(file_name + " Spectroscopic number " + s_num + ": max possible transitions:" + str(
                        max_num_of_levels) + ", actually: " + str(spect_num_data_num_of_lines[s_num]) + os.linesep)
                    warn_f.close()
                    exit(1)


def create_bcfp(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, BCFP_HEADER, "BCFP.INP", "BCFP.INP",
                 {8: lambda n: translation_table[n], 20: lambda n: translation_table[str(int(n) + 1)]})
    shutil.copyfile(os.path.join(out_dir, "BCFP.INP"), os.path.join(out_dir, "BCFP.INP.before.AIW"))
    copy_from_aiw(out_dir)
    sort_file_by_levels(out_dir, "BCFP.INP", 0, 1, 3, 2)


def create_rrec(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, "", "RREC.INP", "output_ph.dat",
                 {5: lambda n: translation_table[n], 10: lambda n: translation_table[str(int(n) + 1)]},
                 "REC", rrec_line_improver)
    sort_file_by_levels(out_dir, "RREC.INP", 0, 1, 2, 0)


def create_excit(out_dir, spec_numbers, translation_table):
    create_union(out_dir, spec_numbers, EXCIT_HEADER, "EXCIT.INP", "outpp.dat",
                 {6: lambda n: translation_table[n], 11: lambda n: translation_table[n]}, "EXC", excit_line_improver)
    sort_file_by_levels(out_dir, "EXCIT.INP", 0, 1, 2, 2)


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
