import csv
import os
import shutil
import sys
from shutil import copy
from subprocess import Popen, PIPE


def error(s):
    sys.stderr.write(s + '\n')
    exit(1)


def warn(out_dir, s):
    warnings_file_path = os.path.join(out_dir, "WARNINGS.txt")
    with open(warnings_file_path, 'ab') as warnf:
        warnf.write("WARNING: " + s + "\n")
        print("WARNING: " + s)


def info(out_dir, s):
    warnings_file_path = os.path.join(out_dir, "WARNINGS.txt")
    with open(warnings_file_path, 'ab') as warnf:
        warnf.write("INFO: " + s + "\n")
        # print("INFO: " + s)


def runcommand(cmd, cwd=".", cmd_in=None):
    proc = Popen(cmd,
                 stdout=PIPE,
                 stderr=PIPE,
                 stdin=PIPE,
                 shell=True,
                 cwd=cwd,
                 universal_newlines=True)
    std_out, std_err = proc.communicate(cmd_in)
    code = proc.returncode
    if code != 0:
        error("**** Failed.\n" + std_out + "\n" + std_err)
    return code, std_out, std_err


def copy_and_run(exe, prefix, out_dir, cwd=".", cmd_in=None, args=""):
    if not os.path.exists(out_dir):
        error(out_dir + " not exists")
    copy(exe, out_dir)
    path_to, file_name = os.path.split(exe)
    cmd = prefix + " " + out_dir + os.path.sep + file_name + " " + args
    print(cmd)
    code, std_out, std_err = runcommand(cmd, cwd, cmd_in)
    return code, std_out, std_err


def read_table():
    dict1 = {}
    dict2 = {}
    path = os.path.dirname(__file__)
    if path == "" or path is None:
        path = "."
    table_file = path + os.path.sep + "PeriodicTable.csv"
    with open(table_file, 'rb') as infile:
        reader = csv.reader(infile)
        headers = next(reader)[0:]
        for row in reader:
            dict1[row[2]] = {key: value for key, value in zip(headers, row[0:])}
            dict2[row[0]] = {key: value for key, value in zip(headers, row[0:])}
    return dict1, dict2


def skip_n_lines(f, num):
    for _ in range(num):
        next(f)


def skip_lines(f):
    skip_n_lines(f, 12)


def create_level_key(level):
    l_int = int(level)
    if l_int > 0:
        return level.zfill(5)
    else:
        return "z" + str(abs(l_int)).zfill(5)


def sort_file_by_levels(out_dir, file_name, s_num_index, from_level_index, to_level_index, skip_lines,
                        count_transitions=False):
    file_path = os.path.join(out_dir, file_name)
    file_path_not_sorted = os.path.join(out_dir, file_name + ".notsorted")
    shutil.copyfile(file_path, file_path_not_sorted)
    lines = {}
    keys = []
    spect_num_data_min_level = {}
    spect_num_data_max_level = {}
    spect_num_data_num_of_lines = {}

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
                key = s_num.zfill(5) + "-" + create_level_key(from_level) + "-" + create_level_key(to_level)
                if key in lines:
                    warn(out_dir, file_name + " duplicate line:\n\t" + line)
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
            if count_transitions:
                if max_num_of_levels < spect_num_data_num_of_lines[s_num]:
                    warn(out_dir, file_name + " Spectroscopic number " + s_num + ": max possible transitions:" + str(
                        max_num_of_levels) + ", actually: " + str(spect_num_data_num_of_lines[s_num]))
                    exit(1)
                else:
                    info(out_dir, file_name + " Spectroscopic number " + s_num + ": max possible transitions:" + str(
                        max_num_of_levels) + ", actually: " + str(
                        spect_num_data_num_of_lines[s_num]) + "... OK")


def read_element(in_dir):
    in_path = in_dir + os.path.sep + "fac.lev"
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
