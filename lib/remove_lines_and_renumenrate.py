import copy
import os
import shutil
from collections import namedtuple

from lib.utils import skip_n_lines

Field = namedtuple('Field', 'start end')
Level = namedtuple('Level', 'sp_num level_num')


def rreplace(s, old, new):
    return (s[::-1].replace(old[::-1], new[::-1], 1))[::-1]


def sp_num_fun(start, end): return lambda s: s[start:end]


def replace_in_file(file_name, num_skip_lines, sp_num_levels_columns, mapping):
    backup_file = file_name + ".backup2"
    shutil.copyfile(file_name, backup_file)
    after_renumerate_file = file_name + ".renumerated"
    count = 0
    with open(after_renumerate_file, "w") as fwrite:
        with open(file_name, "r") as f:
            for l in f:
                if count < num_skip_lines:
                    fwrite.write(l)
                else:
                    for sp_num_level in sp_num_levels_columns:
                        sp_num = sp_num_level.sp_num(l).strip()
                        level_start = sp_num_level.level_num.start
                        level_end = sp_num_level.level_num.end
                        level = l[level_start:level_end].strip()
                        if level in mapping[sp_num]:
                            new_level = mapping[sp_num][level]
                            new_level_formatted = ("%" + str(level_end - level_start) + "s") % new_level
                            l = l[:level_start] + new_level_formatted + l[level_end:]
                        else:
                            print("Missing " + level + " in " + sp_num + " in " + file_name)
                    fwrite.write(l)

                count += 1

    shutil.copyfile(after_renumerate_file, file_name)


def remove_in_file(file_name, num_skip_lines, sp_num_levels_columns, used):
    backup_file = file_name + ".backup1"
    shutil.copyfile(file_name, backup_file)
    after_remove_file = file_name + ".removed"
    count = 0
    with open(after_remove_file, "w") as fwrite:
        with open(file_name, "r") as f:
            for l in f:
                if count < num_skip_lines:
                    fwrite.write(l)
                else:
                    copy_line = True
                    for sp_num_level in sp_num_levels_columns:
                        sp_num = sp_num_level.sp_num(l).strip()
                        level_start = sp_num_level.level_num.start
                        level_end = sp_num_level.level_num.end
                        level = l[level_start:level_end].strip()
                        if level not in used[sp_num]:
                            copy_line = False

                    if copy_line:
                        fwrite.write(l)

                count += 1

    shutil.copyfile(after_remove_file, file_name)


def remove_large(file_name, num_skip_lines, columns, abs_max_value):
    backup_file = file_name + ".backup3"
    shutil.copyfile(file_name, backup_file)
    after_remove_file = file_name + ".removed-large"
    count = 0
    removed = 0
    with open(after_remove_file, "w") as fwrite:
        with open(file_name, "r") as f:
            for l in f:
                if count < num_skip_lines:
                    fwrite.write(l)
                else:
                    copy_line = True
                    parts = l.split()
                    for col in columns:
                        value = abs(float(parts[col]))
                        if value > abs_max_value:
                            copy_line = False

                    if copy_line:
                        fwrite.write(l)
                    else:
                        removed += 1

                count += 1

    shutil.copyfile(after_remove_file, file_name)
    return removed


def read_used_lines(file_name, num_skip_lines, sp_num_levels_columns, used_lines):
    ret = copy.deepcopy(used_lines)
    with open(file_name, "r") as f:
        skip_n_lines(f, num_skip_lines)
        for l in f:
            for sp_num_level in sp_num_levels_columns:
                sp_num = sp_num_level.sp_num(l).strip()
                level = l[sp_num_level.level_num.start:sp_num_level.level_num.end].strip()
                if sp_num not in ret:
                    ret[sp_num] = set()
                levels = ret[sp_num]
                levels.add(level)
    return ret


def remove_lines_from_in1_inp(in1_path, used_lines):
    backup_file = in1_path + ".backup1"
    shutil.copyfile(in1_path, backup_file)
    after_removal_file = in1_path + ".removed"
    after_removal_file_level_numbers = in1_path + ".removed-levels"
    num_of_lines_per_sp_num = {}
    with open(after_removal_file, "w") as fwrite:
        with open(in1_path, "r") as f:
            for l in f:
                fields = l.split()
                if len(fields) == 1:
                    sp_num = fields[0]
                    num_of_lines_per_sp_num[sp_num] = 0
                if len(fields) != 8:
                    fwrite.write(l)
                else:
                    level = fields[7]
                    if sp_num in used_lines and level in used_lines[sp_num]:
                        fwrite.write(l)
                        num_of_lines_per_sp_num[sp_num] = num_of_lines_per_sp_num[sp_num] + 1
                    else:
                        print("Removed " + sp_num + ":" + level)

    with open(after_removal_file_level_numbers, "w") as fwrite:
        with open(after_removal_file, "r") as f:
            for l in f:
                fields = l.split()
                if len(fields) == 10 and fields[0] in used_lines:
                    sp_num = fields[0]
                    old_num_of_levels = fields[1]
                    new_num_of_levels = num_of_lines_per_sp_num[sp_num]
                    l = l.replace(old_num_of_levels, ("%" + str(len(old_num_of_levels)) + "s") % str(new_num_of_levels))
                fwrite.write(l)

    shutil.copyfile(after_removal_file_level_numbers, in1_path)


def renumerate_in1_inp(in1_path):
    ret = {}
    backup_file = in1_path + ".backup2"
    shutil.copyfile(in1_path, backup_file)
    after_renumerate_file = in1_path + ".renumerated"
    with open(after_renumerate_file, "w") as fwrite:
        with open(in1_path, "r") as f:
            for l in f:
                fields = l.split()
                if len(fields) == 1:
                    sp_num = fields[0]
                    if sp_num not in ret:
                        ret[sp_num] = {}
                    level_num = 0
                if len(fields) != 8:
                    fwrite.write(l)
                else:
                    level = fields[7]
                    level_num += 1
                    new_level = str(level_num)
                    ret[sp_num][level] = new_level
                    fwrite.write(rreplace(l, level, new_level))
    shutil.copyfile(after_renumerate_file, in1_path)
    return ret


def remove_unused_lines_and_renumerate(elem_dir):
    in1_path = os.path.join(elem_dir, "IN1.INP")
    rrec_path = os.path.join(elem_dir, "RREC.INP")
    excit_path = os.path.join(elem_dir, "EXCIT.INP")
    spectr_path = os.path.join(elem_dir, "SPECTR.INP")
    bcfp_path = os.path.join(elem_dir, "BFCP.INP")
    used_lines = {}
    # used_lines = read_used_lines(rrec_path, 0, [Level(sp_num_fun(0, 3), Field(4, 10)),
    #                                            Level(lambda s: str(int(s[0:3]) + 1), Field(11, 17))], used_lines)
    used_lines = read_used_lines(excit_path, 2,
                                 [Level(sp_num_fun(0, 3), Field(4, 9)), Level(sp_num_fun(0, 3), Field(10, 15))],
                                 used_lines)
    # used_lines = read_used_lines(spectr_path, 1, [Level(sp_num_fun(0, 3), Field(4, 7))], used_lines)
    # used_lines = read_used_lines(bcfp_path, 2,
    #                             [Level(sp_num_fun(0, 5), Field(6, 10)), Level(sp_num_fun(11, 15), Field(16, 20))],
    #                             used_lines)
    # print(used_lines)
    remove_lines_from_in1_inp(in1_path, used_lines)
    remove_in_file(rrec_path, 0, [Level(sp_num_fun(0, 3), Field(4, 10)),
                                  Level(lambda s: str(int(s[0:3]) + 1), Field(11, 17))], used_lines)

    remove_in_file(spectr_path, 1, [Level(sp_num_fun(0, 3), Field(4, 7))], used_lines)
    remove_in_file(bcfp_path, 2, [Level(sp_num_fun(0, 5), Field(6, 10)), Level(sp_num_fun(11, 15), Field(16, 20))],
                   used_lines)

    replaces = renumerate_in1_inp(in1_path)
    replace_in_file(rrec_path, 0, [Level(sp_num_fun(0, 3), Field(4, 10)),
                                   Level(lambda s: str(int(s[0:3]) + 1), Field(11, 17))], replaces)
    replace_in_file(excit_path, 3, [Level(sp_num_fun(0, 3), Field(4, 9)), Level(sp_num_fun(0, 3), Field(10, 15))],
                    replaces)
    replace_in_file(spectr_path, 1, [Level(sp_num_fun(0, 3), Field(4, 7))], replaces)
    replace_in_file(bcfp_path, 2, [Level(sp_num_fun(0, 5), Field(6, 10)), Level(sp_num_fun(11, 15), Field(16, 20))],
                    replaces)

    return replaces
