import os
import shutil

from lib.utils import runcommand, skip_n_lines

HEADER = "  iSS  iQS  fSS  fQS                     Electron Impact Ionization                    Mthd                         Photoionization                Threshold\n"


def createIonFile(dont_run_all_tools, element, levels_num, o_dir):
    dir_path = os.path.join(o_dir, "fisher")
    in_file_path_1 = os.path.join(o_dir, "BCFP.INP.before.AIW")
    in_file_path_2 = os.path.join(o_dir, "RREC.INP")
    out_file_path = os.path.join(dir_path, "Inz" + element + levels_num + ".INP")
    print("Creation of " + out_file_path)
    levels_to_bcfp = {}
    with open(in_file_path_1, "rb") as inf1:
        for line1 in inf1:
            parts1 = line1.split()
            if len(parts1) > 4:
                id_by = (parts1[0], parts1[1], parts1[3])
                levels_to_bcfp[id_by] = line1
    with open(in_file_path_2, "rb") as inf2:
        skip_n_lines(inf2, 2)
        with open(out_file_path, "wb") as outf:
            outf.write(HEADER)
            for line2 in inf2:
                parts2 = line2.split()
                id_by = (parts2[0], parts2[1], parts2[2])
                if not id_by in levels_to_bcfp:
                    print id_by
                else:
                    bcfp_parts = levels_to_bcfp[id_by].split()
                    bsfp = " %4s %4s %4s %4s %14s %15s %15s %15s" % (
                        bcfp_parts[0], bcfp_parts[1], bcfp_parts[2], bcfp_parts[3], bcfp_parts[4], bcfp_parts[5],
                        bcfp_parts[6], bcfp_parts[7])
                    rrec = " %6s %13s %12s %12s %12s %14s" % (
                        parts2[3], parts2[4], parts2[5], parts2[6], parts2[7], parts2[8])
                    outf.write(bsfp + rrec+"\n")


def run_qsege(dont_run_all_tools, python_path, qsege_path, element, o_dir):
    dir_path = os.path.join(o_dir, "fisher")
    if not dont_run_all_tools:
        os.mkdir(dir_path)
    file_path = os.path.join(dir_path, "QSs" + element + ".inp")
    print("Creation of " + file_path)
    with open(file_path, 'wb') as outf:
        code, std_out, std_err = runcommand(python_path + " " + qsege_path + " IN1.INP " + o_dir, o_dir)
        outf.write(std_out)
        print (std_err)
    with open(file_path, 'rb') as inf:
        for line in inf:
            parts = line.split()
    levels_num = parts.pop()
    new_file_path = os.path.join(dir_path, "QSs" + element + levels_num + ".inp")
    shutil.move(file_path, new_file_path)
    return levels_num


def run_for_fisher(dont_run_all_tools, python_path, qsege_path, element, o_dir):
    levels_num = run_qsege(dont_run_all_tools, python_path, qsege_path, element, o_dir)
    createIonFile(dont_run_all_tools, element, levels_num, o_dir)
