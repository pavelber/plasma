import os
import shutil

from lib.utils import runcommand, skip_n_lines

HEADER = "  iSS  iQS  fSS  fQS                     Electron Impact Ionization                    Mthd                         Photoionization                Threshold\n"


def compute_energy_diff(spectr_num_low, level_low, spectr_num_high, level_high, spectr_num_to_aion_energy,
                        spectr_num_level_to_energy):
    return spectr_num_level_to_energy[(spectr_num_high, level_high)] - \
           spectr_num_level_to_energy[(spectr_num_low, level_low)] + \
           spectr_num_to_aion_energy[spectr_num_low]


def createIonFile(dont_run_all_tools, element, levels_num, o_dir, spectr_num_to_aion_energy,
                  spectr_num_level_to_energy):
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
                    spectr_num_low = bcfp_parts[0]
                    level_low = bcfp_parts[1]
                    spectr_num_high = bcfp_parts[2]
                    level_high = bcfp_parts[3]
                    bsfp = " %4s %4s %4s %4s %14s %15s %15s %15s" % (
                        spectr_num_low, level_low, spectr_num_high, level_high, bcfp_parts[4], bcfp_parts[5],
                        bcfp_parts[6], bcfp_parts[7])
                    energy = compute_energy_diff(spectr_num_low, level_low, spectr_num_high, level_high,
                                                 spectr_num_to_aion_energy,
                                                 spectr_num_level_to_energy)
                    rrec = " %6s %13s %12s %12s %12s %14.2f" % (
                        parts2[3], parts2[4], parts2[5], parts2[6], parts2[7], energy)
                    outf.write(bsfp + rrec + "\n")


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


def get_energy_from_in1_inp(file_path):
    ain_levels_lines = False
    energy_lines = False
    ain_energy = {}
    levels_energy = {}

    with open(file_path, 'rb') as inf:
        for line in inf:
            parts = line.split()
            if ain_levels_lines and len(parts) > 4:
                ain_energy[parts[0]] = float(parts[4])
            if len(parts) > 0 and parts[0] == 'Step=':
                ain_levels_lines = True
            if len(parts) == 1:
                spect_num = parts[0]
                ain_levels_lines = False
                energy_lines = True
            if energy_lines and len(parts) == 7:
                levels_energy[(spect_num, parts[6])] = float(parts[3])
            if energy_lines and parts[0] == "Nucleus":
                levels_energy[(spect_num, parts[5])] = 0.0
    return ain_energy, levels_energy


def run_for_fisher(dont_run_all_tools, python_path, qsege_path, element, o_dir):
    path_to_in1_inp = os.path.join(o_dir, "IN1.INP")
    (spectr_num_to_aion_energy, spectr_num_level_to_energy) = get_energy_from_in1_inp(path_to_in1_inp)
    levels_num = run_qsege(dont_run_all_tools, python_path, qsege_path, element, o_dir)
    createIonFile(dont_run_all_tools, element, levels_num, o_dir, spectr_num_to_aion_energy, spectr_num_level_to_energy)
