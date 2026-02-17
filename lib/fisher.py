import os
import shutil

from lib.create_qsege import create_qsege
from lib.cross_section import get_constants_for_bernshtam_ralchenko
from lib.in1 import IN1
from lib.utils import skip_n_lines

HEADER = "  iSS  iQS  fSS  fQS                     Electron Impact Ionization                    Mthd                         Photoionization                Threshold\n"


def createIonFile(element, levels_num, o_dir, in1_data, bcfp):
    dir_path = os.path.join(o_dir, "fisher")
    bcfp_file_path = os.path.join(o_dir, bcfp)
    rrec_file_path = os.path.join(o_dir, "RREC.INP")
    out_file_path = os.path.join(dir_path, "Inz" + element + levels_num + ".INP")
    print("Creation of " + out_file_path)
    levels_to_bcfp_rrec = {}
    with open(bcfp_file_path, "r") as bcfp_f:
        header = bcfp_f.readline()
        if "Coefficient" in header:
            bcfp_from_databases = True
        else:
            bcfp_from_databases = False
        skip_n_lines(bcfp_f, 1)
        for line in bcfp_f:
            parts = line.split()
            if len(parts) > 4:
                level = (parts[0], parts[1], parts[3])
                if parts[1] != "0":
                    levels_to_bcfp_rrec[level] = (line, None)

    with open(rrec_file_path, "r") as rrec_f:
        skip_n_lines(rrec_f, 2)
        for line in rrec_f:
            parts = line.split()
            level = (parts[0], parts[1], parts[2])
            if level in levels_to_bcfp_rrec:
                levels_to_bcfp_rrec[level] = (levels_to_bcfp_rrec[level][0], line)
            else:
                levels_to_bcfp_rrec[level] = (None, line)
    sorted_levels = sorted(levels_to_bcfp_rrec.keys(), key=lambda x: tuple(map(int, x)))

    with (open(out_file_path, "w") as outf):
        outf.write(HEADER)
        for level in sorted_levels:
            (bcfp_line, rrec_line) = levels_to_bcfp_rrec[level]
            spectr_num_low = level[0]
            level_low = level[1]
            spectr_num_high = str(int(spectr_num_low) + 1)
            level_high = level[2]
            if bcfp_line is not None and bcfp_from_databases:
                branching_ration = float(bcfp_line.split()[4])
            else:
                branching_ration = in1_data.get_branching_ratio(spectr_num_high, level_high)
            if bcfp_line is None or bcfp_from_databases:
                bcfp_line = "%d %d %d %d 0.000E+00 0.000E+00 0.000E+00 0.000E+00" % (
                    int(spectr_num_low), int(level_low), int(spectr_num_high), int(level_high))
                # print("BCFP line is None for " + str(level))
            bcfp_parts = bcfp_line.split()
            bcfp_a = bcfp_parts[4]
            bcfp_b = bcfp_parts[5]
            bcfp_c = bcfp_parts[6]
            bcfp_d = bcfp_parts[7]
            from_sp = bcfp_parts[0]
            from_level = bcfp_parts[1]
            to_sp = bcfp_parts[2]
            to_level = bcfp_parts[3]
            ionization_energy = in1_data.get_ionization_energy(from_sp, from_level, to_sp, to_level)  # Changed
            from_config = in1_data.get_config(from_sp, from_level)  # Changed
            to_config = in1_data.get_config(to_sp, to_level)  # Changed
            (c_l, delta_l, num_of_electrons) = get_constants_for_bernshtam_ralchenko(
                from_config,
                to_config)
            if c_l:
                params = " %13.3e %13.3e %4d %13.3f %13.3e" % (
                    c_l, delta_l, num_of_electrons, ionization_energy, branching_ration)
            else:
                params = " %13.3e %13.3e %4d %13.3f %13.3e" % (0.0, 0.0, 0, 0.0, 0.0)

            bcfp_fit = " %4s %4s %4s %4s %14s %15s %15s %15s" % (
                spectr_num_low, level_low, spectr_num_high, level_high, bcfp_a, bcfp_b,
                bcfp_c, bcfp_d)
            if in1_data.contains_energy(spectr_num_high, level_high) and rrec_line is not None:
                energy = in1_data.get_ionization_energy(spectr_num_low, level_low,
                                                        spectr_num_high, level_high)
                rrec_parts = rrec_line.split()
                rrec_fit = " %6s %13s %12s %12s %12s %13.3f" % (
                    rrec_parts[3], rrec_parts[4], rrec_parts[5], rrec_parts[6], rrec_parts[7], energy)
                outf.write(bcfp_fit + rrec_fit  + "\n")


def run_qsege(dont_run_all_tools, min_sp_num, max_sp_num, element, o_dir, use_fac_lev=True):
    dir_path = os.path.join(o_dir, "fisher")
    if not os.path.isdir(dir_path):
        os.mkdir(dir_path)
    file_path = os.path.join(dir_path, "QSs" + element + ".inp")
    print("Creation of " + file_path)
    if use_fac_lev:
        create_qsege(os.path.join(o_dir, "IN1.INP"), min_sp_num, max_sp_num, o_dir, file_path)
    else:
        create_qsege(os.path.join(o_dir, "IN1.INP"), min_sp_num, max_sp_num, None, file_path)

    with open(file_path, 'r') as inf:
        for line in inf:
            parts = line.split()
    levels_num = parts.pop()
    new_file_path = os.path.join(dir_path, "QSs" + element + levels_num + ".inp")
    shutil.move(file_path, new_file_path)
    return levels_num


def run_for_fisher(dont_run_all_tools, min_sp_num, max_sp_num, element, o_dir, bcfp="BCFP.INP.before.AIW",
                   use_fac_lev=True):
    path_to_in1_inp = os.path.join(o_dir, "IN1.INP")
    in1_data = IN1(path_to_in1_inp)
    levels_num = run_qsege(dont_run_all_tools, min_sp_num, max_sp_num, element, o_dir, use_fac_lev)
    createIonFile(element, levels_num, o_dir, in1_data, bcfp)


if __name__ == "__main__":
    run_for_fisher(True, 1, 9, "Ge", "C:\\work4\\tmp\Ge-new-long", "BCFP.INP", True)
