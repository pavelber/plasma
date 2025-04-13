import os
import shutil

from lib.create_qsege import create_qsege
from lib.cross_section import get_constants_for_bernshtam_ralchenko
from lib.in1 import get_ionization_energy, create_tables
from lib.utils import skip_n_lines

HEADER = "  iSS  iQS  fSS  fQS                     Electron Impact Ionization                    Mthd                         Photoionization                Threshold\n"


def compute_energy_diff(spectr_num_low, level_low, spectr_num_high, level_high, spectr_num_to_aion_energy,
                        spectr_num_level_to_energy):
    return spectr_num_level_to_energy[(spectr_num_high, level_high)] - \
        spectr_num_level_to_energy[(spectr_num_low, level_low)] + \
        spectr_num_to_aion_energy[spectr_num_low]


def createIonFile(element, levels_num, o_dir, spectr_num_to_aion_energy,
                  spectr_num_level_to_energy,
                  transitions_energy_table, configurations_table, ionization_potential,
                  bcfp):
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

    with open(out_file_path, "w") as outf:
        outf.write(HEADER)
        for level in sorted_levels:
            (bcfp_line, rrec_line) = levels_to_bcfp_rrec[level]
            spectr_num_low = level[0]
            level_low = level[1]
            spectr_num_high = str(int(spectr_num_low) + 1)
            level_high = level[2]
            if bcfp_from_databases:
                branching_ration = float(bcfp_line.split()[4])
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
            transition_energy = get_ionization_energy(from_sp, from_level, to_sp, to_level, transitions_energy_table,
                                                      ionization_potential)
            from_config = configurations_table[(from_sp, from_level)]
            to_config = configurations_table[(to_sp, to_level)]
            if bcfp_from_databases:
                (c_l, delta_l, num_of_electrons, ionization_energy) = get_constants_for_bernshtam_ralchenko(
                    transition_energy,
                    from_config,
                    to_config)
            else:
                c_l = None
            if c_l:
                params = " %13.3e %13.3e %4d %13.3f %13.3e" % (
                c_l, delta_l, num_of_electrons, ionization_energy, branching_ration)
            else:
                params = " %13.3e %13.3e %4d %13.3f %13.3e" % (0.0, 0.0, 0, 0.0, 0.0)

            bcfp_fit = " %4s %4s %4s %4s %14s %15s %15s %15s" % (
                spectr_num_low, level_low, spectr_num_high, level_high, bcfp_a, bcfp_b,
                bcfp_c, bcfp_d)
            if (spectr_num_high, level_high) in spectr_num_level_to_energy and rrec_line is not None:
                energy = compute_energy_diff(spectr_num_low, level_low, spectr_num_high, level_high,
                                             spectr_num_to_aion_energy,
                                             spectr_num_level_to_energy)
                rrec_parts = rrec_line.split()
                rrec_fit = " %6s %13s %12s %12s %12s %13.3f" % (
                    rrec_parts[3], rrec_parts[4], rrec_parts[5], rrec_parts[6], rrec_parts[7], energy)
                outf.write(bcfp_fit + rrec_fit + params + "\n")


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


def get_energy_from_in1_inp(file_path):
    ain_levels_lines = False
    energy_lines = False
    ain_energy = {}
    levels_energy = {}

    with open(file_path, 'r') as inf:
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
            if energy_lines and len(parts) == 8:
                levels_energy[(spect_num, parts[7])] = float(parts[4])
            if energy_lines and len(parts) == 9:
                levels_energy[(spect_num, parts[8])] = float(parts[4])
            if energy_lines and parts[0] == "Nucleus":
                levels_energy[(spect_num, '1')] = 0.0
    return ain_energy, levels_energy


def run_for_fisher(dont_run_all_tools, min_sp_num, max_sp_num, element, o_dir, bcfp="BCFP.INP.before.AIW",
                   use_fac_lev=True):
    path_to_in1_inp = os.path.join(o_dir, "IN1.INP")
    (spectr_num_to_aion_energy, spectr_num_level_to_energy) = get_energy_from_in1_inp(path_to_in1_inp)
    (transitions_energy_table, ionization_potential, configurations_table, stat_weight,
     branching_ratio) = create_tables(path_to_in1_inp)
    next_sp_num = str(int(max_sp_num) + 1)
    if (next_sp_num, "1") not in spectr_num_level_to_energy:
        spectr_num_level_to_energy[(next_sp_num, "1")] = 1.0
    levels_num = run_qsege(dont_run_all_tools, min_sp_num, max_sp_num, element, o_dir, use_fac_lev)
    createIonFile(element, levels_num, o_dir,
                  spectr_num_to_aion_energy, spectr_num_level_to_energy,
                  transitions_energy_table, configurations_table, ionization_potential,
                  bcfp)


if __name__ == "__main__":
    run_for_fisher(True, 1, 9, "O", "C:\\Users\\javaa\\Downloads\\", "BCFP-formula.INP", False)
