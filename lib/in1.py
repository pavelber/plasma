from lib.utils import skip_n_lines


def get_ionization_energy(from_sp, from_level, to_sp, to_level, transitions_energy_table, ionization_potential):
    return ionization_potential[from_sp] - transitions_energy_table[(from_sp, from_level)] + transitions_energy_table[(
        to_sp, to_level)]


def create_tables(in1_path):
    with open(in1_path, 'r') as infile:
        skip_n_lines(infile, 13)
        ionization_potential = {}
        configs = {}
        stat_weight = {}
        branching_ratio = {}
        transitions_energy_table = {}
        is_header = True
        for line in infile:
            parts = line.split()
            if len(parts) == 1:
                is_header = False
                sp_num = parts[0]
                if sp_num not in branching_ratio:
                    branching_ratio[sp_num] = {}
            elif is_header:
                ionization_potential[parts[0]] = float(parts[4])
            else:
                if "Nucleus" in line:
                    transitions_energy_table[(sp_num, "1")] = 0.0
                    configs[(sp_num, "1")] = ["1s0"]
                elif "Autoionizating states" in line:
                    pass
                else:
                    level = line[59:66].strip()
                    energy = float(line[29:37].strip())
                    g = float(line[17:23].strip())
                    config = line[0:10].split()
                    transitions_energy_table[(sp_num, level)] = energy
                    configs[(sp_num, level)] = config
                    stat_weight[(sp_num, level)] = g
                    if config not in branching_ratio[sp_num]:
                        branching_ratio[sp_num][config] = []
                    branching_ratio[sp_num][config].append(level)
    return transitions_energy_table, ionization_potential, configs, stat_weight, branching_ratio


def get_branching_ratio(sp_num, config, branching_ratio, stat_weight):
    levels = branching_ratio[sp_num][config]
    sum_of_stat_weights = sum(map(lambda x: stat_weight[(sp_num, x)], levels))
    return stat_weight[(sp_num, config)] / sum_of_stat_weights
