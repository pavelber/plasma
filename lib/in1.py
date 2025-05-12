from lib.exceptions import GenericPlasmaException
from lib.utils import skip_n_lines


class IN1:
    def __init__(self, in1_path):
        self._ionization_potential = {}
        self._configs = {}
        self._stat_weight = {}
        self._branching_ratio = {}
        self._energy_table = {}
        self._levels_per_sp_num = {}
        self._load_data(in1_path)

    def _load_data(self, in1_path):
        """Load and parse the input file."""
        with open(in1_path, 'r') as infile:
            skip_n_lines(infile, 13)
            is_header = True
            current_sp_num = None

            for line in infile:
                parts = line.split()
                if len(parts) == 1:
                    is_header = False
                    current_sp_num = parts[0]
                    if current_sp_num not in self._branching_ratio:
                        self._branching_ratio[current_sp_num] = {}
                elif is_header:
                    self._ionization_potential[parts[0]] = float(parts[4])
                else:
                    if "Nucleus" in line:
                        self._energy_table[(current_sp_num, "1")] = 0.0
                        self._configs[(current_sp_num, "1")] = ["1s0"]
                    elif "Autoionizating states" in line:
                        continue
                    else:
                        level = line[59:66].strip()
                        energy = float(line[27:37].strip())
                        g = float(line[17:23].strip())
                        config = line[0:10].split()

                        self._energy_table[(current_sp_num, level)] = energy
                        self._configs[(current_sp_num, level)] = config
                        self._stat_weight[(current_sp_num, level)] = g
                        if current_sp_num not in self._levels_per_sp_num:
                            self._levels_per_sp_num[current_sp_num] = []
                        self._levels_per_sp_num[current_sp_num].append(level)

                        if tuple(config) not in self._branching_ratio[current_sp_num]:
                            self._branching_ratio[current_sp_num][tuple(config)] = []
                        self._branching_ratio[current_sp_num][tuple(config)].append(level)

    def get_ionization_energy(self, from_sp, from_level, to_sp, to_level):
        """Calculate ionization energy for a transition."""
        try:
            return (self._ionization_potential[from_sp] -
                    self._energy_table[(from_sp, from_level)] +
                    self._energy_table[(to_sp, to_level)])
        except KeyError as e:
            raise ValueError(f"Invalid species or level: {e}")

    def get_branching_ratio(self, sp_num, level):
        """Calculate branching ratio for a species and configuration."""
        config = self._configs[(sp_num, level)]
        config = tuple(config)  # Convert list to tuple for lookup
        try:
            levels = self._branching_ratio[sp_num][config]
            sum_of_stat_weights = sum(self._stat_weight[(sp_num, level)] for level in levels)
            return self._stat_weight[(sp_num, levels[0])] / sum_of_stat_weights
        except KeyError as e:
            raise ValueError(f"Invalid species or configuration: {e}")

    def get_config(self, sp_num, level):
        """Get configuration for a species and level."""
        try:
            return self._configs[(sp_num, level)]
        except KeyError as e:
            raise ValueError(f"Invalid species or level: {e}")

    def get_stat_weight(self, sp_num, level):
        """Get statistical weight for a species and level."""
        try:
            return self._stat_weight[(sp_num, level)]
        except KeyError as e:
            raise ValueError(f"Invalid species or level: {e}")

    def get_ionization_potential(self, sp_num):
        """Get ionization potential for a species."""
        try:
            return self._ionization_potential[sp_num]
        except KeyError as e:
            raise ValueError(f"Invalid species: {e}")

    def get_energy(self, sp_num, level):
        """Get energy for a species and level."""
        try:
            return self._energy_table[(sp_num, level)]
        except KeyError as e:
            raise ValueError(f"Invalid species or level: {e}")

    def contains_energy(self, sp_num, level):
        """Check if energy exists for a species and level."""
        return (sp_num, level) in self._energy_table

    def get_sp_numbers(self):
        """Get all spectroscopic numbers."""
        return sorted(self._ionization_potential.keys())

    def get_levels(self, sp_num):
        """Get all levels for a given spectroscopic number."""
        if sp_num not in self._levels_per_sp_num:
            return None
        return self._levels_per_sp_num[sp_num]

    def find_level_by_energy_statweight_config(self, sp_num, energy, stat_weight, config, percent_energy_match=0.05):
        """Find level by energy, statistical weight, and configuration."""
        found = []
        for level in self._levels_per_sp_num[sp_num]:
            spnum_level = (sp_num, level)
            if (energy > 0.0 and
                    abs(self._energy_table[spnum_level] - energy) / energy < percent_energy_match and
                    self._stat_weight[spnum_level] == stat_weight and
                    self._configs[spnum_level] == config):
                found.append(level)
        if len(found) == 0:
            return None
        closest_level = min(
            found,
            key=lambda l: abs(self._energy_table[(sp_num, l)] - energy)
        )

        return closest_level

    @staticmethod
    def test_number_of_levels_inp1(in1_inp):
        with open(in1_inp, "r") as f:
            num_per_sp = {}
            for l in f:
                fields = l.split()
                if len(fields) == 1:
                    if 'sp_num' in locals() and num_per_sp[sp_num] != level_num:
                        raise GenericPlasmaException("Level number in " + sp_num)
                    if 'sp_num' in locals() and level_num == 0:
                        raise GenericPlasmaException("Level number 0 in " + sp_num)
                    sp_num = fields[0]
                    level_num = 0
                if len(fields) == 10 and fields[9] == '0.000':
                    num_in_header = int(fields[1])
                    if num_in_header == 0:
                        raise GenericPlasmaException("Level number 0 in " + sp_num)
                    num_per_sp[fields[0]] = num_in_header
                if len(fields) == 7:
                    fields.insert(0, "")
                if len(fields) == 8:
                    level_num += 1
                    if int(fields[7]) != level_num:
                        raise GenericPlasmaException("Level number in " + l)
