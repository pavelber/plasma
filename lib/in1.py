from lib.utils import skip_n_lines


class IN1:
    def __init__(self, in1_path):
        self._ionization_potential = {}
        self._configs = {}
        self._stat_weight = {}
        self._branching_ratio = {}
        self.energy_table = {}
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
                        self.energy_table[(current_sp_num, "1")] = 0.0
                        self._configs[(current_sp_num, "1")] = ["1s0"]
                    elif "Autoionizating states" in line:
                        continue
                    else:
                        level = line[59:66].strip()
                        energy = float(line[27:36].strip())
                        g = float(line[17:23].strip())
                        config = line[0:10].split()

                        self.energy_table[(current_sp_num, level)] = energy
                        self._configs[(current_sp_num, level)] = config
                        self._stat_weight[(current_sp_num, level)] = g

                        if tuple(config) not in self._branching_ratio[current_sp_num]:
                            self._branching_ratio[current_sp_num][tuple(config)] = []

                        self._branching_ratio[current_sp_num][tuple(config)].append(level)

    def get_ionization_energy(self, from_sp, from_level, to_sp, to_level):
        """Calculate ionization energy for a transition."""
        try:
            return (self._ionization_potential[from_sp] -
                    self.energy_table[(from_sp, from_level)] +
                    self.energy_table[(to_sp, to_level)])
        except KeyError as e:
            raise ValueError(f"Invalid sp num or level: {e}")

    def get_branching_ratio(self, sp_num, level):
        """Calculate branching ratio for a sp num and configuration."""
        config = self._configs[(sp_num, level)]
        config = tuple(config)  # Convert list to tuple for lookup
        try:
            levels = self._branching_ratio[sp_num][config]
            sum_of_stat_weights = sum(self._stat_weight[(sp_num, level)] for level in levels)
            return self._stat_weight[(sp_num, levels[0])] / sum_of_stat_weights
        except KeyError as e:
            raise ValueError(f"Invalid sp num or configuration: {e}")

    def get_config(self, sp_num, level):
        """Get configuration for a sp num and level."""
        try:
            return self._configs[(sp_num, level)]
        except KeyError as e:
            raise ValueError(f"Invalid sp num or level: {e}")

    def get_stat_weight(self, sp_num, level):
        """Get statistical weight for a sp num and level."""
        try:
            return self._stat_weight[(sp_num, level)]
        except KeyError as e:
            raise ValueError(f"Invalid sp num or level: {e}")

    def get_ionization_potential(self, sp_num):
        """Get ionization potential for a sp num."""
        try:
            return self._ionization_potential[sp_num]
        except KeyError as e:
            raise ValueError(f"Invalid sp num: {e}")

    def get_energy(self, sp_num, level):
        """Get energy for a sp num and level."""
        try:
            return self.energy_table[(sp_num, level)]
        except KeyError as e:
            raise ValueError(f"Invalid sp num or level: {e}")

    def contains_energy(self, sp_num, level):
        """Check if energy exists for a sp num and level."""
        return (sp_num, level) in self.energy_table
