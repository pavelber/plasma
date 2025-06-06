from lib.exceptions import GenericPlasmaException
from lib.utils import skip_n_lines


class IN1:
    def __init__(self, in1_path=None):
        self._header = None  # List to store header lines
        self._ionization_potential = {}
        self._header_fifth_column = {}
        self._configs = {}
        self._stat_weight = {}
        self._branching_ratio = {}
        self._energy_table = {}
        self._levels_per_sp_num = {}
        if in1_path:
            self._load_data(in1_path)

    def _load_data(self, in1_path):
        """Load and parse the input file."""
        with open(in1_path, 'r') as infile:
            # Store the first 13 lines as the header
            self._header = [infile.readline() for _ in range(13)]

            is_header = True
            current_sp_num = None
            level_counts = {}  # Store expected regular and autoionization level counts
            sp_numbers = set()  # Track all spectroscopic numbers
            is_autoion = False  # Track if parsing autoionization levels
            regular_level_num = 0  # Counter for regular levels
            autoion_level_num = 0  # Counter for autoionization levels

            for line in infile:
                parts = line.split()
                if len(parts) == 1:
                    is_header = False
                    current_sp_num = parts[0]
                    sp_numbers.add(current_sp_num)
                    if current_sp_num not in self._branching_ratio:
                        self._branching_ratio[current_sp_num] = {}
                    if current_sp_num not in self._levels_per_sp_num:
                        self._levels_per_sp_num[current_sp_num] = []
                    is_autoion = False  # Reset for new spectroscopic number
                    regular_level_num = 0
                    autoion_level_num = 0
                elif is_header:
                    # Parse ionization potential line
                    sp_num = parts[0]
                    self._ionization_potential[sp_num] = float(parts[4])
                    self._header_fifth_column[sp_num] = int(parts[5])
                    # Store expected level counts for verification
                    regular_levels = int(parts[1])  # Second column: regular levels
                    autoion_levels = int(parts[2])  # Third column: autoionization levels
                    level_counts[sp_num] = {'regular': regular_levels, 'autoion': autoion_levels}
                else:
                    if "Nucleus" in line:
                        # Explicitly handle nucleus level
                        level = "1"
                        self._energy_table[(current_sp_num, level)] = 0.0
                        self._configs[(current_sp_num, level)] = ["1s0"]
                        self._stat_weight[(current_sp_num, level)] = 1.0  # Assume stat_weight 0 for nucleus
                        self._levels_per_sp_num[current_sp_num].append(level)
                        # Add nucleus to branching ratio
                        config = tuple(["1s0"])
                        if config not in self._branching_ratio[current_sp_num]:
                            self._branching_ratio[current_sp_num][config] = []
                        self._branching_ratio[current_sp_num][config].append(level)
                        # Verify nucleus level number
                        regular_level_num += 1
                        if level != "1":
                            raise GenericPlasmaException(
                                f"Non-sequential regular level number in {current_sp_num}: expected 1, found {level}"
                            )
                    elif "autoion" in line.lower():
                        is_autoion = True
                        continue
                    else:
                        level = line[59:68].strip()
                        energy = float(line[27:37].strip())
                        g = float(line[16:23].strip())
                        config = line[0:10].split()

                        self._energy_table[(current_sp_num, level)] = energy
                        self._configs[(current_sp_num, level)] = config
                        self._stat_weight[(current_sp_num, level)] = g
                        self._levels_per_sp_num[current_sp_num].append(level)

                        if tuple(config) not in self._branching_ratio[current_sp_num]:
                            self._branching_ratio[current_sp_num][tuple(config)] = []
                        self._branching_ratio[current_sp_num][tuple(config)].append(level)

                        # Verify level number sequence
                        try:
                            parsed_level = int(level)
                            if is_autoion:
                                autoion_level_num += 1
                                expected_level = -autoion_level_num
                                if parsed_level != expected_level:
                                    raise GenericPlasmaException(
                                        f"Non-sequential autoionization level number in {current_sp_num}: expected {expected_level}, found {parsed_level}"
                                    )
                            else:
                                regular_level_num += 1
                                expected_level = regular_level_num if parsed_level > 0 else 1
                                if parsed_level != expected_level:
                                    raise GenericPlasmaException(
                                        f"Non-sequential regular level number in {current_sp_num}: expected {expected_level}, found {parsed_level}"
                                    )
                        except ValueError:
                            pass  # Skip non-integer levels

            # Verify level counts for each spectroscopic number
            for sp_num in level_counts:
                if sp_num not in self._levels_per_sp_num:
                    raise GenericPlasmaException(f"No levels found for spectroscopic number {sp_num}")
                levels = self._levels_per_sp_num[sp_num]
                # Count regular (non-negative, including nucleus) and autoionization (negative) levels
                regular_count = sum(1 for level in levels if level == "1" or (level.isdigit() and int(level) >= 0))
                autoion_count = sum(1 for level in levels if level.startswith('-') and level[1:].isdigit())
                expected_regular = level_counts[sp_num]['regular']
                expected_autoion = level_counts[sp_num]['autoion']
                if regular_count != expected_regular:
                    raise GenericPlasmaException(
                        f"Mismatch in regular levels for {sp_num}: expected {expected_regular}, found {regular_count}"
                    )
                if autoion_count != expected_autoion:
                    raise GenericPlasmaException(
                        f"Mismatch in autoionization levels for {sp_num}: expected {expected_autoion}, found {autoion_count}"
                    )

            # Verify every spectroscopic number has an ionization potential line
            for sp_num in sp_numbers:
                if sp_num not in level_counts:
                    raise GenericPlasmaException(
                        f"Spectroscopic number {sp_num} has no ionization potential line in header"
                    )

    @classmethod
    def create_empty(cls):
        """Create an empty IN1 instance."""
        return cls()

    def add_or_replace_sp_data(self, sp_num, other_in1):
        """Add or replace all data for a given spectroscopic number from another IN1 instance.
        Returns a renumeration table mapping old levels to new levels."""
        if sp_num not in other_in1._ionization_potential:
            raise ValueError(f"Spectroscopic number {sp_num} not found in the provided IN1 instance")

        # Store old levels for renumeration
        old_levels = self._levels_per_sp_num.get(sp_num, []).copy()
        renumeration_table = {}

        # If there are old levels, attempt to match them to new levels
        if old_levels:
            for old_level in old_levels:
                if old_level == "1":  # Nucleus level
                    renumeration_table[old_level] = "1"  # Nucleus always maps to nucleus
                    continue
                energy = self._energy_table.get((sp_num, old_level), 0.0)
                stat_weight = self._stat_weight.get((sp_num, old_level), 0.0)
                config = self._configs.get((sp_num, old_level), [])
                # Find matching level in other_in1
                new_level = other_in1.find_level_by_energy_statweight_config(
                    sp_num, energy, stat_weight, config, percent_energy_match=0.05
                )
                renumeration_table[old_level] = new_level

        # Replace or add ionization potential
        self._ionization_potential[sp_num] = other_in1._ionization_potential[sp_num]

        # Clear existing data for this sp_num if it exists
        if sp_num in self._levels_per_sp_num:
            for level in self._levels_per_sp_num[sp_num]:
                key = (sp_num, level)
                if key in self._energy_table:
                    del self._energy_table[key]
                if key in self._configs:
                    del self._configs[key]
                if key in self._stat_weight:
                    del self._stat_weight[key]
            del self._levels_per_sp_num[sp_num]
            if sp_num in self._branching_ratio:
                del self._branching_ratio[sp_num]

        # Copy levels data
        if sp_num in other_in1._levels_per_sp_num:
            self._levels_per_sp_num[sp_num] = other_in1._levels_per_sp_num[sp_num].copy()
            for level in self._levels_per_sp_num[sp_num]:
                key = (sp_num, level)
                self._energy_table[key] = other_in1._energy_table[key]
                self._configs[key] = other_in1._configs[key].copy()
                self._stat_weight[key] = other_in1._stat_weight[key]

        # Copy branching ratio data
        if sp_num in other_in1._branching_ratio:
            self._branching_ratio[sp_num] = {}
            for config, levels in other_in1._branching_ratio[sp_num].items():
                self._branching_ratio[sp_num][config] = levels.copy()

        return renumeration_table

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

    def dump_to_string(self):
        """
        Dumps the IN1 class data to a string in the IN1.INP file format.
        """
        output_lines = []

        # Write stored header
        if self._header:
            output_lines.extend(self._header)

        for sp_num in sorted(self._ionization_potential.keys()):
            # Calculate regular and autoionization level counts
            levels = self._levels_per_sp_num.get(sp_num, [])
            regular_count = sum(1 for level in levels if level == "1" or (level.isdigit() and int(level) >= 0))
            autoion_count = sum(1 for level in levels if level.startswith('-') and level[1:].isdigit())

            # Ionization potential line
            ion_pot = self._ionization_potential[sp_num]
            some_number = self._header_fifth_column[sp_num]

            output_lines.append(
                f"{sp_num:<3} {regular_count:>4} {autoion_count:>4}   0  {ion_pot:>7.2f}    {some_number:>4}  {ion_pot:>7.2f}   0.0000   0.0000   0.000\n"
            )
        # Spectroscopic number sections
        for sp_num in sorted(self._ionization_potential.keys()):

            # Levels for this spectroscopic number
            if sp_num in self._levels_per_sp_num:
                output_lines.append(sp_num+"\n")
                for level in sorted(self._levels_per_sp_num[sp_num],
                                    key=lambda x: int(x) if x.isdigit() else float('inf')):
                    key = (sp_num, level)
                    config = self._configs.get(key, ["Unknown"])
                    stat_weight = self._stat_weight.get(key, 0.0)
                    energy = self._energy_table.get(key, 0.0)

                    # Format configuration (join with spaces, pad to fit)
                    config_str = " ".join(config).ljust(15)
                    if config == ["1s0"]:  # Nucleus case
                        config_str = "Nucleus".ljust(15)

                    if level =="-1":
                        output_lines.append("Autoionizating states\n")
                    # Level line format
                    output_lines.append(
                        f"{config_str:<15} {int(stat_weight):<10} {energy:>10.3f}     0.00e+00 0.00e+0      {level}\n"
                    )

        return "".join(output_lines)

    def dump_to_file(self, filename):
        """Write the IN1 data to a file using dump_to_string."""
        with open(filename, 'w') as outfile:
            outfile.write(self.dump_to_string())