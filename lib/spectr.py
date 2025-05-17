import os

header_template = "%f %f %f %d\n"

class SPECTR:
    def __init__(self, spectr_path=None):
        self._header = None  # Tuple: (atomic_mass, param1, param2, count)
        self._transitions = {}  # Key: (sp_num, up_level, low_level), Value: dict with 'weight', 'wavelength', 'eins_coeff', 'osc_strength'
        self._spectroscopic_numbers = set()  # Set of all spectroscopic numbers
        if spectr_path:
            self._load_data(spectr_path)

    def _load_data(self, spectr_path):
        """Load and parse the SPECTR.INP file."""
        with open(spectr_path, 'r') as infile:
            # Read header line
            header_line = infile.readline().strip()
            header_parts = header_line.split()
            if len(header_parts) >= 4:
                self._header = (
                    float(header_parts[0]),  # atomic_mass
                    float(header_parts[1]),  # param1
                    float(header_parts[2]),  # param2
                    int(header_parts[3])     # count
                )
            else:
                raise ValueError("Invalid SPECTR.INP header format")

            # Read transition lines
            for line in infile:
                parts = line.strip().split()
                if len(parts) < 6:
                    continue  # Skip invalid lines

                sp_num = parts[0]
                up_level = parts[1]
                low_level = parts[2]
                weight = float(parts[3])
                wavelength = float(parts[4])
                eins_coeff = float(parts[5])
                osc_strength = float(parts[6]) if len(parts) > 6 else None  # Optional

                self._spectroscopic_numbers.add(sp_num)
                transition_key = (sp_num, up_level, low_level)
                self._transitions[transition_key] = {
                    'weight': weight,
                    'wavelength': wavelength,
                    'eins_coeff': eins_coeff,
                    'osc_strength': osc_strength
                }

    def get_transition_data(self, sp_num, up_level, low_level):
        """Get data for a specific transition."""
        try:
            return self._transitions[(sp_num, up_level, low_level)]
        except KeyError:
            raise ValueError(f"Transition ({sp_num}, {up_level}, {low_level}) not found")

    def get_wavelength(self, sp_num, up_level, low_level):
        """Get wavelength for a transition."""
        return self.get_transition_data(sp_num, up_level, low_level)['wavelength']

    def get_eins_coeff(self, sp_num, up_level, low_level):
        """Get Einstein coefficient for a transition."""
        return self.get_transition_data(sp_num, up_level, low_level)['eins_coeff']

    def get_osc_strength(self, sp_num, up_level, low_level):
        """Get oscillator strength for a transition, if available."""
        return self.get_transition_data(sp_num, up_level, low_level)['osc_strength']

    def get_transitions_for_sp(self, sp_num):
        """Get all transitions for a given spectroscopic number."""
        return [
            (s, u, l)
            for (s, u, l) in self._transitions
            if s == sp_num
        ]

    def get_sp_numbers(self):
        """Get all spectroscopic numbers."""
        return sorted(self._spectroscopic_numbers)

    def contains_transition(self, sp_num, up_level, low_level):
        """Check if a transition exists."""
        return (sp_num, up_level, low_level) in self._transitions

    def dump_to_string(self):
        """
        Dumps the SPECTR class data to a string in the SPECTR.INP file format.
        """
        if not self._header and not self._transitions:
            return ""

        output_lines = []
        # Write header
        if self._header:
            atomic_mass, param1, param2, count = self._header
            output_lines.append(header_template % (atomic_mass, param1, param2, count))

        # Write transitions
        format_str = "%3s %3s %3s 1 %8.3f %8.3e"  # Base format without osc_strength
        sorted_items = sorted(self._transitions.items(),
                              key=lambda item: self._sort_key_for_transitions(item[0]))

        for (sp_num, up_level, low_level), data in sorted_items:
            line = format_str % (
                sp_num,
                up_level,
                low_level,
                data['wavelength'],
                data['eins_coeff']
            )
            if data['osc_strength'] is not None:
                line += f" %8.3e" % data['osc_strength']
            output_lines.append(line + "\n")

        return "".join(output_lines)

    def dump_to_file(self, filename):
        """Write the SPECTR data to a file using dump_to_string."""
        with open(filename, 'w') as outfile:
            outfile.write(self.dump_to_string())

    @staticmethod
    def _get_level_sort_tuple(level_str):
        """
        Creates a sort key component for a level string.
        Sort Order: Positive integers, zero, negative integers, non-integer strings.
        """
        try:
            val = int(level_str)
            if val > 0:
                return (0, val)
            elif val == 0:
                return (1, 0)
            else:
                return (2, -val)
        except ValueError:
            return (3, level_str)

    @staticmethod
    def _sort_key_for_transitions(transition_key_tuple):
        """
        Helper function to create a sort key for transition tuples.
        """
        sp_num, up_level_str, low_level_str = transition_key_tuple
        key_up_level = SPECTR._get_level_sort_tuple(up_level_str)
        key_low_level = SPECTR._get_level_sort_tuple(low_level_str)
        return (sp_num, key_up_level, key_low_level)

    def replace_transitions(self, start_sp_num, other, renumeration_table):
        """
        Replace transitions starting from a given spectroscopic number using another SPECTR instance.
        For start_sp_num, apply level number translations; for higher numbers, take all transitions from other.

        Args:
            start_sp_num (str): Spectroscopic number from which to start replacing transitions.
            other (SPECTR): Another SPECTR instance to take transitions from.
            renumeration_table (dict): Dictionary mapping old level numbers to new for start_sp_num, with non-None values.
        """
        # Create a new transitions dictionary
        new_transitions = {}
        new_spectroscopic_numbers = set()

        # Step 1: Copy transitions for spectroscopic numbers < start_sp_num
        for (sp_num, up_level, low_level), data in self._transitions.items():
            if sp_num < start_sp_num:
                new_transitions[(sp_num, up_level, low_level)] = data.copy()
                new_spectroscopic_numbers.add(sp_num)

        # Step 2: Handle transitions for start_sp_num with renumeration_table
        for (sp_num, up_level, low_level), data in self._transitions.items():
            if sp_num == start_sp_num:
                # Check if both levels are in renumeration_table and values are not None
                if (up_level in renumeration_table and renumeration_table[up_level] is not None and
                    low_level in renumeration_table and renumeration_table[low_level] is not None):
                    # Translate level numbers
                    new_up_level = renumeration_table[up_level]
                    new_low_level = renumeration_table[low_level]
                    # Add translated transition
                    new_transitions[(sp_num, new_up_level, new_low_level)] = data.copy()
                    new_spectroscopic_numbers.add(sp_num)

        # Step 3: Copy transitions for spectroscopic numbers > start_sp_num from other
        for (sp_num, up_level, low_level), data in other._transitions.items():
            if sp_num > start_sp_num:
                new_transitions[(sp_num, up_level, low_level)] = data.copy()
                new_spectroscopic_numbers.add(sp_num)

        # Update instance data
        self._transitions = new_transitions
        self._spectroscopic_numbers = new_spectroscopic_numbers
        # Preserve header from self (assume it remains unchanged)
        self._header = self._header or other._header  # Use other's header if self has none