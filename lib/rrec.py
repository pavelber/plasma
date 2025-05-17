import os

class RREC:
    def __init__(self, rrec_path=None):
        self._transitions = {}  # Key: (sp_num, from_level, to_level), Value: dict with 'method_id', 'coefficients'
        self._spectroscopic_numbers = set()  # Set of all spectroscopic numbers
        if rrec_path:
            self._load_data(rrec_path)

    def _load_data(self, rrec_path):
        """Load and parse the RREC.INP file."""
        with open(rrec_path, 'r') as infile:
            for line in infile:
                parts = line.strip().split()
                if len(parts) < 14:
                    continue  # Skip invalid lines

                sp_num = parts[0]
                from_level = parts[1]
                to_level = parts[2]
                method_id = parts[3]
                coefficients = [float(parts[i]) for i in range(4, 14)]  # A, B, C, D, E, F, G, H, I, J

                self._spectroscopic_numbers.add(sp_num)
                transition_key = (sp_num, from_level, to_level)
                self._transitions[transition_key] = {
                    'method_id': method_id,
                    'coefficients': coefficients
                }

    def get_transition_data(self, sp_num, from_level, to_level):
        """Get data for a specific transition."""
        try:
            return self._transitions[(sp_num, from_level, to_level)]
        except KeyError:
            raise ValueError(f"Transition ({sp_num}, {from_level}, {to_level}) not found")

    def get_method_id(self, sp_num, from_level, to_level):
        """Get the fitting method ID for a transition."""
        return self.get_transition_data(sp_num, from_level, to_level)['method_id']

    def get_coefficients(self, sp_num, from_level, to_level):
        """Get fitting coefficients for a transition."""
        return self.get_transition_data(sp_num, from_level, to_level)['coefficients']

    def get_transitions_for_sp(self, sp_num):
        """Get all transitions for a given spectroscopic number."""
        return [
            (s, f, t)
            for (s, f, t) in self._transitions
            if s == sp_num
        ]

    def get_sp_numbers(self):
        """Get all spectroscopic numbers."""
        return sorted(self._spectroscopic_numbers)

    def contains_transition(self, sp_num, from_level, to_level):
        """Check if a transition exists."""
        return (sp_num, from_level, to_level) in self._transitions

    def dump_to_string(self):
        """
        Dumps the RREC class data to a string in the RREC.INP file format.
        """
        if not self._transitions:
            return ""

        output_lines = []
        format_str = "%3s %6s %6s %4s %12.3E %12.3E %12.3E %12.3E %12.3E %12.3E %12.3E %12.3E %12.3E %12.3E"

        sorted_items = sorted(self._transitions.items(),
                              key=lambda item: self._sort_key_for_transitions(item[0]))

        for (sp_num, from_level, to_level), data in sorted_items:
            coeffs = data['coefficients']
            line = format_str % (
                sp_num,
                from_level,
                to_level,
                int(data['method_id']),  # Assuming method_id is integer
                coeffs[0], coeffs[1], coeffs[2], coeffs[3], coeffs[4],
                coeffs[5], coeffs[6], coeffs[7], coeffs[8], coeffs[9]
            )
            output_lines.append(line + "\n")

        return "".join(output_lines)

    def dump_to_file(self, filename):
        """Write the RREC data to a file using dump_to_string."""
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
        sp_num, from_level_str, to_level_str = transition_key_tuple
        key_from_level = RREC._get_level_sort_tuple(from_level_str)
        key_to_level = RREC._get_level_sort_tuple(to_level_str)
        return (sp_num, key_from_level, key_to_level)

    def replace_transitions(self, start_sp_num, other, renumeration_table):
        """
        Replace transitions starting from a given spectroscopic number using another RREC instance.
        For start_sp_num, apply level number translations; for higher numbers, take all transitions from other.

        Args:
            start_sp_num (str): Spectroscopic number from which to start replacing transitions.
            other (RREC): Another RREC instance to take transitions from.
            renumeration_table (dict): Dictionary mapping old level numbers to new for start_sp_num, with non-None values.
        """
        # Create a new transitions dictionary
        new_transitions = {}
        new_spectroscopic_numbers = set()

        # Step 1: Copy transitions for spectroscopic numbers < start_sp_num
        for (sp_num, from_level, to_level), data in self._transitions.items():
            if sp_num < start_sp_num:
                new_transitions[(sp_num, from_level, to_level)] = data.copy()
                new_spectroscopic_numbers.add(sp_num)

        # Step 2: Handle transitions for start_sp_num with renumeration_table
        for (sp_num, from_level, to_level), data in self._transitions.items():
            if sp_num == start_sp_num:
                # Check if both levels are in renumeration_table and values are not None
                if (from_level in renumeration_table and renumeration_table[from_level] is not None and
                    to_level in renumeration_table and renumeration_table[to_level] is not None):
                    # Translate level numbers
                    new_from_level = renumeration_table[from_level]
                    new_to_level = renumeration_table[to_level]
                    # Add translated transition
                    new_transitions[(sp_num, new_from_level, new_to_level)] = data.copy()
                    new_spectroscopic_numbers.add(sp_num)

        # Step 3: Copy transitions for spectroscopic numbers > start_sp_num from other
        for (sp_num, from_level, to_level), data in other._transitions.items():
            if sp_num > start_sp_num:
                new_transitions[(sp_num, from_level, to_level)] = data.copy()
                new_spectroscopic_numbers.add(sp_num)

        # Update instance data
        self._transitions = new_transitions
        self._spectroscopic_numbers = new_spectroscopic_numbers