import os
from os import path

from lib.create_cross_section_fits import parse_excitation_spectroscopic_files, TransitionType, fitting_functions
from lib.cross_section import create_cross_section_function
from lib.in1 import IN1
from lib.utils import skip_n_lines
from lib.fit_parameters import select_best_fit

reject_bad_fits = True
BAD_FIT_THRESHOLD = 1e-16

header = """  SS   #1   #2   Mthd        A          B            C            D            E            F          Osc.Strngth
------------------------------------------------------------------------------------------------------------------
"""

class EXCIT:
    def __init__(self, excit_path=None):
        self._transitions = {}  # Key: (sp_num, from_level, to_level), Value: dict with 'method_id', 'coefficients', 'osc_strength'
        self._spectroscopic_numbers = set()  # Set of all spectroscopic numbers
        if excit_path:
            self._load_data(excit_path)

    def _load_data(self, excit_path):
        """Load and parse the EXCIT file."""
        with open(excit_path, 'r') as infile:
            skip_n_lines(infile, 2)  # Skip header and separator lines
            for line in infile:
                # Parse fixed-width line
                parts = [
                    line[0:4].strip(),   # SS
                    line[4:9].strip(),   # #1 (from_level)
                    line[9:14].strip(),  # #2 (to_level)
                    line[14:19].strip(), # Mthd
                    line[19:33].strip(), # A
                    line[33:46].strip(), # B
                    line[46:59].strip(), # C
                    line[59:72].strip(), # D
                    line[72:85].strip(), # E
                    line[85:98].strip(), # F
                    line[98:].strip()    # Osc.Strngth
                ]
                if len(parts) < 11 or not parts[0]:
                    continue  # Skip invalid lines

                sp_num = parts[0]
                from_level = parts[1]
                to_level = parts[2]
                method_id = parts[3]
                coefficients = [float(parts[i]) for i in range(4, 10)]  # A, B, C, D, E, F
                osc_strength = float(parts[10])

                self._spectroscopic_numbers.add(sp_num)
                transition_key = (sp_num, from_level, to_level)
                self._transitions[transition_key] = {
                    'method_id': method_id,
                    'coefficients': coefficients,
                    'osc_strength': osc_strength
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

    def get_osc_strength(self, sp_num, from_level, to_level):
        """Get oscillator strength for a transition."""
        return self.get_transition_data(sp_num, from_level, to_level)['osc_strength']

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
        Dumps the EXCIT class data to a string in the EXCIT file format.
        """
        if not self._transitions:
            return header

        output_lines = [header]
        widths = [4, 5, 5, 5, 14, 13, 13, 13, 13, 13, 16]  # Fixed column widths
        format_str = "{:>4}{:>5}{:>5}{:>5}{:>14.3E}{:>13.3E}{:>13.3E}{:>13.3E}{:>13.3E}{:>13.3E}{:>16.3E}"

        sorted_items = sorted(self._transitions.items(),
                              key=lambda item: self._sort_key_for_transitions(item[0]))

        for (sp_num, from_level, to_level), data in sorted_items:
            coeffs = data['coefficients']
            line = format_str.format(
                sp_num,
                from_level,
                to_level,
                int(data['method_id']),  # Assuming method_id is integer
                coeffs[0], coeffs[1], coeffs[2], coeffs[3], coeffs[4], coeffs[5],
                data['osc_strength']
            )
            output_lines.append(line + "\n")

        return "".join(output_lines)

    def dump_to_file(self, filename):
        """Write the EXCIT data to a file using dump_to_string."""
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
        key_from_level = EXCIT._get_level_sort_tuple(from_level_str)
        key_to_level = EXCIT._get_level_sort_tuple(to_level_str)
        return (sp_num, key_from_level, key_to_level)

    @staticmethod
    def _modify_line(line, best_id, best_params):
        """
        Modifies a fixed-width formatted line based on new values.

        Args:
            line (str): The original line string.
            best_id (str or int): The new value for the method column.
            best_params (list): A list of new values for coefficient columns (up to 6).

        Returns:
            str: The modified line string.
        """
        widths = [4, 5, 5, 5, 14, 13, 13, 13, 13, 13, 16]
        parsed_parts = []
        current_pos = 0
        for width in widths:
            parsed_parts.append(line[current_pos:current_pos + width])
            current_pos += width

        new_parts = list(parsed_parts)
        new_parts[3] = str(best_id).rjust(widths[3])

        for i in range(6):
            col_index = 4 + i
            if i < len(best_params):
                try:
                    value = float(best_params[i])
                    formatted_value = "{:> {}.3E}".format(value, widths[col_index] - 1)
                    if len(formatted_value) > widths[col_index]:
                        formatted_value = "{:> {}.2E}".format(value, widths[col_index] - 1)
                    if len(formatted_value) > widths[col_index]:
                        formatted_value = str(best_params[i])[:widths[col_index]].rjust(widths[col_index])
                except ValueError:
                    formatted_value = str(best_params[i]).rjust(widths[col_index])
            else:
                formatted_value = "{:> {}.3E}".format(0.0, widths[col_index] - 1)
                if len(formatted_value) > widths[col_index]:
                    formatted_value = "0.0".rjust(widths[col_index])
            new_parts[col_index] = formatted_value.rjust(widths[col_index])

        return "".join(new_parts)

    def replace_fits_from_cross_sections(self, in1_path, cross_section_directory):
        """
        Replace fitting parameters for transitions with available cross-section data.

        Args:
            in1_path (str): Path to the IN1 file.
            cross_section_directory (str): Directory containing cross-section files.
        """
        in1_data = IN1(in1_path)
        cross_sections = parse_excitation_spectroscopic_files(cross_section_directory)

        for transition_key in list(self._transitions.keys()):
            sp_num, from_level, to_level = transition_key
            if (sp_num, from_level, to_level) in cross_sections:
                best_id, best_params = select_best_fit(
                    table=cross_sections[(sp_num, from_level, to_level)],
                    function_objects=fitting_functions[TransitionType.EXCITATION],
                    ionization_potential=in1_data.get_ionization_potential(sp_num),
                    stat_weight=0,  # Not used
                    reject_bad_fits=reject_bad_fits,
                    bad_fit_threshold=BAD_FIT_THRESHOLD
                )
                # Update transition data
                self._transitions[transition_key]['method_id'] = str(best_id)
                # Ensure coefficients list is 6 elements, padding with 0.0 if needed
                new_coeffs = [float(p) if i < len(best_params) else 0.0 for i, p in enumerate(best_params + (0.0,) * 6)][:6]
                self._transitions[transition_key]['coefficients'] = new_coeffs


# Example usage
if __name__ == "__main__":
    import sys

    if len(sys.argv) != 4:
        print("Usage: python script.py <excit_input_file> <in1_file> <excit_output_file>")
        sys.exit(1)

    excit_input_path = sys.argv[1]
    in1_path = sys.argv[2]
    excit_output_path = sys.argv[3]
    cross_section_directory = "C:\\work2\\plasma\\db\\O\\excitation-crosssection\\"

    # Load EXCIT
    excit = EXCIT(excit_input_path)

    # Replace fits
    excit.replace_fits_from_cross_sections(
        in1_path=in1_path,
        cross_section_directory=cross_section_directory
    )

    # Dump to output file
    excit.dump_to_file(excit_output_path)