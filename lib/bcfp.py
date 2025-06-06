import os
from math import log
from os import path

from lib.create_cross_section_fits import parse_ionization_spectroscopic_files
from lib.cross_section import create_cross_section_function
from lib.fit_parameters import create_fits_from_range, create_fits
from lib.in1 import IN1
from lib.utils import skip_n_lines

reject_bad_fits = True

header = """   iSS   iQS   fSS   fQS           D              -A               B               C
-----------------------------------------------------------------------------------
"""

START_E = 1.0
END_E = 100.0
STEP_E = 1.0
METHOD = 'powell'
BAD_FIT_THRESHOLD = 1e-16


class BCFP:
    def __init__(self, bcfp_path=None):
        self._transitions = {}  # Key: (from_sp, from_level, to_sp, to_level), Value: dict with 'branching_ratio' or 'coefficients'
        self._spectroscopic_numbers = set()  # Set of all spectroscopic numbers
        if bcfp_path:
            self._load_data(bcfp_path)

    def _load_data(self, bcfp_path):
        """Load and parse the BCFP file."""
        with open(bcfp_path, 'r') as infile:
            header = infile.readline().strip()
            is_format_1 = "Coefficient" in header  # Format 1 has branching ratio + 3 coefficients
            skip_n_lines(infile, 1)  # Skip separator line

            for line in infile:
                parts = line.split()
                if len(parts) < 8:
                    continue  # Skip invalid lines

                from_sp = parts[0]
                from_level = parts[1]
                to_sp = parts[2]
                to_level = parts[3]

                self._spectroscopic_numbers.add(from_sp)
                self._spectroscopic_numbers.add(to_sp)

                transition_key = (from_sp, from_level, to_sp, to_level)
                if is_format_1:
                    # Format 1: branching ratio + 3 coefficients
                    branching_ratio = float(parts[4])
                    coefficients = [float(parts[i]) for i in range(5, 8)]  # A, B, C
                    self._transitions[transition_key] = {
                        'branching_ratio': branching_ratio,
                        'coefficients': coefficients
                    }
                else:
                    # Format 2: 4 coefficients (D, -A, B, C)
                    coefficients = [float(parts[i]) for i in range(4, 8)]  # D, -A, B, C
                    self._transitions[transition_key] = {
                        'coefficients': coefficients
                    }

    def get_transition_data(self, from_sp, from_level, to_sp, to_level):
        """Get data for a specific transition."""
        try:
            return self._transitions[(from_sp, from_level, to_sp, to_level)]
        except KeyError:
            raise ValueError(f"Transition ({from_sp}, {from_level}, {to_sp}, {to_level}) not found")

    def get_branching_ratio(self, from_sp, from_level, to_sp, to_level):
        """Get a branching ratio for a transition (only for Format 1 files)."""
        data = self.get_transition_data(from_sp, from_level, to_sp, to_level)
        if 'branching_ratio' not in data:
            raise ValueError(
                f"No branching ratio available for transition ({from_sp}, {from_level}, {to_sp}, {to_level})")
        return data['branching_ratio']

    def get_coefficients(self, from_sp, from_level, to_sp, to_level):
        """Get fitting coefficients for a transition."""
        return self.get_transition_data(from_sp, from_level, to_sp, to_level)['coefficients']

    def get_transitions_for_sp(self, sp_num):
        """Get all transitions involving a given spectroscopic number."""
        return [
            (from_sp, from_level, to_sp, to_level)
            for (from_sp, from_level, to_sp, to_level) in self._transitions
            if from_sp == sp_num or to_sp == sp_num
        ]

    def get_sp_numbers(self):
        """Get all spectroscopic numbers."""
        return sorted(self._spectroscopic_numbers)

    def contains_transition(self, from_sp, from_level, to_sp, to_level):
        """Check if a transition exists."""
        return (from_sp, from_level, to_sp, to_level) in self._transitions

    def dump_to_string(self):
        """
        Dumps the BCFP class data to a string in the BCFP file format.
        The output format (Format 1 or Format 2) is inferred from the stored data.
        """
        output_lines = []

        if not self._transitions:
            return header

        first_transition_data = next(iter(self._transitions.values()))
        is_output_format_1 = 'branching_ratio' in first_transition_data

        keys_prefix_format = "{:>5}{:>6}{:>6}{:>6}"

        if is_output_format_1:
            header_titles_line = header.replace("          D              -A               B               C",
                                                " Coefficient              A               B               C")
            output_lines.append(header_titles_line)

            sorted_items = sorted(self._transitions.items(),
                                  key=lambda item: BCFP._sort_key_for_transitions(item[0]))

            for key_tuple, data_dict in sorted_items:
                prefix_str = keys_prefix_format.format(key_tuple[0], key_tuple[1],
                                                       key_tuple[2], key_tuple[3])

                br = data_dict['branching_ratio']
                coeffs_A_B_C = data_dict['coefficients']
                data_values_str = (f"{br:16.4e} {coeffs_A_B_C[0]:16.4e} "
                                   f"{coeffs_A_B_C[1]:16.4e} {coeffs_A_B_C[2]:16.4e}")
                output_lines.append(f"{prefix_str}{data_values_str}\n")
        else:
            output_lines.append(header)
            sorted_items = sorted(self._transitions.items(),
                                  key=lambda item: BCFP._sort_key_for_transitions(item[0]))

            for key_tuple, data_dict in sorted_items:
                prefix_str = keys_prefix_format.format(key_tuple[0], key_tuple[1],
                                                       key_tuple[2], key_tuple[3])
                coeffs_D_negA_B_C = data_dict['coefficients']
                comment = data_dict.get('comment', '')
                data_values_str = (f"{coeffs_D_negA_B_C[0]:16.4e} {coeffs_D_negA_B_C[1]:16.4e} "
                                   f"{coeffs_D_negA_B_C[2]:16.4e} {coeffs_D_negA_B_C[3]:16.4e}")
                output_lines.append(f"{prefix_str}{data_values_str}{comment}\n")

        return "".join(output_lines)

    @staticmethod
    def _get_level_sort_tuple(level_str):
        """
        Creates a sort key component for a level string.
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
        from_sp_str, from_level_str, to_sp_str, to_level_str = transition_key_tuple
        key_from_sp = from_sp_str
        key_to_sp = to_sp_str
        key_from_level = BCFP._get_level_sort_tuple(from_level_str)
        key_to_level = BCFP._get_level_sort_tuple(to_level_str)
        return (key_from_sp, key_from_level, key_to_sp, key_to_level)

    @staticmethod
    def approximation_fun(params, E0, stat_weight, x):
        """
        Approximation function with 4 parameters.

        Args:
            params (list or array): Parameters [a, b, c, d].
            E0 (float): Ionization potential.
            stat_weight (float): Statistical weight.
            x (float): Energy ratio.

        Returns:
            float: Approximated value.
        """
        a, b, c, d = params
        y = 1 - 1 / x
        E = x * E0
        return 3.8101e-16 * (a * log(x) + b * y * y + c * y / x + d * y / (x * x)) * E / stat_weight

    def dump_to_file(self, filename):
        """Write the BCFP data to a file using dump_to_string."""
        with open(filename, 'w') as outfile:
            outfile.write(self.dump_to_string())

    @staticmethod
    def create_tabulation(energy_function, from_sp, from_level, to_sp, to_level, transition_energy, outdir):
        """
        Create a tabulation file for a given transition.
        """
        filename = path.join(outdir, f"{from_sp}_{from_level}_{to_sp}_{to_level}.txt")
        with open(filename, 'w') as f:
            f.write(f"# {from_sp} {from_level} {to_sp} {to_level} {transition_energy}\n")
            f.write(f"# e/I_zqq'        sigma\n")
            e = START_E
            while e <= END_E:
                f.write(f"{e:8.3f} {energy_function(e):10.3e}\n")
                e += STEP_E

    def create_fac_fitting_params(self, in1_data, cross_section_directory=None, create_tabulation_files=False, remove_bad_fits=True):
        """
        Convert a Format 1 BCFP (branching ratio + 3 coefficients) to Format 2 (4 coefficients).
        Updates the _transitions dictionary in memory.

        Args:
            in1_data (IN1): Instance of IN1
            cross_section_directory (str, optional): Directory containing cross-section files.
            create_tabulation_files (bool): If True, generate tabulation files.
            remove_bad_fits (bool): If True, remove transitions with failed fits; otherwise, keep with zeros and comment.
        """

        # Load cross-sections if provided
        cross_sections = {}
        if cross_section_directory and path.exists(cross_section_directory):
            cross_sections = parse_ionization_spectroscopic_files(cross_section_directory)

        # Create tabulation directory if needed
        outdir = None
        if create_tabulation_files:
            outdir = path.join(path.dirname(in1_path), 'tabulation')
            if not path.exists(outdir):
                os.mkdir(outdir)

        # Store transitions to remove if remove_bad_fits is True
        transitions_to_remove = []
        params = None  # Initial parameters for fitting

        for transition_key in self._transitions:
            from_sp, from_level, to_sp, to_level = transition_key
            data = self._transitions[transition_key]

            # Skip if already in Format 2
            if 'branching_ratio' not in data:
                continue

            # Get transition data
            coef = data['branching_ratio']
            transition_energy = in1_data.get_ionization_energy(from_sp, from_level, to_sp, to_level)
            from_config = in1_data.get_config(from_sp, from_level)
            to_config = in1_data.get_config(to_sp, to_level)
            from_stat_weight = in1_data.get_stat_weight(from_sp, from_level)

            result = None
            comment = ""
            # Try fitting from cross-section data if available
            if ((from_sp, from_level), (to_sp, to_level)) in cross_sections:
                print(f"Fitting from cross-section for {from_sp} {from_level} -> {to_sp} {to_level}")
                result, square_diff = create_fits(
                    table=cross_sections[((from_sp, from_level), (to_sp, to_level))],
                    approximation_fun=self.approximation_fun,
                    ionization_potential=in1_data.get_ionization_potential(from_sp),
                    stat_weight=from_stat_weight,
                    initial_params=params,
                    reject_bad_fits=reject_bad_fits,
                    bad_fit_threshold=BAD_FIT_THRESHOLD
                )
                if result is None:
                    print(f"Warning: Can't fit table {from_sp} {from_level} -> {to_sp} {to_level}")
                else:
                    comment = " #from db"

            # If no cross-section fit, try fitting from energy function
            if result is None:
                energy_function = create_cross_section_function(transition_energy, coef, from_config, to_config)
                if energy_function is None:
                    print(f"Warning: No energy function for {from_sp} {from_level} -> {to_sp} {to_level}")
                    if remove_bad_fits:
                        transitions_to_remove.append(transition_key)
                    else:
                        self._transitions[transition_key] = {
                            'coefficients': [0.0, 0.0, 0.0, 0.0],
                            'comment': "# Can't create fitting params"
                        }
                    continue

                if create_tabulation_files:
                    self.create_tabulation(energy_function, from_sp, from_level, to_sp, to_level, transition_energy, outdir)

                result, square_diff = create_fits_from_range(
                    cross_section_function=energy_function,
                    approximation_fun=self.approximation_fun,
                    ionization_potential=in1_data.get_ionization_potential(from_sp),
                    stat_weight=from_stat_weight,
                    initial_params=params,
                    start_e=START_E,
                    end_e=END_E,
                    step_e=STEP_E,
                    method=METHOD,
                    reject_bad_fits=reject_bad_fits,
                    bad_fit_threshold=BAD_FIT_THRESHOLD
                )

                if result is None:
                    print(f"Warning: Can't fit for {from_sp} {from_level} -> {to_sp} {to_level}")
                    if remove_bad_fits:
                        transitions_to_remove.append(transition_key)
                    else:
                        self._transitions[transition_key] = {
                            'coefficients': [0.0, 0.0, 0.0, 0.0],
                            'comment': "# Can't create fitting params"
                        }
                    continue

            # Update parameters for next iteration
            params = result

            # Update transition data with new coefficients
            self._transitions[transition_key] = {
                'coefficients': result,  # [D, -A, B, C]
                'comment': comment
            }

        # Remove transitions with failed fits if requested
        if remove_bad_fits:
            for transition_key in transitions_to_remove:
                del self._transitions[transition_key]
                # Update spectroscopic numbers
                from_sp, _, to_sp, _ = transition_key
                # Remove from_sp and to_sp from _spectroscopic_numbers if they no longer appear
                for sp in [from_sp, to_sp]:
                    if sp in self._spectroscopic_numbers:
                        if not any(t[0] == sp or t[2] == sp for t in self._transitions):
                            self._spectroscopic_numbers.remove(sp)

    def replace_transitions(self, start_sp_num, other, renumeration_table):
        """
        Replace transitions starting from a given spectroscopic number using another BCFP instance.
        For start_sp_num, apply level number translations to to_level in transitions from (sp_num < start_sp_num) to start_sp_num;
        for higher numbers, take all transitions from other.

        Args:
            start_sp_num (str): Spectroscopic number from which to start replacing transitions.
            other (BCFP): Another BCFP instance to take transitions from.
            renumeration_table (dict): Dictionary mapping old level numbers to new for start_sp_num, with non-None values.
        """
        # Create new transitions dictionary and spectroscopic numbers set
        new_transitions = {}
        new_spectroscopic_numbers = set()

        # Step 1: Copy transitions where both from_sp and to_sp are < start_sp_num
        for (from_sp, from_level, to_sp, to_level), data in self._transitions.items():
            if to_sp < start_sp_num:
                new_transitions[(from_sp, from_level, to_sp, to_level)] = data.copy()
                new_spectroscopic_numbers.add(from_sp)
                new_spectroscopic_numbers.add(to_sp)
            elif to_sp == start_sp_num:
                if to_level in renumeration_table and renumeration_table[to_level] is not None:
                    new_to_level = renumeration_table[to_level]
                    new_transitions[(from_sp, from_level, to_sp, new_to_level)] = data.copy()
                    new_spectroscopic_numbers.add(from_sp)
                    new_spectroscopic_numbers.add(to_sp)


        # Step 3: Copy transitions from other where from_sp or to_sp > start_sp_num
        for (from_sp, from_level, to_sp, to_level), data in other._transitions.items():
            if to_sp > start_sp_num:
                new_transitions[(from_sp, from_level, to_sp, to_level)] = data.copy()
                new_spectroscopic_numbers.add(from_sp)
                new_spectroscopic_numbers.add(to_sp)

        # Update instance data
        self._transitions = new_transitions
        self._spectroscopic_numbers = new_spectroscopic_numbers
# Example usage
if __name__ == "__main__":
    import sys

    if len(sys.argv) != 4:
        print("Usage: python script.py <bcfp_input_file> <in1_file> <bcfp_output_file>")
        sys.exit(1)

    bcfp_input_path = sys.argv[1]
    in1_path = sys.argv[2]
    bcfp_output_path = sys.argv[3]
    cross_section_directory = "C:\\work2\\plasma\\db\\O\\ionization-crosssection\\"

    # Load BCFP
    bcfp = BCFP(bcfp_input_path)

    # Convert to Format 2
    bcfp.create_fac_fitting_params(
        in1_data=IN1(in1_path),
        cross_section_directory=cross_section_directory,
        create_tabulation_files=False,
        remove_bad_fits=False
    )

    # Dump to output file
    with open(bcfp_output_path, 'w') as outfile:
        outfile.write(bcfp.dump_to_string())