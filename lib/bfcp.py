from lib.exceptions import GenericPlasmaException
from lib.utils import skip_n_lines


class BFCP:
    def __init__(self, bfcp_path=None):
        self._transitions = {}  # Key: (from_sp, from_level, to_sp, to_level), Value: dict with 'branching_ratio' or 'coefficients'
        self._spectroscopic_numbers = set()  # Set of all spectroscopic numbers
        if bfcp_path:
            self._load_data(bfcp_path)

    def _load_data(self, bfcp_path):
        """Load and parse the BFCP file."""
        with open(bfcp_path, 'r') as infile:
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

    @classmethod
    def create_empty(cls):
        """Create an empty BFCP instance."""
        return cls()

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
            raise ValueError(f"No branching ratio available for transition ({from_sp}, {from_level}, {to_sp}, {to_level})")
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