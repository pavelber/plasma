from lib.utils import skip_n_lines


class BFCP_From_databases:
    def __init__(self, bfcp_path):
        self._transitions = {}
        self._load_data(bfcp_path)

    def _load_data(self, bfcp_path):
        with open(bfcp_path, 'r') as f:
            skip_n_lines(f, 2)
            for line in f:
                if line.strip() and not line.startswith('-'):
                    parts = line.split()
                    if len(parts) >= 5:
                        z, lvl, z1, lvl1 = map(str, parts[:4])
                        coefficient = float(parts[4])
                        key = (z, lvl, z1, lvl1)
                        self._transitions[key] = coefficient

    def get_branching_coefficient(self, transition_key):
        return self._transitions[transition_key]

    def has_transition(self,transition_key):
        return transition_key in self._transitions