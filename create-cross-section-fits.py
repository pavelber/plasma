from lib.cross_section import nomad_5, nomad_11, nomad_16
from lib.fit_parameters import select_best_fit


def read_cross_section_dat(filepath: str) -> list[tuple[float, float]]:
    """
    Parse a .dat file containing ionization cross-section data and return a list of (x, y) tuples.

    Args:
        filepath (str): Path to the .dat file.

    Returns:
        list[tuple[float, float]]: List of (Eel, ionization_cross_section) pairs.

    Raises:
        FileNotFoundError: If the file does not exist.
        ValueError: If the file format is invalid or data cannot be parsed.
    """
    result = []

    try:
        with open(filepath, 'r') as file:
            # Skip header lines (first two lines)
            next(file)  # Skip "Eel Ionization cross section"
            next(file)  # Skip "eV cm+(2)"

            # Process data lines
            for line in file:
                # Split line into columns, handling whitespace
                columns = line.strip().split()

                # Ensure exactly two columns
                if len(columns) != 2:
                    raise ValueError(f"Invalid line format: {line.strip()}")

                # Parse Eel and cross section
                try:
                    eel = float(columns[0])
                    cross_section = float(columns[1])
                    result.append((eel, cross_section))
                except ValueError as e:
                    raise ValueError(f"Failed to parse line: {line.strip()} - {str(e)}")

        print(select_best_fit(
            table=result,
            function_objects=[
                {'id': 'nomad_5', 'function': nomad_5, 'num_params': 5},
                {'id': 'nomad_11', 'function': nomad_11, 'num_params': 5},
                {'id': 'nomad_16', 'function': nomad_16, 'num_params': 6,
                 'initial_params': [1.0, 1.0, 1.0, 1.0, 1.0, 2.0],
                 'bounds': [(0, None), (0, None), (0, None), (0, None), (1e-6, None), (1.01, None)]},
            ],
            ionization_potential=1.0,
            stat_weight=1.0,
            method='powell',
            reject_bad_fits=True,
            bad_fit_threshold=1e-16,
        ))

    except FileNotFoundError:
        raise FileNotFoundError(f"File not found: {filepath}")
    except Exception as e:
        raise ValueError(f"Error parsing file {filepath}: {str(e)}")


if __name__ == "__main__":
    read_cross_section_dat("c:\\work4\\db\\o-dat\\OI-OII_6-11.dat")
