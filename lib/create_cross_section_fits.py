import os
import re
from enum import Enum

from lib.cross_section import nomad_5, nomad_11, nomad_16
from lib.roman import roman_to_int
from lib.utils import read_table


class TransitionType(Enum):
    EXCITATION = "excitation"
    IONIZATION = "ionization"


fitting_functions = {
    TransitionType.EXCITATION: [
        {'id': '5', 'function': nomad_5, 'num_params': 5},
        {'id': '11', 'function': nomad_11, 'num_params': 5},
        {'id': '16', 'function': nomad_16, 'num_params': 6,
         'initial_params': [1.0, 1.0, 1.0, 1.0, 1.0, 2.0],
         'bounds': [(0, None), (0, None), (0, None), (0, None), (1e-6, None), (1.01, None)]},
    ],
    TransitionType.IONIZATION: [
        {'id': '5', 'function': nomad_5, 'num_params': 5},
        {'id': '11', 'function': nomad_11, 'num_params': 5},
        {'id': '16', 'function': nomad_16, 'num_params': 6,
         'initial_params': [1.0, 1.0, 1.0, 1.0, 1.0, 2.0],
         'bounds': [(0, None), (0, None), (0, None), (0, None), (1e-6, None), (1.01, None)]},
    ]
}


def parse_excitation_spectroscopic_files(directory):
    """
    Reads file names in the given directory, parses spectroscopic transition file names,
    and extracts element, source, and target spectroscopic numbers and levels.

    Args:
        directory (str): Path to the directory containing spectroscopic files.

    Returns:
        list: List of dictionaries containing parsed data for each valid file.
    """
    # Get valid elements from read_table()[0] keys
    valid_elements = set(read_table()[0].keys())  # Convert dict_keys to set for efficient lookup

    # Initialize result list
    parsed_data = []

    # Regular expression to match file names like OI-OII_27-12.any_extension
    pattern = re.compile(
        r'^([A-Z][a-z]?) (I|II|III|IV|V|VI|VII|VIII|IX|X) (\d+)-(\d+)\.\w+$')
    result = {}
    # Iterate through files in the directory
    for filename in os.listdir(directory):
        match = pattern.match(filename)
        if match:
            # Extract components
            source_element, sp_num_roman, source_level, target_level = match.groups()

            # Debug: Log parsed components
            #  print(f"Parsing {filename}:")
            #  print(f"  Source Element: {source_element}, Source Roman: {source_roman}")
            #  print(f"  Target Element: {target_element}, Target Roman: {target_roman}")
            #  print(f"  Source Level: {source_level}, Target Level: {target_level}")

            # Validate elements
            if source_element not in valid_elements:
                print(f"  Skipping {filename}: Invalid element(s) ({source_element})")
                continue

            # Convert Roman numerals to integers
            spec_num = str(roman_to_int(sp_num_roman))

            # Convert levels to integers
            source_level = source_level
            target_level = target_level
            result[(spec_num, source_level, target_level)] = (
                read_cross_section_dat(directory + filename))
        else:
            print(f"  Skipping {filename}: Does not match expected pattern")

    return result


def parse_ionization_spectroscopic_files(directory):
    """
    Reads file names in the given directory, parses spectroscopic transition file names,
    and extracts element, source, and target spectroscopic numbers and levels.

    Args:
        directory (str): Path to the directory containing spectroscopic files.

    Returns:
        list: List of dictionaries containing parsed data for each valid file.
    """
    # Get valid elements from read_table()[0] keys
    valid_elements = set(read_table()[0].keys())  # Convert dict_keys to set for efficient lookup

    # Initialize result list
    parsed_data = []

    # Regular expression to match file names like OI-OII_27-12.any_extension
    pattern = re.compile(
        r'^([A-Z][a-z]?)(I|II|III|IV|V|VI|VII|VIII|IX|X)-([A-Z][a-z]?)(I|II|III|IV|V|VI|VII|VIII|IX|X)_(\d+)-(\d+)\.\w+$')
    result = {}
    # Iterate through files in the directory
    for filename in os.listdir(directory):
        match = pattern.match(filename)
        if match:
            # Extract components
            source_element, source_roman, target_element, target_roman, source_level, target_level = match.groups()

            # Debug: Log parsed components
            #  print(f"Parsing {filename}:")
            #  print(f"  Source Element: {source_element}, Source Roman: {source_roman}")
            #  print(f"  Target Element: {target_element}, Target Roman: {target_roman}")
            #  print(f"  Source Level: {source_level}, Target Level: {target_level}")

            # Validate elements
            if source_element not in valid_elements or target_element not in valid_elements:
                print(f"  Skipping {filename}: Invalid element(s) ({source_element}, {target_element})")
                continue

            # Additional check: Ensure target_element doesn't contain Roman numerals
            if any(roman in target_element for roman in ['I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII', 'IX', 'X']):
                print(f"  Skipping {filename}: Target element {target_element} contains Roman numerals")
                continue

            # Convert Roman numerals to integers
            source_spec_num = str(roman_to_int(source_roman))
            target_spec_num = str(roman_to_int(target_roman))

            # Convert levels to integers
            source_level = source_level
            target_level = target_level
            result[((source_spec_num, source_level), (target_spec_num, target_level))] = (
                read_cross_section_dat(directory + filename))
        else:
            print(f"  Skipping {filename}: Does not match expected pattern")

    return result


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
        :param transition_type:
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

        return result

    except FileNotFoundError:
        raise FileNotFoundError(f"File not found: {filepath}")
    except Exception as e:
        raise ValueError(f"Error parsing file {filepath}: {str(e)}")


if __name__ == "__main__":
    print(parse_ionization_spectroscopic_files("c:\\work4\\db\\o-dat\\"))
