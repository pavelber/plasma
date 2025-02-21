import sys

from util import process_line, ConversionType


def process_file(input_file, conversion_type):
    with open(input_file, 'r') as infile:
        lines = infile.readlines()

    processed_lines = [process_line(line, 6, 10, conversion_type) for line in lines]
    return ''.join(processed_lines)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python script.py input_filename [conversion_type] ")
        sys.exit(1)

    input_filename = sys.argv[1]
    if len(sys.argv) > 2:
        conversion_type = ConversionType[sys.argv[2]]
    else:
        conversion_type = ConversionType.NEGATIVE_TO_200
    processed_data = process_file(input_filename, conversion_type)
    print(processed_data)
