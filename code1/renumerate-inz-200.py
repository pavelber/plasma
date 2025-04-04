import sys


def process_line(line, start, end):
    try:
        # Extract the number from positions 38-41
        num_str = line[start:end]
        num = int(num_str)

        # Check if the number is in the specified range
        if 200 <= num <= 299:
            num += 100

        # Replace the original number with the modified one
        modified_line = line[:start] + str(num).rjust(4) + line[end:]
        return modified_line
    except ValueError:
        # If conversion to integer fails, return the original line
        return line


def process_file(input_file):
    with open(input_file, 'r') as infile:
        lines = infile.readlines()

    processed_lines = [process_line(process_line(line, 7, 11), 17, 21) for line in lines]
    return ''.join(processed_lines)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python script.py input_filename")
        sys.exit(1)

    input_filename = sys.argv[1]
    processed_data = process_file(input_filename)
    print(processed_data)
