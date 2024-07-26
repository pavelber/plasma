import sys


def process_line(line):
    if line[0:3] == "   ":
        states = line[0:26].split()
        if len(states) > 1:
            st1 = states[-2]
            st2 = states[-1]
        else:
            st1 = ""
            st2 = states[0]

        if st1.endswith("1"):
            st1 = st1[:-1]
        if st2.endswith("1"):
            st2 = st2[:-1]
        processed_line = " " + st1.ljust(4) + " " + st2.ljust(4) + line[26:]
    else:
        processed_line = line

    return processed_line


def process_file(input_file):
    with open(input_file, 'r') as infile:
        lines = infile.readlines()
    processed_lines = [process_line(line) for line in lines]
    return ''.join(processed_lines)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python script.py input_filename")
        sys.exit(1)

    input_filename = sys.argv[1]
    processed_data = process_file(input_filename)
    print(processed_data)
