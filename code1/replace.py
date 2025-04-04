import re
import sys

def process_value(match):
    num = int(match.group(0))
    new_num = -num + 300
    return " " +str(new_num)+" " 

def process_line(line):
    # Regular expression to match negative integers
    pattern = r' (-\d+) '
    processed_line = re.sub(pattern, process_value, line)
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
