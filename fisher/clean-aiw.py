import sys

def process_file(filename):
    try:
        with open(filename, 'r') as file:
            for line in file:
                fields = line.split()  # Assuming fields are separated by whitespace
                if len(fields) >= 4:
                    try:
                        fourth_field = int(fields[3])
                        if fourth_field <= 300:
                            print(line.strip())  # Print the line without trailing newline
                    except ValueError:
                        # Ignore lines where the fourth field is not a valid integer
                        print(line.strip())
                else:
                    # Ignore lines with fewer than 4 fields
                    print(line.strip())
    except FileNotFoundError:
        print(f"File '{filename}' not found.")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python script.py <filename>")
    else:
        input_filename = sys.argv[1]
        process_file(input_filename)
