import os
import sys
import glob

def parse_qs_file(filepath):
    """
    Parses a QSs file to extract:
    - first_ss: First Spectroscopic Symbol in the table
    - h_like_ss: SS of the H-like ion (last entry in the table)
    - nucleus_count: The last number on the line containing 'nucl'
    - total_states: The last number on the last non-empty line of the file
    """
    first_ss = None
    h_like_ss = None
    nucleus_count = None
    total_states = None

    try:
        with open(filepath, 'r') as f:
            lines = [line.strip() for line in f if line.strip()]
    except Exception as e:
        print(f"Error reading {filepath}: {e}")
        return None

    if not lines:
        return None

    # Parse Table for first_ss and h_like_ss
    # Table usually starts after a separator line '----'
    # Look for header line starting with SpS
    table_start_index = -1
    for i, line in enumerate(lines):
        if line.startswith("SpS") and "QSs" in line:
            # Found header. The next line should be separator, then data.
            # However check if next line is separator
            if i + 1 < len(lines) and lines[i+1].startswith("-"):
               table_start_index = i + 2
            else:
               # Maybe no separator? Assume data starts next
               table_start_index = i + 1
            break
    
    if table_start_index != -1 and table_start_index < len(lines):
        # Read first row
        parts = lines[table_start_index].split()
        if parts:
            first_ss = parts[0]
        
        # Iterate to find end of table (separator or end of entries)
        # We assume table continues until a line starting with '-' or empty (already filtered)
        # However, looking at QSs.inp, there is a separator line '----------' after the table (line 11).
        # So we scan until we hit a separator line.
        current_idx = table_start_index
        while current_idx < len(lines):
            line = lines[current_idx]
            if line.startswith("-"):
                break
            # Update h_like_ss
            parts = line.split()
            if parts:
                h_like_ss = parts[0]
            current_idx += 1
    
    # Parse nucleus_count
    for line in lines:
        if "nucl" in line.lower():
            parts = line.split()
            if parts:
                nucleus_count = parts[-1]
                # Break? Usually only one nucleus line.
                break
    
    # Parse total_states (last token of last line)
    if lines:
        last_line = lines[-1]
        parts = last_line.split()
        if parts:
            total_states = parts[-1]

    return {
        'filename': os.path.basename(filepath),
        'first_ss': first_ss,
        'h_like_ss': h_like_ss,
        'nucleus_count': nucleus_count,
        'total_states': total_states
    }

def count_lines(filepath):
    try:
        with open(filepath, 'r') as f:
            return sum(1 for line in f)
    except Exception as e:
        print(f"Error reading {filepath}: {e}")
        return 0

def main():
    if len(sys.argv) < 2:
        print("Usage: python create_params0.py <directory>")
        sys.exit(1)

    directory = sys.argv[1]
    
    if not os.path.isdir(directory):
        print(f"Error: Directory '{directory}' does not exist.")
        sys.exit(1)

    # 1. Find and parse QSs*.inp files
    qs_pattern = os.path.join(directory, "QSs*.inp")
    qs_files = glob.glob(qs_pattern)
    
    parsed_data = []
    for qs_file in qs_files:
        # Ignore backups or other extensions if pattern matches vaguely (glob shouldn't if specific)
        if not qs_file.lower().endswith('.inp'):
            continue
            
        data = parse_qs_file(qs_file)
        if data:
            # Convert first_ss to int for sorting
            try:
                data['sort_key'] = int(data['first_ss'])
            except:
                data['sort_key'] = 0
            parsed_data.append(data)
    
    # Sort by first_ss descending
    parsed_data.sort(key=lambda x: x['sort_key'], reverse=True)
    
    if len(parsed_data) != 4:
        print(f"Warning: Expected 4 QSs line files, found {len(parsed_data)}.")

    # 2. Count lines in other files
    exc_lines = count_lines(os.path.join(directory, "Exc.inp"))
    inz_lines = count_lines(os.path.join(directory, "Inz.inp"))
    aiw_lines = count_lines(os.path.join(directory, "AIw.inp"))

    # 3. Construct Params0.inp content
    # Expected columns: sorted QSs files
    
    # helper to safely get values
    def get_row_values(key):
        return [str(d.get(key, '?')) for d in parsed_data]

    row1 = get_row_values('first_ss')
    row2 = get_row_values('h_like_ss')
    row3 = get_row_values('nucleus_count')
    row4 = get_row_values('total_states')

    # Formatting with fixed width for alignment like original
    def format_row(values, comment):
        # Use 4 chars width for the first value, and 3 for the rest
        if values:
            formatted_vals = f"{values[0]:>4}" + "".join(f"{val:>3}" for val in values[1:])
        else:
            formatted_vals = ""
        return f"{formatted_vals}  {comment}"

    lines_to_write = []
    lines_to_write.append(format_row(row1, "first spectroscopic symbol (SS) in each of 4 \"QSs.inp\"; here SS=25 for O-like Ge"))
    lines_to_write.append(format_row(row2, "SS of H-like ion in each of 4 \"QSs.inp\". NIF code doesn't accept SS[H] > 36"))
    lines_to_write.append(format_row(row3, "nucleus # in 4 \"QSs.inp\"."))
    lines_to_write.append(format_row(row4, "number of states in each of 4 \"QSs.inp\"; NIF code doesn't accept > 2300 states."))
    lines_to_write.append("----------------------")
    lines_to_write.append(f"{exc_lines:>6}   the number of strings in file \"Exc.inp\" (including the title)")
    lines_to_write.append(f"{inz_lines:>6}   the number of strings in file \"Inz.inp\" (including the title)")
    lines_to_write.append(f"{aiw_lines:>6}   the number of strings in file \"AIw.inp\" (including the title)")
    lines_to_write.append("") # Final newline

    content = "\n".join(lines_to_write)

    # 4. Handle Backup
    output_filename = "Params0.inp"
    output_path = os.path.join(directory, output_filename)
    backup_path = os.path.join(directory, output_filename + ".old")

    if os.path.exists(output_path):
        if not os.path.exists(backup_path):
            print(f"Renaming existing {output_filename} to .old")
            os.rename(output_path, backup_path)
        else:
            print(f"Backup {backup_path} exists. Overwriting current {output_filename} without new backup.")
    
    # Write new file
    with open(output_path, 'w') as f:
        f.write(content)
    
    print(f"Successfully created {output_path}")

if __name__ == "__main__":
    main()
