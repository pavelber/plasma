import os
from collections import defaultdict


def process_data_file():
    """
    Reads data from a specified input file, filters lines, calculates a new
    distributed 5th column value (V_prime_5) to avoid repetitions in Col 4,
    and then calculates the final 6th column (V_double_prime_6) based on V_prime_5.

    The distribution spreads identical Column 4 values between V_4 and V_4 + 0.001.

    Output format (7 columns, CSV):
    SpN, high, low, wavelength, wavelength-distributed, energy, tau
    """
    # --- Hardcoded File Paths ---
    INPUT_FILE_PATH = "..\\inform3.dat"
    OUTPUT_FILE_PATH = "..\\output_results3.csv"
    # ---------------------------

    CONSTANT_DIVIDER = 13.29842
    DELTA = 0.001  # The range over which to distribute repeated values

    # Stores valid lines: [[col1, col2, col3, col4_val, col7, line_num], ...]
    valid_records = []
    # Stores the count of repetitions for each unique Col 4 value
    col4_counts = defaultdict(int)

    line_number = 0

    print(f"Starting PASS 1: Reading and counting repetitions in file: {INPUT_FILE_PATH}")

    try:
        # --- PASS 1: Filter and Aggregate ---
        with open(INPUT_FILE_PATH, 'r') as infile:
            for line in infile:
                line_number += 1
                # Splits by any whitespace (space, tab)
                columns = line.strip().split()

                # 1. Filter: Check for 7 columns
                if len(columns) != 7:
                    continue

                # 2. Filter: Check if the 5th column (index 4) is "tau"
                # Note: The original 5th column is excluded from the output.
                if columns[4] != "tau":
                    continue

                col_4_str = columns[3]

                try:
                    # Attempt to convert the 4th column to float
                    col_4_float = float(col_4_str)

                    # Store data required for the output, including the original Col 4 float value
                    # and the original Col 7 string value (now the final 'tau' column)
                    record = [columns[0], columns[1], columns[2], col_4_float, columns[6], line_number]
                    valid_records.append(record)

                    # Count the repetition for this specific Col 4 float value
                    col4_counts[col_4_float] += 1

                except ValueError:
                    print(
                        f"Error on line {line_number}: 4th column ('{col_4_str}') is not a valid number. Skipping record.")
                except Exception as e:
                    print(f"An unexpected error occurred processing line {line_number}: {e}")

        # --- PASS 2: Calculate Distributed Value and Final Result ---
        print(f"\nStarting PASS 2: Calculating distributed values and final results.")

        # Tracks the current distribution index (i) for each Col 4 group
        col4_current_index = defaultdict(int)
        final_output_lines = []
        success_count = 0

        for record in valid_records:
            col_1, col_2, col_3, v_4, col_7, current_line_num = record

            # 1. Get total count (N) for this V_4 value
            n_total = col4_counts[v_4]

            # 2. Get and increment the unique index (i) for this V_4 group
            i_index = col4_current_index[v_4]
            col4_current_index[v_4] += 1

            # 3. Calculate the increment step: delta * (i / N)
            increment_step = DELTA * (i_index / n_total)

            # 4. Calculate the new, distributed 5th column value (V'_5)
            # This value is spread between V_4 and V_4 + 0.001
            v_prime_5 = v_4 + increment_step

            # 5. Calculate the new 6th column value (V''_6) using the distributed V'_5
            v_double_prime_6 = ""
            if v_prime_5 == 0.0:
                print(f"Warning on line {current_line_num}: Division by zero in final calculation (New Col 5 is zero).")
                v_double_prime_6 = "DIV_BY_ZERO"
            else:
                v_double_prime_6 = CONSTANT_DIVIDER / v_prime_5

            # 6. Build the final output line (7 columns)
            # SpN, high, low, wavelength, wavelength-distributed, energy, tau
            output_columns = [
                str(col_1),  # 1st column (SpN)
                str(col_2),  # 2nd column (high)
                str(col_3),  # 3rd column (low)
                str(v_4),  # 4th column (wavelength)
                f"{v_prime_5:.5f}",  # 5th column (wavelength-distributed, V'_5), FORMATTED TO 5 DECIMAL PLACES
                f"{v_double_prime_6}",  # 6th column (energy, V''_6)
                str(col_7)  # 7th column (tau, original Col 7)
            ]

            # Join the columns with a COMMA delimiter for CSV output
            final_output_lines.append(",".join(output_columns))
            success_count += 1

        # Write all successfully processed lines to the output file
        with open(OUTPUT_FILE_PATH, 'w') as outfile:
            # 1. Write the header row
            header = "SpN,high,low,wavelength,wavelength-distributed,energy,tau"
            outfile.write(header + "\n")

            # 2. Write the data rows
            for line in final_output_lines:
                outfile.write(line + "\n")

        print("\n--- Processing Complete ---")
        print(f"Total lines processed (passes 1 & 2): {line_number}")
        print(f"Valid records processed: {len(valid_records)}")
        print(f"Lines written to {OUTPUT_FILE_PATH}: {success_count}")

    except FileNotFoundError:
        print(f"Error: Input file not found at path: {INPUT_FILE_PATH}")
    except Exception as e:
        print(f"A critical error occurred: {e}")


if __name__ == "__main__":
    process_data_file()