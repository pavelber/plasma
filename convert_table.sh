#!/bin/bash

# Step 1: Remove empty lines and read all numbers into a one-dimensional array
declare -a numbers
number_count=0
while IFS= read -r line; do
    # Skip empty lines
    if [[ -n "$line" ]]; then
        # Split line into numbers and append to array
        read -ra nums <<< "$line"
        for num in "${nums[@]}"; do
            numbers[$number_count]="$num"
            ((number_count++))
        done
    fi
done

# Step 2: Verify we have 120 numbers
if [[ $number_count -ne 120 ]]; then
    echo "Error: Expected 120 numbers, found $number_count" >&2
    exit 1
fi

# Step 3: Fill a 2D table (15 rows, 8 columns)
declare -A table
index=0
for ((row=0; row<15; row++)); do
    for ((col=0; col<8; col++)); do
        table[$row,$col]=${numbers[$index]}
        ((index++))
    done
done

# Step 4: Print by columns, comma-separated
for ((col=0; col<8; col++)); do
    # Collect all values for the current column
    column_values=()
    for ((row=0; row<15; row++)); do
        column_values+=("${table[$row,$col]}")
    done
    # Join values with commas and print
    printf "%s\n" "$(IFS=','; echo "${column_values[*]}")"
done