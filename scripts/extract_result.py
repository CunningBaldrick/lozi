#!/usr/bin/env python3

# Parses output files produced by the calculate_entropy program, with or
# without -v, and outputs all (lower bound, upper bound) pairs, if any.

import re
import sys

def extract_entropy_values(file_path):
    pattern = r"\s(0\.\d+(E[+-]\d+)?)\s<=\sEntropy\s<=\s(0\.\d+(E[+-]\d+)?)$"

    extracted_pairs = []
    try:
        with open(file_path, 'r') as file:
            for line in file:
                # Check if the line ends with a newline character.
                # If not, the program might have died in the middle
                # of printing out the upper bound, in which case we
                # don't want to use the maybe truncated value.
                if line.endswith('\n'):
                    match = re.search(pattern, line)
                    if match:
                        num1, num2 = match.group(1), match.group(3)
                        extracted_pairs.append((num1, num2))
    except FileNotFoundError:
        print(f"File not found: {file_path}")
        sys.exit(1)

    return extracted_pairs

if len(sys.argv) != 2:
    print("Usage: extract_result.py <file_path>")
    sys.exit(1)

file_path = sys.argv[1]
extracted_pairs = extract_entropy_values(file_path)
for pair in extracted_pairs:
    print(pair[0], pair[1])
