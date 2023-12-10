#!/usr/bin/env python3

# Scans a directory (usually Data, but you need to pass it on the command line)
# looking for directories holding entropy computation results and uploads those
# to a postgresql database.  See also copy_data_from_postgresql.py, which goes
# the other way.

import os
import re
import sys
import psycopg2

# Database connection parameters - modify these with your database details.
db_params = {
#  'database': 'your_database',
#  'user': 'your_username',
#  'password': 'your_password',
#  'host': 'localhost',
#  'port': '5432'
}

# Function to create the table if it doesn't exist.
def create_table_if_not_exists(cursor):
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS lozi (
        a_num INTEGER NOT NULL,
        a_den INTEGER NOT NULL CHECK(a_den > 0),
        b_num INTEGER NOT NULL,
        b_den INTEGER NOT NULL CHECK(b_den > 0),
        lower NUMERIC NOT NULL,
        upper NUMERIC NOT NULL,
        lwho TEXT NOT NULL,
        uwho TEXT NOT NULL,
        comment TEXT,
        tough BOOLEAN NOT NULL DEFAULT false,
        PRIMARY KEY (a_num, a_den, b_num, b_den),
        CHECK(gcd(a_num, a_den) = 1),
        CHECK(gcd(b_num, b_den) = 1),
        CHECK(lower <= upper)
    )
    """)

# Function to insert data into the database.
def insert_data(cursor, data):
    query = """
    INSERT INTO lozi (a_num, a_den, b_num, b_den, lower, upper, lwho, uwho, comment)
    VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s)
    ON CONFLICT (a_num, a_den, b_num, b_den) DO UPDATE
    SET lower=greatest(lozi.lower,EXCLUDED.lower),
       upper=least(lozi.upper,EXCLUDED.upper),
      lwho=(CASE WHEN EXCLUDED.lower > lozi.lower
            THEN EXCLUDED.lwho ELSE lozi.lwho END),
      uwho=(CASE WHEN EXCLUDED.upper < lozi.upper
            THEN EXCLUDED.uwho ELSE lozi.uwho END)
    """
    cursor.execute(query, data)

# Function to verify directory name matches file contents.
def verify_directory_name(dir_name, data):
    expected_dir_name = '_'.join(data[:4])
    return dir_name == expected_dir_name

# Function to process each directory
def process_directory(dir_path):
    files = ['a_num', 'a_den', 'b_num', 'b_den', 'lower', 'upper', 'lwho', 'uwho', 'comment']
    data = []
    
    for file in files:
        with open(os.path.join(dir_path, file), 'r') as f:
            data.append(f.read().strip())

    return data

# Main script
def main(base_path):
    dir_pattern = re.compile(r'^-?\d+_-?\d+_-?\d+_-?\d+$')

    with psycopg2.connect(**db_params) as conn:
        with conn.cursor() as cursor:
            create_table_if_not_exists(cursor)

            for dir_name in os.listdir(base_path):
                if dir_pattern.match(dir_name):
                    dir_path = os.path.join(base_path, dir_name)
                    data = process_directory(dir_path)

                    # Sanity check for directory name
                    if verify_directory_name(dir_name, data):
                        # Insert data into the database
                        insert_data(cursor, data)
                    else:
                        print(f"Directory name mismatch for {dir_name}")

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: " + sys.argv[0] + " <data directory>")
        sys.exit(1)
    base_path = sys.argv[1]
    main(base_path)
