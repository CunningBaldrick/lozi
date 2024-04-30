#!/usr/bin/env python3

# Downloads entropy computation results from postgresql and outputs them into
# a directory (usually Data, but you need to pass it on the command line),
# updating whatever was there.  See also copy_data_to_postgresql.py, which goes
# the other way.

import argparse
import os
import psycopg2
import sys

COL_NAMES = ['a_num', 'a_den', 'b_num', 'b_den', 'lower', 'upper', 'lwho', 'uwho', 'comment']

def create_directory(base_path, row):
    dir_name = f'{row[0]}_{row[1]}_{row[2]}_{row[3]}'
    path = os.path.join(base_path, dir_name)
    os.makedirs(path, exist_ok=True)
    return path

def write_to_file(path, filename, data):
    with open(os.path.join(path, filename), 'w') as file:
        if data is not None:
            file.write(data + '\n')

def main(target_directory, db_params):
    with psycopg2.connect(**db_params) as conn:
        with conn.cursor() as cursor:
            # In order to not lose precision, convert the numeric columns (in
            # fact all columns) to text in postgres, as the database carefully
            # preserves floating point accuracy.
            cols_as_text = [col + '::text' for col in COL_NAMES]
            cursor.execute('SELECT ' + ','.join(cols_as_text) + ' FROM lozi')
            for row in cursor.fetchall():
                sub_dir = create_directory(target_directory, row)
                for i, col in enumerate(COL_NAMES):
                    write_to_file(sub_dir, col, row[i])

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Download data from PostgreSQL and save into directory.')
    parser.add_argument('--target_directory', type=str, help='Directory where data will be stored')
    parser.add_argument('--database', default=os.getenv('PGDATABASE', 'lozi'), type=str, help='Database name')
    parser.add_argument('--host', type=str, help='Database host')
    parser.add_argument('--password', type=str, help='Database password')
    parser.add_argument('--port', type=str, help='Database port')
    parser.add_argument('--user', type=str, help='Database user')

    args = parser.parse_args()

    # Build db_params dictionary from command line arguments
    db_params = {
        key: value for key, value in vars(args).items() if key in {'database', 'user', 'password', 'host', 'port'} and value is not None
    }

    main(args.target_directory, db_params)
