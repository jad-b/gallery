#!/usr/bin/env python3
"""
nextup.py
===
Parse and display lines in the form of DUE_DATE,DESCRIPTION,ASSIGNED_DATE.
This makes easy work of a simple reminder list.

Future Work:
    - Allow for sorting on due or assigned date
"""
from datetime import datetime, date
from io import StringIO
import argparse
import csv
import itertools
import sys

from colorama import Fore, Style


DATE_FORMAT = "%Y%m%d"


def parse_date(string, fmt):
    return datetime.strptime(string, fmt).date()


class PendingWork:
    display_fmt = "{x.due_date}] {x.description} ({x.assigned_date})"

    def __init__(self, due_date, description, assigned_date):
        self.due_date = parse_date(due_date, DATE_FORMAT)
        self.description = description
        self.assigned_date = parse_date(assigned_date, DATE_FORMAT)

    def __str__(self):
        return self.display_fmt.format(x=self)

    __repr__ = __str__

    @staticmethod
    def parse(taskfile, cutoff):
        # Skip lines with '#' and any amount of whitespace
        for row in taskfile:
            if (row.strip() == "" or row.startswith('#')):
                continue
            break
        # Glue the row we peeked at to the front of the remaining lines
        file_lines = itertools.chain([row], taskfile)
        # Parse and display each of the waiting for lines
        for row in csv.reader(file_lines):
            pending = PendingWork(*row)
            if pending.due_date > cutoff:
                break
            yield pending


def format_work(*work):
    """Print a list of PendingWork"""
    sio = StringIO()
    sio.write(Fore.BLUE)
    if work:
        sio.write("\n".join((str(x) for x in work)))
    else:
        sio.write("No tasks due today.")
    sio.write(Style.RESET_ALL)
    return sio.getvalue()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("infile", type=argparse.FileType())
    parser.add_argument("--date", type=lambda x: parse_date(x, DATE_FORMAT),
                        default=date.today())
    args = parser.parse_args()
    print("Searching for tasks on or before", str(args.date), file=sys.stderr)
    tasks = list(PendingWork.parse(args.infile, args.date))
    print(format_work(*tasks))


if __name__ == "__main__":
    main()
