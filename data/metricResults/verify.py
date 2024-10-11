import csv
import os
from typing import Callable

source_folders = ['paradigmScoreBool', 'paradigmScoreCount', 'paradigmScoreFraction']
target_folder = 'paradigmSwitching'

file_suffixes = ['Briand', 'Landkroon']
aggregates = ['Avr', 'Max', 'Sum']
method_pattern = 'methodResults{}.csv'
object_pattern = 'object{}Results{}.csv'


def compare_left(source_file, target_file):
    if not os.path.exists(source_file) or not os.path.exists(target_file):
        raise FileNotFoundError(f'Error: file {source_file} or {target_file} not found.')
    with open(source_file, 'r') as s, open(target_file, 'r') as t:
        source_reader = csv.reader(s)
        source_header = next(source_reader)
        target_reader = csv.reader(t)
        target_header = next(target_reader)

        if source_header[:3] != target_header[:3]:
            return False
        source = [row[:3] for row in source_reader]
        target = [row[:3] for row in target_reader]
        i, j = 0, 0

        while i < len(source) and j < len(target):
            if source[i] == target[j]:
                i += 1
            j += 1

        return i >= len(source) or j != len(target)


def compare_left_soft(source_file, target_file):
    if not os.path.exists(source_file) or not os.path.exists(target_file):
        raise FileNotFoundError(f'Error: file {source_file} or {target_file} not found.')
    with open(source_file, 'r') as s, open(target_file, 'r') as t:
        source_reader = csv.reader(s)
        source_header = next(source_reader)
        target_reader = csv.reader(t)
        target_header = next(target_reader)

        if source_header[:3] != target_header[:3]:
            return False
        # Read all rows into lists
        source = sorted([row[:3] for row in source_reader])
        target = sorted([row[:3] for row in target_reader])
        i, j = 0, 0

        while i < len(source) and j < len(target):
            if source[i] == target[j]:
                i += 1
            j += 1

        return i >= len(source) or j != len(target)


def assert_match(source_file, target_file):
    if not compare_left(source_file, target_file):
        raise ValueError(f'Error: {source_file} and {target_file} do not have matching rows')


def process_filepairs(f: Callable[[str, str], None]) -> None:
    for subfolder in os.listdir(target_folder):
        for suffix in file_suffixes:
            for source_folder in source_folders:
                source_file = os.path.join(source_folder, subfolder, method_pattern.format(suffix))
                target_file = os.path.join(target_folder, subfolder, method_pattern.format(suffix))
                f(source_file, target_file)

                target_file = os.path.join(target_folder, subfolder, object_pattern.format('', suffix))
                for agg in aggregates:
                    source_file = os.path.join(source_folder, subfolder, object_pattern.format(agg, suffix))
                    f(source_file, target_file)


process_filepairs(assert_match)
print('Data files are good to go')
