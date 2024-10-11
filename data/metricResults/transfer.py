import csv
from re import sub

import verify as v


def check_idempotence(source: str, target: str) -> None:
    """
    Checks if the transfer has already happened.
    :param source: the source file.
    :param target:
    :return: True if the transfer already happened, False if it is good to go
    """
    with open(source, 'r') as s, open(target, 'r') as t:
        source_header = next(csv.reader(s))
        target_header = next(csv.reader(t))
        if any(h in target_header[4:] for h in source_header[4:]):
            raise ValueError(f'The following columns overlap between {source} and {target}: ',
                  filter(lambda h: h in target_header[4:], source_header[4:]))


def execute_transfer(source: str, target: str) -> None:
    """
    Transfers the last two columns for the matching rows.
    Inserts 0.0 when there is no matching source row.
    Removes the columns from the source file.
    :param source: The source file. Does not need to have all rows of target.
    :param target: The target file. Must have a row for each source row, in the same order.
    :return: None once it is done
    """
    print(f'Transferrring from {source} to {target}...')
    new_source: list[list[str]] = []
    new_target: list[list[str]] = []

    with open(source, 'r') as s, open(target, 'r') as t:
        source_reader = csv.reader(s)
        source_header = next(source_reader)
        target_reader = csv.reader(t)
        target_header = next(target_reader)

        new_source.append(source_header[:-2])
        new_target.append(target_header + [sub(r'.*object', '', source)[:3] + h for h in source_header[-2:]])

        source_rows = [row for row in source_reader]
        i = 0

        for target_row in target_reader:
            if i in range(len(source_rows)) and source_rows[i][:3] == target_row[:3]:
                # Found matching source row, append entropy and Gini values, then advance the source row.
                new_target.append(target_row + source_rows[i][-2:])
                new_source.append(source_rows[i][:-2])
                i += 1
            else:
                # No matching source row, enter 0 as a default and hold the source row.
                new_target.append(target_row + ['0.0'] * 2)

    with open(source, 'w', newline='') as s, open(target, 'w', newline='') as t:  # wipes the files before opening
        source_writer = csv.writer(s)
        source_writer.writerows(new_source)
        target_writer = csv.writer(t)
        target_writer.writerows(new_target)


if __name__ == '__main__':
    v.process_filepairs(check_idempotence)
    print('No previous value transfer detected, starting value transfer')
    v.process_filepairs(execute_transfer)
    print('Finished value transfer')
