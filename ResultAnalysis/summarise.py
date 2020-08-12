from glob import glob

import pandas as pd

from main import save_dataframe


def summarise_univariate(path, suffix):
    def join(x):
        if isinstance(x, tuple):
            return ' '.join(x)
        else:
            return x
    df = read_all(path, suffix, ['name', 'precision', 'recall', 'mcc'])
    means = df.groupby('name').agg(['mean', 'std'])
    means.columns = means.columns.map(' '.join)
    save_dataframe(means, path, 'means')
    medians = df.groupby('name').median()
    save_dataframe(medians, path, 'medians')


def read_all(path, suffix, columns):
    all_df = pd.DataFrame(columns=columns)
    for file in glob(path + '*' + suffix):
        df = pd.read_csv(file)[columns]
        all_df = all_df.append(df, ignore_index=True)
    return all_df


if __name__ == '__main__':
    summarise_univariate('results/univariate/functions/', 'functions.csv')
    summarise_univariate('results/univariate/objects/', 'objects.csv')
