from analysis import categories, projects, save_dataframe, get_metric_results, get_columns, parse_args


def main(args):
    folder = f'{args.folder}/regression/descriptive/'
    for category in categories:
        for path, name in projects.items():
            df = get_metric_results(args.folder, path, category)
            if df is not None:
                descriptive(df, folder, category, name, args)


def descriptive(df, folder, category, name, args):
    print(f'[{category}] Descriptive: {name}')
    columns = get_columns(df, args)
    statistics = df[columns].describe().T.rename_axis('name', axis=0)
    statistics['count'] = statistics['count'].astype(int)
    save_dataframe(statistics, folder + category, name)


if __name__ == '__main__':
    main(parse_args())
