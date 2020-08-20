import argparse
import os

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LinearSegmentedColormap, LogNorm

from main import projects, get_metric_results


def scatter(df, subfolder, name, x_axis, y_axis):
    df = df.round({x_axis: 1, y_axis: 1})
    df = df.groupby([x_axis, y_axis, ]).size().reset_index(name='count')
    df.plot.scatter(x_axis, y_axis, s=df['count'], alpha=0.5)
    plt.xlabel('Functional score')
    plt.ylabel('Imperative score')
    plt.title(name)
    savefig('scatter/' + subfolder, name, '.pdf')


def scatter_faults(df, subfolder, name, x_axis, y_axis):
    def to_color(faults):
        if faults == 0:
            return 'blue'
        else:
            return 'red'

    df = df.round({x_axis: 1, y_axis: 1})
    df['color'] = df['faults'].apply(to_color)
    df = df.groupby([x_axis, y_axis, 'color']).size().reset_index(name='count')
    df.plot.scatter(x_axis, y_axis, c=df['color'], s=df['count'], alpha=0.5)
    plt.xlabel('Functional score')
    plt.ylabel('Imperative score')
    plt.title(name)
    savefig('scatter-faults/' + subfolder, name, '.pdf')


def scatter_color(df, subfolder, name, x_axis, y_axis):
    df = df.round({x_axis: 1, y_axis: 1})
    df = df.groupby([x_axis, y_axis, ]).size().reset_index(name='count')
    cmap = LinearSegmentedColormap.from_list('gyr', [(0, 'green'), (0.5, 'yellow'), (1, 'red')], N=256)
    df.plot.scatter(x_axis, y_axis, c=df['count'], cmap=cmap, alpha=0.5, norm=LogNorm(),
                    edgecolors='none', s=12.75, marker="s")
    plt.xlim(-0.25, 7.69)
    plt.ylim(-0.25, 7.13)
    plt.xlabel('Functional score')
    plt.ylabel('Imperative score')
    plt.title(name)
    savefig('scatter-color/' + subfolder, name, '.pdf')


def hist_faults(df, subfolder, name, score_axis, has_points_axis):
    def calc_bin_points(row):
        # Shift scores with points to create gap
        if row[has_points_axis] == 0:
            return row[score_axis]
        elif row[score_axis] >= 0:
            return row[score_axis] + 0.1
        else:
            return row[score_axis] - 0.1

    bin_axis = 'binPoints'
    df[bin_axis] = df.apply(calc_bin_points, axis=1)
    non_faulty = df[df['faults'] == 0]
    faulty = df[df['faults'] > 0]
    plt.hist(
        np.array([faulty[bin_axis], non_faulty[bin_axis]], dtype=object),
        bins=[-1.1, -0.9, -0.7, -0.5, -0.3, -0.09, 0.09, 0.3, 0.5, 0.7, 0.9, 1.1],
        stacked=True,
        color=['lightcoral', 'darkseagreen'],
        edgecolor='white'
    )
    plt.xticks(
        ticks=[-1.1, -0.9, -0.7, -0.5, -0.3, -0.1, 0.0, 0.1, 0.3, 0.5, 0.7, 0.9, 1.1],
        labels=['-1.0', '-0.8', '-0.6', '-0.4', '-0.2', '0.0', 'No points', '0.0', '0.2', '0.4', '0.6', '0.8', '1.0']
    )
    no_points_label = plt.gca().xaxis.get_majorticklabels()[6]
    no_points_label.set_rotation(90)
    no_points_label.set_y(0.22)
    plt.xlabel('Paradigm score')
    plt.ylabel('Occurrences')
    plt.title(name)
    savefig('hist-faults/' + subfolder, name, '.pdf')


def savefig(directory, filename, extension):
    if args.write:
        directory = f'../data/analysisResults/paradigmScore/plots/{directory}'
        os.makedirs(directory, exist_ok=True)
        plt.savefig(
            directory + filename + extension,
            bbox_inches='tight',
            metadata={'Creator': None, 'Producer': None, 'CreationDate': None}
        )
    if args.show:
        plt.show()
    plt.close()


def plot_methods(project, name):
    df = get_metric_results('paradigmScore', project, 'methodResultsBriand')
    name = name + ' methods'
    subfolder = 'methods/'
    if args.scatter:
        scatter(df, subfolder, name, 'FunctionalScoreFraction', 'ImperativeScoreFraction')
    if args.scatter_faults:
        scatter_faults(df, subfolder, name, 'FunctionalScoreFraction', 'ImperativeScoreFraction')
    if args.scatter_color:
        scatter_color(df, subfolder, name, 'FunctionalScoreFraction', 'ImperativeScoreFraction')
    if args.hist:
        hist_faults(df, subfolder, name, 'ParadigmScoreFraction', 'HasPointsFraction')


def plot_objects(project, name):
    df = get_metric_results('paradigmScore', project, 'objectMethodResultsBriand')
    name = name + ' objects'
    subfolder = 'objects/'
    if args.scatter:
        scatter(df, subfolder, name, 'FunctionalScoreFractionAvr', 'ImperativeScoreFractionAvr')
    if args.scatter_faults:
        scatter_faults(df, subfolder, name, 'FunctionalScoreFractionAvr', 'ImperativeScoreFractionAvr')
    if args.scatter_color:
        scatter_color(df, subfolder, name, 'FunctionalScoreFractionAvr', 'ImperativeScoreFractionAvr')
    if args.hist:
        hist_faults(df, subfolder, name, 'ParadigmScoreFractionAvr', 'HasPointsFractionMax')


def main():
    for project, name in projects.items():
        if project in args.projects:
            if not args.skip_methods:
                plot_methods(project, name)
            if not args.skip_objects:
                plot_objects(project, name)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--projects', help='Select projects', dest='projects', nargs='+', default=projects.keys())
    parser.add_argument('--skip-methods', help='Skip Methods', dest='skip_methods', action="store_true")
    parser.add_argument('--skip-objects', help='Skip objects', dest='skip_objects', action="store_true")
    parser.add_argument('--show', help='Show plots', dest='show', action="store_true")
    parser.add_argument('--write', help='Write plots', dest='write', action="store_true")
    parser.add_argument('--scatter', help='Create paradigm score scatter plots', dest='scatter', action="store_true")
    parser.add_argument('--scatter-faults', help='Create paradigm score scatter plots showing faulty scores',
                        dest='scatter_faults', action="store_true")
    parser.add_argument('--scatter-color', help='Create colored paradigm score scatter plots', dest='scatter_color',
                        action="store_true")
    parser.add_argument('--hist', help='Show plots', dest='hist', action="store_true")
    args = parser.parse_args()
    main()
