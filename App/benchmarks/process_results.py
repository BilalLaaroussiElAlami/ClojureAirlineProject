#!/usr/bin/env python3
# This is a Python script that parses the benchmark results, calculates some statistics,
# and generates graphs. You can use it as a template and extend it as necessary.
# (You are not required to use this.)

# To get started, you need to create a Python virtual environment and install the
# dependencies, like this:
# $ python3 -m venv venv
# $ source venv/bin/activate
# $ pip install -r requirements.txt
#
# We use numpy [1] for statistics and matplotlib [2] for plotting.
#
# When running this script, make sure the virtual environment is activated!
# $ source venv/bin/activate
# $ python3 process_results.py
#
# [1] https://numpy.org/
# [2] https://matplotlib.org/

import logging
import glob
import re
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt


# Name of the experiment.
EXPERIMENT_NAME = "inputs"
# Name of the benchmark. File names should be be result-$NAME-$ITERATION.txt.
BENCHMARK_NAME = r"result-(?P<name>.+)-(?P<iteration>\d+).txt"
# Expected benchmark names
NAMES = ["simple", "random"]
# Number of iterations expected per benchmark
NUMBER_OF_ITERATIONS = 30


# Set log level.
logging.basicConfig(level=logging.INFO)


def parse_file(filename: str, name: str) -> tuple[float, list[str]]:
    """Parses a benchmark result file and returns a tuple with a result
    and a list of parameters."""
    with open(filename) as f:
        lines = f.readlines()

    # The first six lines contain the parameters.
    parameters = lines[:6]

    # The result is on the next line, and looks like "Elapsed time: 67.322677 msecs"
    line = r"\"Elapsed time: ([\d.]+) msecs\"\n"
    m = re.match(line, lines[6])
    # Check if lines are as expected
    if not m:
        raise Exception(f"Unexpected line ({filename}, line 7):\n" + lines[6])
    time = float(m.group(1))

    return (time, parameters)


def parse_files(names: list[str]) -> dict[str, list[float]]:
    """Parse the benchmark files and return the results."""
    results = {name: [] for name in NAMES}
    parameters = {name: [] for name in NAMES}
    for filename in names:
        m = re.match(BENCHMARK_NAME, filename)
        if not m:
            continue
        name = m.group("name")
        if name not in NAMES:
            raise Exception(f"Unexpected benchmark name: {name}")
        result, pars = parse_file(filename, name)
        results[name].append(result)
        parameters[name].append(pars)
    # Check if parameters match across all files of the same benchmark.
    for name, pars in parameters.items():
        for p in pars:
            if p != pars[0]:
                raise Exception(
                    f"Parameters do not match for all iterations of benchmark {name}:\n"
                    + f"{p} != {pars[0]}"
                )
        logging.info(f"Parameters for {name}:\n%s", "".join(pars[0]))
    return results


def calculate_speedups(results: dict[int, list[float]]) -> dict[int, list[float]]:
    """Calculate the speedups for all results.

    The base time is the median of the results for 1 thread."""
    base = np.median(results[1])
    speedups = {i: [base / t for t in result] for (i, result) in results.items()}
    return speedups


def plot_boxplots(times: dict[str, list[float]]):
    """Plot the results as boxplots."""
    mpl.rcParams.update({"font.size": 16})
    fig, ax = plt.subplots(figsize=(10, 5))
    ax.boxplot([times[name] for name in times], notch=True, bootstrap=1000)
    # Note: the 'notch' represents the confidence interval, calculated using
    # bootstrapping, a statistical technique that resamples the data.
    # Note: matplotlib draws the whiskers at 1.5 * IQR from the 1st and 3rd quartile.
    # Points outside the whiskers are plotted as outliers (individual circles).
    # Different styles exist.
    # See https://matplotlib.org/stable/api/_as_gen/matplotlib.axes.Axes.boxplot.html
    ax.set_title(EXPERIMENT_NAME)
    ax.set_xlabel("Input file")
    ax.set_ylabel("Time (ms)")
    ax.set_xticks(list(range(1, len(times) + 1)))
    ax.set_xticklabels(list(times.keys()))
    ax.set_ylim(0, 1.1 * max([max(times[n]) for n in times]))
    fig.savefig(f"result-{EXPERIMENT_NAME}-boxplot.pdf")


def plot_violinplots(times: dict[str, list[float]]):
    """Plot the results as violin plots (alternative)."""
    mpl.rcParams.update({"font.size": 16})
    fig, ax = plt.subplots(figsize=(10, 5))
    ax.violinplot([times[name] for name in times], showmedians=True)
    # See https://matplotlib.org/stable/api/_as_gen/matplotlib.axes.Axes.violinplot.html
    ax.set_title(EXPERIMENT_NAME)
    ax.set_xlabel("Input file")
    ax.set_ylabel("Time (ms)")
    ax.set_xticks(list(range(1, len(times) + 1)))
    ax.set_xticklabels(list(times.keys()))
    ax.set_ylim(0, 1.1 * max([max(times[n]) for n in times]))
    fig.savefig(f"result-{EXPERIMENT_NAME}-violinplot.pdf")


def plot_errorbars(times: dict[str, list[float]]):
    """Plot the results as plot with error bars."""
    mpl.rcParams.update({"font.size": 16})
    fig, ax = plt.subplots(figsize=(10, 5))
    x = list(times.keys())                  # ["random", "simple"]
    x_numbers = list(range(1, len(x) + 1))  # [1, 2]
    medians = [np.median(times[name]) for name in times]
    errors_down = [np.median(times[n]) - np.quantile(times[n], 0.25) for n in times]
    errors_up = [np.quantile(times[n], 0.75) - np.median(times[n]) for n in times]
    ax.errorbar(x_numbers, y=medians, yerr=[errors_down, errors_up], ls="none")
    ax.set_title(EXPERIMENT_NAME)
    ax.set_xlabel("Input file")
    ax.set_ylabel("Time (ms)")
    ax.set_xticks(x_numbers)
    ax.set_xticklabels(x)
    ax.set_ylim(0, 1.1 * max([max(times[n]) for n in times]))
    fig.savefig(f"result-{EXPERIMENT_NAME}-errorbars.pdf")


def main():
    # Gather benchmark files, i.e. all files in this directory that match the regex.
    files = glob.glob("result-*.txt")

    # Parse benchmark results.
    results = parse_files(files)
    logging.info("Results: %s", results)

    # Calculate and print 1st quartile, median, and 3rd quartile.
    # These may be useful for the report.
    time_quartiles = {
        i: np.quantile(results[i], [0.25, 0.5, 0.75]) for i in results
    }
    logging.info("Time quartiles: %s", time_quartiles)

    # Plot results using box plots.
    plot_boxplots(results)

    # Alternative: plot using violin plots.
    plot_violinplots(results)

    # Alternative: plot using graph with error bars.
    plot_errorbars(results)


if __name__ == "__main__":
    main()
