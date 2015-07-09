#!/usr/bin/env python
# Take a makefile and use Make and post-processing to convert it to a dot-file.

import re
import os
import sys
import argparse
import itertools
import subprocess as sp
import graphviz as gv

kdescription = """Generate a Graphviz dependency graph from a Makefile."""

kspecial_targets = [
    ".PHONY",
    ".SUFFIXES",
    ".DEFAULT",
    ".PRECIOUS",
    ".INTERMEDIATE",
    ".SECONDARY",
    ".SECONDEXPANSION",
    ".DELETE_ON_ERROR",
    ".IGNORE",
    ".LOW_RESOLUTION_TIME",
    ".SILENT",
    ".EXPORT_ALL_VARIABLES",
    ".NOTPARALLEL",
    ".ONESHELL",
    ".POSIX",
]

def read_makefile(makefile):
    """Run GNU Make to retrieve the Makefile database.

    .. Keyword Arguments:
    :param: makefile: The name of the Makefile to run.

    .. Types:
    :type: makefile: A string.

    .. Returns:
    :returns: The complete Makefile database.
    :rtype: A string.

    """
    cmd = ["make", "--print-data-base"]
    if makefile:
        cmd.extend(["--file", makefile])
    return sp.check_output(cmd).split("\n");


def filter_make_database(make_lines, source_dir):
    """Filter the Make database to retrieve the desired dependency graph.

    .. Keyword Arguments:
    :param: make_lines: The makefile database.
    :param: source_dir: The base directory where Make was executed.

    .. Types:
    :type: make_lines: A list of strings.
    :type: source_dir: A string.

    .. Returns:
    :returns: The Makefile dependency graph.
    :rtype: A dictionary of sets.

    """
    idx = 0
    # Jump to the Files section.
    while not make_lines[idx].startswith("# Files"):
        idx += 1

    # Create a dictionary over all targets and their pre-requisites.
    targets = dict()
    regex = "^%s/?" % source_dir
    for i in range(idx, len(make_lines)):
        line = make_lines[i]
        match = re.match("([^#].+)\s*:\s*(.*)", line)
        if match:
            k, v = match.group(1, 2)
            k = re.sub(regex, "", k)
            if v == "":
                targets[k] = set()
            else:
                deps = re.split(r"\s*\|\s*|\s+", v)
                targets[k] = {re.sub(regex, "", d) for d in deps if d}
        elif line.startswith("# Finished Make data base"):
            idx = i
            break

    return targets


def filter_graph(graph,
                 keep_suffix_rules,
                 keep_special_rules,
                 delete_unnecessary,
                 ignore):
    """Filter out the part of the graph that we aren't interested in.

    .. Keyword Arguments:
    :param: graph: The graph to be filtered.
    :param: keep_suffix_rules: Should we keep the old-style suffix rules?
    :param: keep_special_rules: Should we keep the special make targets?
    :param: delete_unnecessary: Should we keep isolated vertices?
    :param: ignore: Additional targets to be ignored.

    .. Types:
    :type: graph: A dictionary depicting the Makefile graph.
    :type: keep_suffix_rules: Boolean.
    :type: keep_special_rules: Boolean.
    :type: delete_unnecessary: Boolean.
    :type: ignore: List.

    """
    cond = lambda k: (not keep_special_rules and k in kspecial_targets or
                      not keep_suffix_rules and k.startswith(".") or
                      ignore and k in ignore or k == "")
    for k, v in graph.items():
        if cond(k):
            del graph[k]
        else:
            graph[k] = set(itertools.ifilterfalse(cond, graph[k]))

    if not delete_unnecessary:
        return

    for k, v0 in graph.items():
        if not v0 and all(k not in s for s in graph.values()):
            del graph[k]
    return


def generate_graph(dotfile, targets, merge_edges = False):
    """Generate the Makefile dependency graph in Graphviz dot-format.

    .. Keyword Arguments:
    :param: dotfile: The name for the output dotfile.
    :param: targets: The Makefile dependency graph.
    :param: merge_edges: Should 2 or more edges be merged? (default: False)

    .. Types:
    :type: dotfile: A string
    :type: targets: A dictionary of sets.
    :type: targets: A boolean.

    """
    g = gv.Digraph(name="Makefile",
                   comment="Generated using make2dot.",
                   format="pdf",
                   engine="dot")
    g.attr('graph',
           rankdir="BT",
           concentrate="true",
           ranksep="2.0",
           splines="true")
    for v, e in targets.items():
        g.node(v)
        if merge_edges and len(e) > 2:
            merge_target_edges(g, v, e)
        else:
            g.edges(zip(e, itertools.repeat(v)))

    return g.save(dotfile)


def merge_target_edges(graph, vertex, edges):
    """Merge the edges pointing to the given vertex.

    .. Keyword Arguments:
    :param: graph: The graph the vertex and edges belong to.
    :param: vertex: The vertex the edges are point to.
    :param: edges: The edges pointing at the vertex.

    .. Types:
    :type: graph: A Graphviz graph.
    :type: vertex: A string with the vertex name.
    :type: edges: A list of strings with the edges.

    """
    virt_v = "_virtual_" + vertex
    graph.node(virt_v, shape="point", width="0.01", heigh="0.01")
    for e in edges:
        graph.edge(e, virt_v, dir="none")
    graph.edge(virt_v, vertex)


def main(makefile,
         dotfile,
         source_dir,
         keep_suffix_rules,
         keep_special_rules,
         delete_unnecessary,
         merge_edges,
         ignore):
    """Parse the Makefile database output and generate the dependency graph.

    .. Keyword Arguments:
    :param: makefile: The makefile to generate the dependency graph from.
    :param: source_dir: The source directory for the makefile.
    :param: keep_suffix_rules: Should we keep the old-style suffix rules?
    :param: keep_special_rules: Should we keep the special make targets?
    :param: delete_unnecessary: Should we keep isolated vertices?
    :param: merge_edges: Should targets with more than 2 deps. be merged?
    :param: ignore: Additional targets to be ignored.

    .. Types:
    :type: makefile: A string.
    :type: source_dir: A string.
    :type: keep_suffix_rules: Boolean.
    :type: keep_special_rules: Boolean.
    :type: delete_unnecessary: Boolean.
    :type: merge_edges: Boolean.
    :type: ignore: List.

    """
    make_lines = read_makefile(makefile)
    targets = filter_make_database(make_lines, source_dir)
    filter_graph(targets,
                 keep_suffix_rules,
                 keep_special_rules,
                 delete_unnecessary,
                 ignore)
    generate_graph(dotfile, targets, merge_edges)


def parse_args():
    """Parse the program arguments and return them as a dictionary.

    .. Returns:
    :returns: The program arguments converted to a dictionary.

    """
    formatter = argparse.RawDescriptionHelpFormatter
    parser = argparse.ArgumentParser(description=kdescription,
                                     formatter_class=formatter)
    parser.add_argument('dotfile', default="make.dot", nargs="?",
                        help="The Graphviz dotfile to be created.")
    parser.add_argument('-f', '--makefile', default="", metavar="FILE",
                        help="The makefile to create a graph from. "
                        "Defaults to any file recognized by GNU Make.")
    parser.add_argument('-s', '--source-dir', default=os.getcwd(),
                        metavar="DIR",
                        help="The source directory. Used to strip away "
                        "absolute paths in the dependency graph. Defaults to "
                        "the current working directory.")
    parser.add_argument('-k', '--keep-suffix-rules', action="store_true",
                        help="Keep the old-style suffix rules in the graph.")
    parser.add_argument('-r', '--keep-special-rules', action="store_true",
                        help="Keep the 'special' make targets.")
    parser.add_argument('-d', '--delete-unnecessary', action="store_true",
                        help="Remove all targets that are not required by any "
                        "other target and does not itself require any other "
                        "target.")
    parser.add_argument('-m', '--merge-edges', action="store_true",
                        help="Merge edges to targets with 2 or more "
                        "dependencies to a single edge.")
    parser.add_argument("-i", "--ignore", nargs="+",
                        help="Ignore the list of given targets.")

    return vars(parser.parse_args())


if __name__ == "__main__":
    args = parse_args()
    sys.exit(main(**args))
