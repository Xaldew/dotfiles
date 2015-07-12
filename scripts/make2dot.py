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
    env = os.environ.copy()
    env["LC_ALL"] = "C"
    cmd = ["make", "--print-data-base", "--print-directory"]
    if makefile:
        cmd.extend(["--file", makefile])
    return sp.check_output(cmd, env=env).split("\n");


def parse_make_database(make_lines, source_dir):
    """Parse the Make database to retrieve the dependency graphs.

    .. Keyword Arguments:
    :param: make_lines: The makefile database.
    :param: source_dir: The base directory where Make was first executed.

    .. Types:
    :type: make_lines: A list of strings.
    :type: source_dir: A string.

    .. Returns:
    :returns: A list of Makefile dependency graphs.
    :rtype: A list of dictionaries of sets.

    """
    idx = 0
    make_dir = ""
    dir_stack = list()
    dags = list()
    regex = re.compile("^%s/?" % source_dir)
    while idx < len(make_lines):
        line = make_lines[idx]
        match = re.match("# make\[\d+\]: Entering directory `(.+)'", line)
        if match:
            dir_stack.append(regex.sub("", match.group(1)))
        elif line.startswith("# Files"):
            start_idx = idx + 1
            while not make_lines[idx].startswith("# Finished Make data base"):
                idx += 1
            subgraph = make_lines[start_idx:idx]
            subgraph = parse_make_subgraph(subgraph, source_dir, make_dir)
            dags.append(subgraph)
        elif re.match("# make\[\d+\]: Leaving directory `.+'", line):
            make_dir = dir_stack.pop()
        idx += 1

    return dags


def parse_make_subgraph(make_lines, source_dir, make_dir, node_id = 0):
    """Parse a subgraph of Make dependencies.

    .. Keyword Arguments:
    :param: make_lines: The lines of the Makefile sub-graph.
    :param: source_dir: The directory where the first Makefile was called.
    :param: make_dir: The directory where the current Makefile was invoked.

    .. Types:
    :type: make_lines: A list of strings.
    :type: source_dir: A string.
    :type: make_dir: A string.

    .. Returns:
    :returns: A dependency graph.
    :rtype: A dictionary with a string as key and set of strings as values.

    """
    targets = dict()
    regex = re.compile("^%s/?" % make_dir)
    re_rem = re.compile("^%s/?" % source_dir)
    # Add directory prefix if not present.
    remover = lambda t: re_rem.sub("", t)
    prefixer = lambda t: os.path.join(make_dir, t) if regex.match(t) else t
    for line in make_lines:
        match = re.match("([^#\t ]+)\s*:\s*(.*)", line)
        if match:
            k, v = match.group(1, 2)
            if re.match("\s*(?:.+)\s*(?:=|:=|::=|\?=)", v):
                continue
            k = prefixer(remover(k))
            if v == "":
                targets[k] = set()
            else:
                deps = re.split(r"\s*\|\s*|\s+", v)
                targets[k] = {prefixer(remover(d)) for d in deps if d}
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


def generate_graph(dotfile, dags, merge_edges=False, ratio=0.75):
    """Generate the Makefile dependency graph in Graphviz dot-format.

    .. Keyword Arguments:
    :param: dotfile: The name for the output dotfile.
    :param: dags: The Makefile dependency graph.
    :param: merge_edges: Should 2 or more edges be merged? (default: False)
    :param: ratio: Ratio between the width and height of the graph.

    .. Types:
    :type: dotfile: A string
    :type: dags: A dictionary of sets.
    :type: merge_edges: A boolean.
    :type: ratio: an integer.

    .. Returns:
    :returns: The name of the saved dotfile.
    :rtype: a string.

    """
    attrs = { "rankdir" : "BT",
              "concentrate" : "true",
              "ratio" : str(ratio),
              "splines" : "true"}
    graph = gv.Digraph(name="Makefile",
                       comment="Generated using make2dot.",
                       format="pdf",
                       engine="dot",
                       graph_attr=attrs)
    for i, dag in enumerate(dags):
        sg = gv.Digraph("cluster_" + str(i), graph_attr=attrs)
        for vertex, edges in dag.items():
            suffix = "_%d" % i
            sg.node(vertex + suffix, label=vertex)
            if merge_edges and len(edges) > 2:
                merge_target_edges(sg, suffix, vertex, edges)
            else:
                for e in edges:
                    graph.edge(e + suffix, vertex + suffix)
        graph.subgraph(sg)

    return graph.save(dotfile)


def merge_target_edges(graph, suffix, vertex, edges):
    """Merge the edges pointing to the given vertex.

    .. Keyword Arguments:
    :param: graph: The graph the vertex and edges belong to.
    :param: suffix: Suffix all vertices and edges with this suffix.
    :param: vertex: The vertex the edges are point to.
    :param: edges: The edges pointing at the vertex.

    .. Types:
    :type: graph: A Graphviz graph.
    :type: suffix: A string.
    :type: vertex: A string with the vertex name.
    :type: edges: A list of strings with the edges.

    """
    virt_v = "_virtual_" + vertex + suffix
    graph.node(virt_v, shape="point", width="0.01", heigh="0.01")
    for e in edges:
        graph.edge(e + suffix, virt_v, dir="none")
    graph.edge(virt_v, vertex + suffix)


def main(makefile,
         dotfile,
         source_dir,
         keep_suffix_rules,
         keep_special_rules,
         delete_unnecessary,
         merge_edges,
         ratio,
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
    dags = parse_make_database(make_lines, source_dir)
    for d in dags:
        filter_graph(d,
                     keep_suffix_rules,
                     keep_special_rules,
                     delete_unnecessary,
                     ignore)
    generate_graph(dotfile, dags, merge_edges, ratio)


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
    parser.add_argument('--keep-suffix-rules', action="store_true",
                        help="Keep the old-style suffix rules in the graph.")
    parser.add_argument('--keep-special-rules', action="store_true",
                        help="Keep the 'special' make targets.")
    parser.add_argument('-d', '--delete-unnecessary', action="store_true",
                        help="Remove all targets that are not required by any "
                        "other target and does not itself require any other "
                        "target.")
    parser.add_argument('-m', '--merge-edges', action="store_true",
                        help="Merge edges to targets with 2 or more "
                        "dependencies to a single edge.")
    parser.add_argument('-r', '--ratio', action="store", type=float,
                        default=0.75, help="Set the width-to-height ratio.")
    parser.add_argument("-i", "--ignore", nargs="+",
                        help="Ignore the list of given targets.")

    return vars(parser.parse_args())


if __name__ == "__main__":
    args = parse_args()
    sys.exit(main(**args))
