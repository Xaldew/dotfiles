#!/usr/bin/env python3

"""Use libclang to generate a mostly correct C++ output stream operator."""

# https://libclang.readthedocs.io/en/latest/

import argparse
import logging
import sys
import os

import clang
import clang.cindex


def is_bitflag_enum(n):
    """Check if a node is a bitflag enumeration type.

    .. Keyword Arguments:
    :param n: The enum node to check

    .. Types:
    :type n: A libclang cursor type.

    """
    def is_pow2(v):
        """Check if the number is a power of two.

        Note that 0 is considered a power of two in this case.

        """
        return v & (v - 1) == 0
    for ch in n.get_children():
        if ch.kind == clang.cindex.CursorKind.ENUM_CONSTANT_DECL:
            if not is_pow2(ch.enum_value):
                return False
    return True


def gen_bitflag_enum(n):
    """Generate a print operator for a bitflag enumeration type.

    .. Keyword Arguments:
    :param n: The enum node to generate an output operator for.

    .. Types:
    :type n: A libclang cursor type.

    """
    return """
std::ostream& operator<<(std::ostream &os, {type} bf)
{{
    bool is_first = true;
    std::stringstream ss;
    using UnderlyingT = typename std::make_unsigned_t<typename std::underlying_type_t<{type}>>;
    UnderlyingT t = static_cast<{type}>(i);
    for (size_t i = 0; i < std::numeric_limits<UnderlyingT>::digits; i++)
    {{
        bool is_set = t & (static_cast<UnderlyingT>(1) << i);
        if (is_set && !is_first)
            ss << " | ";
        if (is_set)
        {{
            is_first = false;
            ss << t;
        }}
    }}
    return os << ss.str();
}}
    """.format(type=n.spelling)


def gen_scoped_enum(n):
    """Generate a print operator for a scoped enumeration node type.

    .. Keyword Arguments:
    :param n: The enum node to generate an output operator for.

    .. Types:
    :type n: A libclang cursor type.

    """
    out = []
    i = "    "
    out.append(f"std::ostream& operator<<(std::ostream &os, {n.spelling} v)")
    out.append("{")
    out.append(i + "switch(v)")
    out.append(i + "{")
    for ch in n.get_children():
        out.append('{i}case {type}::{value}: os << "{value}"; break;'
                   ''.format(i=i, type=n.type.spelling, value=ch.spelling))
    out.append(i + "}")
    out.append(i + "return os;")
    out.append("}")
    return "\n".join(out)


def gen_enum(n):
    """Generate a print operator for a enumeration node type.

    .. Keyword Arguments:
    :param n: The enum node to generate an output operator for.

    .. Types:
    :type n: A libclang cursor type.

    """
    out = []
    i = "    "
    out.append(f"std::ostream& operator<<(std::ostream &os, {n.spelling} v)")
    out.append("{")
    out.append(i + "switch(v)")
    out.append(i + "{")
    for ch in n.get_children():
        out.append('{i}case {value}: os << "{value}"; break;'
                   ''.format(i=i, value=ch.spelling))
    out.append(i + "}")
    out.append(i + "return os;")
    out.append("}")
    return "\n".join(out)


def gen_struct(n):
    """Generate an output operator for a C/C++ struct.

    .. Keyword Arguments:
    :param n: The struct node to generate an output operator for

    .. Types:
    :type n: A libclang cursor type.

    """
    out = []
    i = "    "
    out.append(f"std::ostream& operator<<(std::ostream &os, const {n.spelling} &v)")
    out.append("{")
    out.append(i + f'os << "{n.spelling}{{";')
    fields = []
    for m in n.get_children():
        if m.kind == clang.cindex.CursorKind.FIELD_DECL:
            fields.append(m)
    for j, m in enumerate(fields):
        if j != len(fields) - 1:
            out.append(i + f'os << "{m.spelling}=" << v.{m.spelling} << ", ";')
        else:
            out.append(i + f'os << "{m.spelling}=" << v.{m.spelling};')
    out.append(i + 'os << "}";')
    out.append(i + "return os;")
    out.append("}")
    return "\n".join(out)


def removeprefix(rstr, prefix):
    """Remove a prefix from a string."""
    if rstr.startswith(prefix):
        return rstr[len(prefix):]
    else:
        return rstr


def main(args):
    """Generate an output-stream operator for the given class.

    .. Keyword Arguments:
    :param args: An argparse namespace object.

    .. Returns:
    :returns: 0 if successful, otherwise an error code.

    """
    fname = removeprefix(args.file, "file://")
    cdb_dir = args.compilation_database
    cmd = ""
    wd = ""
    path = ""
    try:
        cdb = clang.cindex.CompilationDatabase.fromDirectory(cdb_dir)
        cmds = cdb.getCompileCommands(fname)
        for c in cmds:
            if fname in c.filename:
                cmd = [str(a) for a in c.arguments]
                wd = c.directory
                path = c.filename
                break
    except clang.cindex.CompilationDatabaseError:
        cmd = []
        wd = os.getcwd()
        path = fname
        logging.warning("No known compile commands or working directory.")

    # os.chdir(wd)

    # Need to remove first 2 and last 2 strings on the command line, Libclang
    # does not like the --driver-mode=g++, '-- filename' syntax.
    logging.info("Using compile commands: %s", " ".join(cmd))
    logging.info("Using workding directory: %s", wd)
    logging.info("Using file path: %s", path)

    # TODO: Speed-up using AST file?
    index = clang.cindex.Index.create()
    tu = index.parse(path, args=cmd[2:-2])

    for n in tu.cursor.walk_preorder():
        if args.token != n.displayname:
            continue
        # TODO: Run clang-format on region?
        if n.kind == clang.cindex.CursorKind.ENUM_DECL:
            if is_bitflag_enum(n):
                print(gen_bitflag_enum(n))
            elif n.is_scoped_enum():
                print(gen_scoped_enum(n))
            else:
                print(gen_enum(n))
        elif n.kind == clang.cindex.CursorKind.STRUCT_DECL:
            print(gen_struct(n))
        else:
            # TODO: Unknown cursor, generate error?
            pass

    return 0


def find_dominating_directory(start_dir, name):
    """Find the dominating directory which contains the file 'name'.

    .. Keyword Arguments:
    :param start_dir: The directory from where to start the search.
    :param name: The file name to search for.

    .. Types:
    :type start_dir: A string.
    :type name: A string.

    .. Returns:
    :returns: The path to the dominating directory or the
              root directory if not found.
    :rtype: A string.

    """
    cdir = start_dir
    pdir = ""
    while cdir != pdir:
        for fil in os.listdir(cdir):
            if name == fil:
                return cdir
        pdir = cdir
        cdir = os.path.realpath(os.path.join(cdir, ".."))
    return cdir


def find_compilation_database_dir(cwd):
    """Search through all dominating directories for the compilation database.

    .. Keyword Arguments:
    :param cwd: The current working directory.

    .. Returns:
    :returns: The directory containing the compilation database, or the current
    working directory, if not found.

    """
    name = "compile_commands.json"
    cdb_dir = find_dominating_directory(cwd, name)
    if cdb_dir == "/" and not os.path.exists(os.path.join(cdb_dir, name)):
        return cwd
    else:
        return cdb_dir


def parse_arguments(argv):
    """Parse the given argument vector.

    .. Keyword Arguments:
    :param argv: The arguments to be parsed.

    .. Types:
    :type argv: A list of strings.

    .. Returns:
    :returns: The parsed arguments.
    :rtype: A argparse namespace object.

    """
    fmtr = argparse.RawDescriptionHelpFormatter
    kdesc = "C++ output stream operator generator."
    parser = argparse.ArgumentParser(description=kdesc, formatter_class=fmtr)
    parser.add_argument("token",
                        help="The token to generate an output operator for.")
    parser.add_argument("file",
                        help="The file that the contains the token.")
    parser.add_argument("-c", "--compilation-database",
                        default=find_compilation_database_dir(os.getcwd()),
                        help="Directory containing the compilation database.")
    parser.add_argument("-v", "--verbosity", metavar="N", type=int,
                        default=logging.WARNING,
                        choices=range(logging.NOTSET, logging.CRITICAL),
                        help="Set logging verbosity level.")
    return parser.parse_args(argv)


if __name__ == '__main__':
    ARGS = parse_arguments(sys.argv[1:])
    logging.basicConfig(level=ARGS.verbosity)
    sys.exit(main(ARGS))
