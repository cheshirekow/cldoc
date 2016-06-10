# This file is part of cldoc.  cldoc is free software: you can
# redistribute it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation, version 2.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
# -*- coding: utf-8 -*-

from clang import cindex
import tempfile

from defdict import Defdict

import glob
import logging
import os
import platform
import json
import re
import sets
import sys

import comment
import nodes
import includepaths
import documentmerger

from . import example
from . import utf8

from ctypes.util import find_library

if platform.system() == 'Darwin':
    libclangs = [
        '/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib',
        '/Library/Developer/CommandLineTools/usr/lib/libclang.dylib'
    ]

    found = False

    for libclang in libclangs:
        if os.path.exists(libclang):
            cindex.Config.set_library_path(os.path.dirname(libclang))
            found = True
            break

    if not found:
        lname = find_library("clang")

        if not lname is None:
            cindex.Config.set_library_file(lname)
else:
    versions = [None, '3.6', '3.5', '3.4', '3.3', '3.2']

    for v in versions:
        name = 'clang'

        if not v is None:
            name += '-' + v

        lname = find_library(name)

        if not lname is None:
            cindex.Config.set_library_file(lname)
            break

testconf = cindex.Config()

try:
    testconf.get_cindex_library()
except cindex.LibclangError as e:
    sys.stderr.write("\nFatal: Failed to locate libclang library. cldoc depends on libclang for parsing sources, please make sure you have libclang installed.\n" + str(e) + "\n\n")
    sys.exit(1)

class ChangeDir(object):
    """Provides 'with' semantics for os.chdir(). Will change to the desired directory and then
       change back when leaving scope. Example:

       os.chdir(os.expanduser('~'))
       print os.getcwd()         # e.g. '/home/josh'
       with ChangeDir('/tmp'):
           print os.getcwd()     # '/tmp'
       print os.getcwd()         # e.g. '/home/josh
    """
    # TODO(josh): investigate https://github.com/jaraco/path.py

    def __init__(self, targetdir):
        self.targetdir = targetdir
        self.old_cwd = ''

    def __enter__(self):
        self.old_cwd = os.getcwd()
        os.chdir(self.targetdir)

    def __exit__(self, exception_type, exception_value, traceback):
        os.chdir(self.old_cwd)
        return exception_value is None


def strip_flag(flags, flag, nargs=0):
    flag_index = flags.index(flag)
    if flag_index != -1:
        flags.pop(flag_index)
        for i in range(nargs):
            flags.pop(flag_index)


def get_flags(command):
    flags = includepaths.flags([]) + command.split()[1:]
    strip_flag(flags, '-c', 1)
    strip_flag(flags, '-o', 1)
    # TODO(josh): remove these hacks
    while '-Werror' in flags:
        flags.remove('-Werror')
    return flags + ['-Wno-mismatched-tags', '-Wno-absolute-value',
                    '-Wno-ignored-qualifiers', '-Wno-unused-function']

class Tree(documentmerger.DocumentMerger):
    def __init__(self, basedir, files, flags, command_db_path=None):
        self.basedir = basedir
        self.processed = {}
        self.files, ok = self.expand_sources([os.path.realpath(f) for f in files])

        if not ok:
            sys.exit(1)

        self.flags = includepaths.flags(flags)
        if command_db_path:
            with open(command_db_path) as command_db_file:
                self.compile_commands = json.load(command_db_file)
        else:
            self.compile_commands = None

        # Sort files on sources, then headers
        self.files.sort(lambda a, b: cmp(self.is_header(a), self.is_header(b)))

        self.processing = {}
        self.kindmap = {}

        # Things to skip
        self.kindmap[cindex.CursorKind.USING_DIRECTIVE] = None

        # Create a map from CursorKind to classes representing those cursor
        # kinds.
        for cls in nodes.Node.subclasses():
            if hasattr(cls, 'kind'):
                self.kindmap[cls.kind] = cls

        self.root = nodes.Root()

        self.all_nodes = []
        self.cursor_to_node = Defdict()
        self.usr_to_node = Defdict()
        self.qid_to_node = Defdict()

        # Map from category name to the nodes.Category for that category
        self.category_to_node = Defdict()

        # Map from filename to comment.CommentsDatabase
        self.commentsdbs = Defdict()

        self.qid_to_node[None] = self.root
        self.usr_to_node[None] = self.root

    def filter_source(self, path):
        return (path.endswith('.c') or path.endswith('.cpp') 
                or path.endswith('.h') or path.endswith('.cc') 
                or path.endswith('.hh') or path.endswith('.hpp'))

    def expand_sources(self, sources, filter=None):
        ret = []
        ok = True

        for source in sources:
            if not filter is None and not filter(source):
                continue

            if os.path.isdir(source):
                source_list = [os.path.join(source, x)
                               for x in os.listdir(source)]
                retdir, okdir = self.expand_sources(source_list,
                                                    self.filter_source)

                if not okdir:
                    ok = False

                ret += retdir
            elif not os.path.exists(source):
                sys.stderr.write("The specified source `" + source 
                                 + "` could not be found\n")
                ok = False
            else:
                ret.append(source)

        return (ret, ok)

    def is_header(self, filename):
        return (filename.endswith('.hh') 
                or filename.endswith('.hpp') 
                or filename.endswith('.h'))

    def find_node_comment(self, node):
        for location in node.comment_locations:
            db = self.commentsdbs[location.file.name]

            if db:
                cm = db.lookup(location)

                if cm:
                    return cm

        return None

    def process_file(self, f, directory=None, command=None):
        """
        process a single file
        """
        if f in self.processed:
            return

        print('Processing {0}'.format(os.path.basename(f)))

        if directory:
            assert command is not None, \
                "If directory is supplied, command must also be supplied"
            with ChangeDir(directory):
                tu = self.index.parse(f, get_flags(command))
        else:
            tu = self.index.parse(f, self.flags)

        if len(tu.diagnostics) != 0:
            fatal = False

            for d in tu.diagnostics:
                sys.stderr.write(d.format)
                sys.stderr.write("\n")

                if d.severity == cindex.Diagnostic.Fatal or \
                   d.severity == cindex.Diagnostic.Error:
                    fatal = True

            if fatal:
                sys.stderr.write("\nCould not generate documentation for "
                                 "%s due to parser errors\n" % (f,))
                sys.exit(1)

        if not tu:
            sys.stderr.write("Could not parse file %s...\n" % (f,))
            sys.exit(1)

        # Extract comments from files and included files that we are
        # supposed to inspect
        extractfiles = [f]

        for inc in tu.get_includes():
            filename = str(inc.include)
            self.headers[filename] = True

            if (filename in self.processed 
                or (not filename in self.files) 
                or filename in extractfiles):
                continue

            extractfiles.append(filename)

        for e in extractfiles:
            db = comment.CommentsDatabase(e, tu)

            self.add_categories(db.category_names)
            self.commentsdbs[e] = db

        self.visit(tu.cursor.get_children())

        for f in self.processing:
            self.processed[f] = True

        self.processing = {}


    def process(self):
        """
        process processes all the files with clang and extracts all relevant
        nodes from the generated AST
        """

        self.headers = {}


        num_files = len(self.files)
        if self.compile_commands:
            num_compile_commands = len(self.compile_commands)
        else:
            num_compile_commands = 0

        self.index = cindex.Index.create()

        total_to_process = num_files + num_compile_commands
        for i, f in enumerate(self.files):
            progress_percent = 1e2 * i / total_to_process
            sys.stdout.write('[{:6.2f}%] '.format(progress_percent))
            self.process_file(f)

        if self.compile_commands:
            for i, entry in enumerate(self.compile_commands):
                progress_percent = 1e2 * (i + num_files) / total_to_process
                sys.stdout.write('[{:6.2f}%] '.format(progress_percent))
                self.process_file(entry['file'], entry['directory'], entry['command'])


        # Construct hierarchy of nodes.
        for node in self.all_nodes:
            q = node.qid

            if node.parent is None:
                par = self.find_parent(node)

                # Lookup categories for things in the root
                if (par is None or par == self.root) and (not node.cursor is None):
                    location = node.cursor.extent.start
                    db = self.commentsdbs[location.file.name]

                    if db:
                        par = self.category_to_node[db.lookup_category(location)]

                if par is None:
                    par = self.root

                par.append(node)

            # Resolve comment
            cm = self.find_node_comment(node)

            if cm:
                node.merge_comment(cm)

        # Keep track of classes to resolve bases and subclasses
        classes = {}

        # Map final qid to node
        for node in self.all_nodes:
            q = node.qid
            self.qid_to_node[q] = node

            if isinstance(node, nodes.Class):
                classes[q] = node

        # Resolve bases and subclasses
        for qid in classes:
            classes[qid].resolve_bases(classes)

    def markup_code(self, index):
        for node in self.all_nodes:
            if node.comment is None:
                continue

            if not node.comment.doc:
                continue

            comps = node.comment.doc.components

            for i in range(len(comps)):
                component = comps[i]

                if not isinstance(component, comment.Comment.Example):
                    continue

                text = str(component)

                tmpfile = tempfile.NamedTemporaryFile(delete=False)
                tmpfile.write(text)
                filename = tmpfile.name
                tmpfile.close()

                tu = index.parse(filename, self.flags, options=1)
                tokens = tu.get_tokens(extent=tu.get_extent(filename, (0, os.stat(filename).st_size)))
                os.unlink(filename)

                hl = []
                incstart = None

                for token in tokens:
                    start = token.extent.start.offset
                    end = token.extent.end.offset

                    if token.kind == cindex.TokenKind.KEYWORD:
                        hl.append((start, end, 'keyword'))
                        continue
                    elif token.kind == cindex.TokenKind.COMMENT:
                        hl.append((start, end, 'comment'))

                    cursor = token.cursor

                    if cursor.kind == cindex.CursorKind.PREPROCESSING_DIRECTIVE:
                        hl.append((start, end, 'preprocessor'))
                    elif cursor.kind == cindex.CursorKind.INCLUSION_DIRECTIVE and incstart is None:
                        incstart = cursor
                    elif (not incstart is None) and \
                         token.kind == cindex.TokenKind.PUNCTUATION and \
                         token.spelling == '>':
                        hl.append((incstart.extent.start.offset, end, 'preprocessor'))
                        incstart = None

                ex = example.Example()
                lastpos = 0

                for ih in range(len(hl)):
                    h = hl[ih]

                    ex.append(text[lastpos:h[0]])
                    ex.append(text[h[0]:h[1]], h[2])

                    lastpos = h[1]

                ex.append(text[lastpos:])
                comps[i] = ex

    def match_ref(self, child, name):
        if isinstance(name, utf8.string):
            return name == child.name
        else:
            return name.match(child.name)

    def find_ref(self, node, name, goup):
        if node is None:
            return []

        ret = []

        for child in node.resolve_nodes:
            if self.match_ref(child, name):
                ret.append(child)

        if goup and len(ret) == 0:
            return self.find_ref(node.parent, name, True)
        else:
            return ret

    def cross_ref_node(self, node):
        if not node.comment is None:
            node.comment.resolve_refs(self.find_ref, node)

        for child in node.children:
            self.cross_ref_node(child)

    def cross_ref(self):
        self.cross_ref_node(self.root)
        self.markup_code(self.index)

    def decl_on_c_struct(self, node, tp):
        n = self.cursor_to_node[tp.decl]

        if isinstance(n, nodes.Struct) or \
           isinstance(n, nodes.Typedef) or \
           isinstance(n, nodes.Enum):
            return n

        return None

    def c_function_is_constructor(self, node):
        hints = ['new', 'init', 'alloc', 'create']

        for hint in hints:
            if node.name.startswith(hint + "_") or \
               node.name.endswith("_" + hint):
                return True

        return False

    def node_on_c_struct(self, node):
        if isinstance(node, nodes.Method) or \
           not isinstance(node, nodes.Function):
            return None

        decl = None

        if self.c_function_is_constructor(node):
            decl = self.decl_on_c_struct(node, node.return_type)

        if not decl:
            args = node.arguments

            if len(args) > 0:
                decl = self.decl_on_c_struct(node, args[0].type)

        return decl

    def find_parent(self, node):
        cursor = node.cursor

        # If node is a C function, then see if we should group it to a struct
        parent = self.node_on_c_struct(node)

        if parent:
            return parent

        while cursor:
            cursor = cursor.semantic_parent
            parent = self.cursor_to_node[cursor]

            if parent:
                return parent

        return self.root

    def register_node(self, node, parent=None):
        self.all_nodes.append(node)

        self.usr_to_node[node.cursor.get_usr()] = node
        self.cursor_to_node[node.cursor] = node

        # Typedefs in clang are not parents of typedefs, but we like it better
        # that way, explicitly set the parent directly here
        if parent and isinstance(parent, nodes.Typedef):
            parent.append(node)

        if parent and hasattr(parent, 'current_access'):
            node.access = parent.current_access

    def register_anon_typedef(self, node, parent):
        node.typedef = parent
        node.add_comment_location(parent.cursor.extent.start)

        self.all_nodes.remove(parent)

        # Map references to the typedef directly to the node
        self.usr_to_node[parent.cursor.get_usr()] = node
        self.cursor_to_node[parent.cursor] = node

    def cursor_is_exposed(self, cursor):
        # Only cursors which are in headers are exposed.
        filename = str(cursor.location.file)
        return filename in self.headers or self.is_header(filename)

    def is_unique_anon_struct(self, node, parent):
        if not node:
            return False

        if not isinstance(node, nodes.Struct):
            return False

        if not (node.is_anonymous or not node.name):
            return False

        return not isinstance(parent, nodes.Typedef)

    def visit(self, citer, parent=None):
        """
        visit iterates over the provided cursor iterator and creates nodes
        from the AST cursors.
        """
        if not citer:
            return

        while True:
            try:
                item = citer.next()
            except StopIteration:
                return

            # Check the source of item
            if not item.location.file:
                self.visit(item.get_children())
                continue

            # Ignore files we already processed
            if str(item.location.file) in self.processed:
                continue

            # Ignore files other than the ones we are scanning for
            if not str(item.location.file) in self.files:
                # TODO(josh): re-enable this check after figuring out how to
                # match files from compilation database
                pass
                # continue

            realpath_to_file = os.path.realpath(str(item.location.file))
            realpath_to_base = os.path.realpath(self.basedir)
            relpath_from_basedir = os.path.relpath(realpath_to_file,
                                                   realpath_to_base)

            # Only include files that are a subpath of basedir
            # NOTE(josh): item.location.file might be relative to the cwd
            # of the command
            if not len(relpath_from_basedir) < len(realpath_to_file):
                continue

            # Ignore unexposed things
            if item.kind == cindex.CursorKind.UNEXPOSED_DECL:
                self.visit(item.get_children(), parent)
                continue

            self.processing[str(item.location.file)] = True

            if item.kind in self.kindmap:
                cls = self.kindmap[item.kind]

                if not cls:
                    # Skip
                    continue

                # see if we already have a node for this thing
                node = self.usr_to_node[item.get_usr()]

                if not node or self.is_unique_anon_struct(node, parent):
                    # Only register new nodes if they are exposed.
                    if self.cursor_is_exposed(item):
                        node = cls(item, None)
                        self.register_node(node, parent)

                elif isinstance(parent, nodes.Typedef) and isinstance(node, nodes.Struct):
                    # Typedefs are handled a bit specially because what happens
                    # is that clang first exposes an unnamed struct/enum, and
                    # then exposes the typedef, with as a child again the
                    # cursor to the already defined struct/enum. This is a
                    # bit reversed as to how we normally process things.
                    self.register_anon_typedef(node, parent)
                else:
                    self.cursor_to_node[item] = node
                    node.add_ref(item)

                if node and node.process_children:
                    self.visit(item.get_children(), node)
            else:
                par = self.cursor_to_node[item.semantic_parent]

                if not par:
                    par = parent

                if par:
                    ret = par.visit(item, citer)

                    if not ret is None:
                        for node in ret:
                            self.register_node(node, par)

                ignoretop = [cindex.CursorKind.TYPE_REF, cindex.CursorKind.PARM_DECL]

                if (not par or ret is None) and not item.kind in ignoretop:
                    logging.warning("Unhandled cursor: %s", item.kind)

class Tree2(documentmerger.DocumentMerger):
    """A tree of documentation nodes generated from a single translation
       unit, built by clang from a compile command in the database."""
    def __init__(self, args, flags):
        self.output_dir = args.output
        self.base_dir = args.basedir
        
        # Add standard include paths to flags
        self.flags = includepaths.flags(flags)
        
        self.processed = {}     # Maps file names to whether or not they've 
                                # been processed
        self.processing = {}    # Maps file names to whether or not we're
                                # currently processing them
        
        # Maps CursorKind id's to CursorKind python objects representing
        # those curosor kinds
        self.kindmap = {}

        # Things to skip
        self.kindmap[cindex.CursorKind.USING_DIRECTIVE] = None

        # Create a map from CursorKind to classes representing those cursor
        # kinds.
        for cls in nodes.Node.subclasses():
            if hasattr(cls, 'kind'):
                self.kindmap[cls.kind] = cls

        self.root = nodes.Root()

        self.all_nodes = []
        self.cursor_to_node = Defdict()

        self.usr_to_node = Defdict()    # Map clang unique identifier to node
        self.qid_to_node = Defdict()    # Maps qualified id to nodes

        # Map from category name to the nodes.Category for that category
        self.category_to_node = Defdict()

        # Map from filename to comment.CommentsDatabase
        self.commentsdbs = Defdict()

        self.qid_to_node[None] = self.root
        self.usr_to_node[None] = self.root

        # Clang index
        self.index = cindex.Index.create()

    def find_node_comment(self, node):
        for location in node.comment_locations:
            db = self.commentsdbs[location.file.name]

            if db:
                cm = db.lookup(location)

                if cm:
                    return cm

        return None

    def file_is_in_basedir(self, filepath):
        realpath_to_file = os.path.realpath(filepath)
        realpath_to_base = os.path.realpath(self.base_dir)
        relpath_from_basedir = os.path.relpath(realpath_to_file,
                                               realpath_to_base)

        # Only include files that are a subpath of basedir
        # NOTE(josh): item.location.file might be relative to the cwd
        # of the command
        return len(relpath_from_basedir) < len(realpath_to_file)

    def process_file(self, f, directory, command):
        """
        process a single file
        """
        # If the file has already been processed then skip
        self.headers = {}

        if directory:
            assert command is not None, \
                "If directory is supplied, command must also be supplied"
            with ChangeDir(directory):
                tu = self.index.parse(f, get_flags(command))
        else:
            tu = self.index.parse(f, self.flags)

        if len(tu.diagnostics) != 0:
            fatal = False

            for d in tu.diagnostics:
                sys.stderr.write(d.format)
                sys.stderr.write("\n")

                if d.severity == cindex.Diagnostic.Fatal or \
                   d.severity == cindex.Diagnostic.Error:
                    fatal = True

            if fatal:
                sys.stderr.write("\nCould not generate documentation for "
                                 "%s due to parser errors\n" % (f,))
                sys.exit(1)

        if not tu:
            sys.stderr.write("Could not parse file %s...\n" % (f,))
            sys.exit(1)

        # Extract comments from files and included files that we are
        # supposed to inspect
        extractfiles = [f]

        for inc in tu.get_includes():
            filename = str(inc.include)

            if (filename in self.processed 
                or not self.file_is_in_basedir(filename)
                or filename in extractfiles):
                continue

            self.headers[filename] = True
            extractfiles.append(filename)

        for e in extractfiles:
            db = comment.CommentsDatabase(e, tu)

            self.add_categories(db.category_names)
            self.commentsdbs[e] = db

        self.visit(tu.cursor.get_children())

        for f in self.processing:
            self.processed[f] = True

        self.processing = {}


    def post_process(self):
        """
        post_process nodes
        """

        # Construct hierarchy of nodes.
        for node in self.all_nodes:
            q = node.qid

            if node.parent is None:
                par = self.find_parent(node)

                # Lookup categories for things in the root
                if (par is None or par == self.root) and (not node.cursor is None):
                    location = node.cursor.extent.start
                    db = self.commentsdbs[location.file.name]

                    if db:
                        par = self.category_to_node[db.lookup_category(location)]

                if par is None:
                    par = self.root

                par.append(node)

            # Resolve comment
            cm = self.find_node_comment(node)

            if cm:
                node.merge_comment(cm)

        # Keep track of classes to resolve bases and subclasses
        classes = {}

        # Map final qid to node
        for node in self.all_nodes:
            q = node.qid
            self.qid_to_node[q] = node

            if isinstance(node, nodes.Class):
                classes[q] = node

        # Resolve bases and subclasses
        for qid in classes:
            classes[qid].resolve_bases(classes)

    def markup_code(self, index):
        for node in self.all_nodes:
            if node.comment is None:
                continue

            if not node.comment.doc:
                continue

            comps = node.comment.doc.components

            for i in range(len(comps)):
                component = comps[i]

                if not isinstance(component, comment.Comment.Example):
                    continue

                text = str(component)

                tmpfile = tempfile.NamedTemporaryFile(delete=False)
                tmpfile.write(text)
                filename = tmpfile.name
                tmpfile.close()

                tu = index.parse(filename, self.flags, options=1)
                tokens = tu.get_tokens(extent=tu.get_extent(filename, (0, os.stat(filename).st_size)))
                os.unlink(filename)

                hl = []
                incstart = None

                for token in tokens:
                    start = token.extent.start.offset
                    end = token.extent.end.offset

                    if token.kind == cindex.TokenKind.KEYWORD:
                        hl.append((start, end, 'keyword'))
                        continue
                    elif token.kind == cindex.TokenKind.COMMENT:
                        hl.append((start, end, 'comment'))

                    cursor = token.cursor

                    if cursor.kind == cindex.CursorKind.PREPROCESSING_DIRECTIVE:
                        hl.append((start, end, 'preprocessor'))
                    elif cursor.kind == cindex.CursorKind.INCLUSION_DIRECTIVE and incstart is None:
                        incstart = cursor
                    elif (not incstart is None) and \
                         token.kind == cindex.TokenKind.PUNCTUATION and \
                         token.spelling == '>':
                        hl.append((incstart.extent.start.offset, end, 'preprocessor'))
                        incstart = None

                ex = example.Example()
                lastpos = 0

                for ih in range(len(hl)):
                    h = hl[ih]

                    ex.append(text[lastpos:h[0]])
                    ex.append(text[h[0]:h[1]], h[2])

                    lastpos = h[1]

                ex.append(text[lastpos:])
                comps[i] = ex

    def match_ref(self, child, name):
        if isinstance(name, utf8.string):
            return name == child.name
        else:
            return name.match(child.name)

    def find_ref(self, node, name, goup):
        if node is None:
            return []

        ret = []

        for child in node.resolve_nodes:
            if self.match_ref(child, name):
                ret.append(child)

        if goup and len(ret) == 0:
            return self.find_ref(node.parent, name, True)
        else:
            return ret

    def cross_ref_node(self, node):
        if not node.comment is None:
            node.comment.resolve_refs(self.find_ref, node)

        for child in node.children:
            self.cross_ref_node(child)

    def cross_ref(self):
        self.cross_ref_node(self.root)
        self.markup_code(self.index)

    def decl_on_c_struct(self, node, tp):
        n = self.cursor_to_node[tp.decl]

        if isinstance(n, nodes.Struct) or \
           isinstance(n, nodes.Typedef) or \
           isinstance(n, nodes.Enum):
            return n

        return None

    def c_function_is_constructor(self, node):
        hints = ['new', 'init', 'alloc', 'create']

        for hint in hints:
            if node.name.startswith(hint + "_") or \
               node.name.endswith("_" + hint):
                return True

        return False

    def node_on_c_struct(self, node):
        if isinstance(node, nodes.Method) or \
           not isinstance(node, nodes.Function):
            return None

        decl = None

        if self.c_function_is_constructor(node):
            decl = self.decl_on_c_struct(node, node.return_type)

        if not decl:
            args = node.arguments

            if len(args) > 0:
                decl = self.decl_on_c_struct(node, args[0].type)

        return decl

    def find_parent(self, node):
        cursor = node.cursor

        # If node is a C function, then see if we should group it to a struct
        parent = self.node_on_c_struct(node)

        if parent:
            return parent

        while cursor:
            cursor = cursor.semantic_parent
            parent = self.cursor_to_node[cursor]

            if parent:
                return parent

        return self.root

    def register_node(self, node, parent=None):
        self.all_nodes.append(node)

        self.usr_to_node[node.cursor.get_usr()] = node
        self.cursor_to_node[node.cursor] = node

        # Typedefs in clang are not parents of typedefs, but we like it better
        # that way, explicitly set the parent directly here
        if parent and isinstance(parent, nodes.Typedef):
            parent.append(node)

        if parent and hasattr(parent, 'current_access'):
            node.access = parent.current_access

    def register_anon_typedef(self, node, parent):
        node.typedef = parent
        node.add_comment_location(parent.cursor.extent.start)

        self.all_nodes.remove(parent)

        # Map references to the typedef directly to the node
        self.usr_to_node[parent.cursor.get_usr()] = node
        self.cursor_to_node[parent.cursor] = node

    def cursor_is_exposed(self, cursor):
        # Only cursors which are in headers are exposed.
        filename = str(cursor.location.file)
        return filename in self.headers

    def is_unique_anon_struct(self, node, parent):
        if not node:
            return False

        if not isinstance(node, nodes.Struct):
            return False

        if not (node.is_anonymous or not node.name):
            return False

        return not isinstance(parent, nodes.Typedef)

    def visit(self, citer, parent=None):
        """
        visit iterates over the provided cursor iterator and creates nodes
        from the AST cursors.
        """
        if not citer:
            return

        while True:
            try:
                item = citer.next()
            except StopIteration:
                return

            # Check the source of item
            if not item.location.file:
                self.visit(item.get_children())
                continue

            # Ignore files we already processed
            if str(item.location.file) in self.processed:
                continue

            if not self.file_is_in_basedir(str(item.location.file)):
                continue
            
            # Ignore unexposed things
            if item.kind == cindex.CursorKind.UNEXPOSED_DECL:
                self.visit(item.get_children(), parent)
                continue

            self.processing[str(item.location.file)] = True

            if item.kind in self.kindmap:
                cls = self.kindmap[item.kind]

                if not cls:
                    # Skip
                    continue

                # see if we already have a node for this thing
                node = self.usr_to_node[item.get_usr()]

                if not node or self.is_unique_anon_struct(node, parent):
                    # Only register new nodes if they are exposed.
                    if self.cursor_is_exposed(item):
                        node = cls(item, None)
                        self.register_node(node, parent)

                elif isinstance(parent, nodes.Typedef) and isinstance(node, nodes.Struct):
                    # Typedefs are handled a bit specially because what happens
                    # is that clang first exposes an unnamed struct/enum, and
                    # then exposes the typedef, with as a child again the
                    # cursor to the already defined struct/enum. This is a
                    # bit reversed as to how we normally process things.
                    self.register_anon_typedef(node, parent)
                else:
                    self.cursor_to_node[item] = node
                    node.add_ref(item)

                if node and node.process_children:
                    self.visit(item.get_children(), node)
            else:
                par = self.cursor_to_node[item.semantic_parent]

                if not par:
                    par = parent

                if par:
                    ret = par.visit(item, citer)

                    if not ret is None:
                        for node in ret:
                            self.register_node(node, par)

                ignoretop = [cindex.CursorKind.TYPE_REF, cindex.CursorKind.PARM_DECL]

                if (not par or ret is None) and not item.kind in ignoretop:
                    logging.warning("Unhandled cursor: %s", item.kind)

# vi:ts=4:et
