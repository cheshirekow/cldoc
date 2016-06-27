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
"""clang based documentation generator"""

from __future__ import absolute_import

import argparse
import json
import logging
import os
import sys
import shutil
import sqlalchemy
import sqlalchemy.orm
import subprocess
import tempfile

from . import nodes
from . import orm
from . import fs, staticsite
from . import tree

def run_generate(tree, opts):
    if opts.type != 'html' and opts.type != 'xml':
        return

    from . import generators

    generator = generators.Xml(tree, opts)

    if opts.type == 'html' and opts.static:
        baseout = fs.fs.mkdtemp()
    else:
        baseout = opts.output

    xmlout = os.path.join(baseout, 'xml')
    generator.generate(xmlout)

    if opts.type == 'html':
        generators.Html(tree).generate(baseout, opts.static, opts.custom_js,
                                    opts.custom_css)

        if opts.static:
            staticsite.generate(baseout, opts)

def process(args):
    """Process files in the compile_commands database."""


def run(args):
    try:
        sep = args.index('--')
    except ValueError:
        if not '--help' in args:
            sys.stderr.write('Please use: cldoc generate ...\n')
            sys.exit(1)
        else:
            sep = -1

    log_level_map = dict(error=logging.ERROR,
                         warning=logging.WARNING,
                         info=logging.INFO,
                         debug=logging.DEBUG)

    parser = argparse.ArgumentParser(description=__doc__)

    parser.add_argument('--loglevel', default='error', metavar='LEVEL',
                        choices=log_level_map.keys(),
                        help='specify the logevel')

    parser.add_argument('--report', action='store_true',
                        help='report documentation coverage and errors')

    parser.add_argument('--output', default=None, metavar='DIR', required=True,
                        help='specify the output directory')

    parser.add_argument('--basedir', default=None, metavar='DIR', required=True,
                        help='the project base directory, only files that are'
                             'subpaths of this directory are processed')

    parser.add_argument('--compile-commands', default=None, required=True,
                        help='Path to compile_commands.json')

    parser.add_argument('--type', default='html', choices=['html', 'xml'],
                        help='Type of doc to generate')

    parser.add_argument('--static', action='store_true',
                        help='Generate static html')

    parser.add_argument('--custom-js', action='append', default=list(),
                        help='Custom javascript added to each file')

    parser.add_argument('--custom-css', action='append', default=list(),
                        help='Custom css added to each file')

    restargs = args[sep + 1:]
    extraflags = args[:sep]

    opts = parser.parse_args(restargs)

    # Set up the main logger
    format_str = '%(levelname)-8s %(filename)s [%(lineno)-3s] : %(message)s'
    logging.basicConfig(level=log_level_map[opts.loglevel],
                        format=format_str,
                        datefmt='%Y-%m-%d %H:%M:%S',
                        filemode='w')

    # Initialize sqlite database
    db_path = os.path.join(opts.output, 'nodes.sqlite')
    sqla_engine = sqlalchemy.create_engine('sqlite:///' + db_path)
    orm.Base.metadata.create_all(sqla_engine)
    sqla_session = sqlalchemy.orm.sessionmaker(bind=sqla_engine)()
    
    with open(opts.compile_commands) as command_db_file:
            compile_commands = json.load(command_db_file)
    num_commands = len(compile_commands)
    longest_basename = 10
    for command_idx, compile_command in enumerate(compile_commands):
        progress = 100.0 * command_idx / num_commands
        file_basename = os.path.basename(compile_command['file'])
        format_str = '[{:6.2f}%] {:' + '{}'.format(longest_basename) + 's}\n'
        longest_basename = max(longest_basename, len(file_basename))
        sys.stdout.write(format_str.format(progress, file_basename))
        sys.stdout.flush()
        doc_tree = tree.Tree2(opts, extraflags)
        doc_tree.process_file(compile_command['file'],
                              compile_command['directory'],
                              compile_command['command'])
        doc_tree.post_process()
        doc_tree.cross_ref()

        # NOTE(josh): Just for now, commit all nodes
        for node in doc_tree.all_nodes:
            sqla_session.add(node)
            sqla_session.commit()
        run_generate(doc_tree, opts)

# vi:ts=4:et
