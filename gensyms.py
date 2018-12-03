import configparser
from os import path
import sys


HEADER_FILE = """\
// WARNING!
// This file is generated. Do not edit by hand.
// Instead, edit ../symbols.cfg and run ../gensyms.py

#include "emacs-module.h"

#ifndef SYMBOLS_H
#define SYMBOLS_H

void esyms_init(emacs_env *env);

{decls}

#endif /* SYMBOLS_H */
"""


IMPL_FILE = """\
// WARNING!
// This file is generated. Do not edit by hand.
// Instead, edit ../symbols.cfg and run ../gensyms.py

#include "symbols.h"

{decls}

void esyms_init(emacs_env *env)
{{
{init}
}}
"""


def join_indent(lines, levels=0, sep='\n'):
    indent = ' ' * 4 * levels
    return sep.join(indent + line for line in lines)


def sym_to_c(sym):
    return 'esym_{}'.format(sym.replace('-', '_'))


def unique_syms(spec):
    syms = {sym for section in spec.values() for sym in section}
    return sorted(syms)


def gen_header(spec):
    declarations = []

    for sym in unique_syms(spec):
        declarations.append('extern emacs_value {};'.format(sym_to_c(sym)))

    declarations = '\n'.join(declarations)
    return HEADER_FILE.format(decls=declarations)


def gen_impl(spec):
    declarations = []
    inits = []

    for sym in unique_syms(spec):
        declarations.append('emacs_value {};'.format(sym_to_c(sym)))
        inits.append('{} = env->make_global_ref(env, env->intern(env, "{}"));'.format(sym_to_c(sym), sym))

    declarations = join_indent(declarations)
    inits = join_indent(inits, levels=1)
    return IMPL_FILE.format(
        decls=declarations,
        init=inits,
    )

def gensyms(spec):
    header = gen_header(spec)
    impl = gen_impl(spec)
    return header, impl

if __name__ == '__main__':
    spec = configparser.ConfigParser(allow_no_value=True, strict=False)

    rootdir = path.dirname(path.abspath(__file__))
    infile = path.join(rootdir, 'symbols.cfg')
    headerfile = path.join(rootdir, 'src', 'symbols.h')
    implfile = path.join(rootdir, 'src', 'symbols.c')

    spec.read(infile)
    header, impl = gensyms(spec)

    with open(headerfile, 'w') as f:
        f.write(header)

    with open(implfile, 'w') as f:
        f.write(impl)
