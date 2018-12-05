import configparser
from os import path
import sys


HEADER_FILE = """\
// WARNING!
// This file is generated. Do not edit by hand.
// Instead, edit ../symbols.cfg and run ../gensyms.py

#include "emacs-module.h"
#include "git2.h"

#ifndef SYMBOLS_H
#define SYMBOLS_H

typedef union {{
{union}
}} esym_enumval;

typedef struct {{
    emacs_value *symbol;
    esym_enumval value;
}} esym_map;

{decls}

void esyms_init(emacs_env *env);

#endif /* SYMBOLS_H */
"""


IMPL_FILE = """\
// WARNING!
// This file is generated. Do not edit by hand.
// Instead, edit ../symbols.cfg and run ../gensyms.py

#include "symbols.h"
#include "git2.h"

{decls}

void esyms_init(emacs_env *env)
{{
{init}
}}
"""


RESERVED_SECS = {'DEFAULT', 'unmapped'}


def join_indent(lines, levels=0, sep='\n'):
    indent = ' ' * 4 * levels
    return sep.join(indent + line for line in lines)


def sym_to_c(sym):
    return 'esym_{}'.format(sym.replace('-', '_'))


def unique_syms(spec):
    if isinstance(spec, configparser.ConfigParser):
        syms = {
            sym for section in spec.values() for sym in section
            if not sym.startswith('__')
        }
    else:
        syms = {
            sym for sym in spec
            if not sym.startswith('__')
        }

    return sorted(syms)


def all_syms(section):
    return list((sym, val) for sym, val in section.items() if not sym.startswith('__'))


def mapname(name, raw=False):
    if name.startswith('git_'):
        name = name[4:]
    if name.endswith('_t'):
        name = name[:-2]
    if raw:
        return name
    return 'esym_{}_map'.format(name)


def gen_header(spec):
    declarations = []
    union = []

    for secname, section in spec.items():
        if secname in RESERVED_SECS:
            continue
        typename = section.get('__type', secname)
        union.append('{} {};'.format(typename, mapname(secname, raw=True)))
        declarations.append('extern esym_map {}[{}];'.format(
            mapname(secname), len(unique_syms(section))+1))
    for sym in unique_syms(spec):
        declarations.append('extern emacs_value {};'.format(sym_to_c(sym)))

    union = join_indent(union, levels=1)
    declarations = join_indent(declarations)
    return HEADER_FILE.format(decls=declarations, union=union)


def gen_impl(spec):
    declarations = []
    inits = []

    for sym in unique_syms(spec):
        declarations.append('emacs_value {};'.format(sym_to_c(sym)))
        inits.append('{} = env->make_global_ref(env, env->intern(env, "{}"));'.format(sym_to_c(sym), sym))

    for secname, section in spec.items():
        if secname in RESERVED_SECS:
            continue
        mname = mapname(secname)
        syms = all_syms(section)
        prefix = section.get('__prefix', '')

        map_inits = []
        for sym, val in syms:
            if val is None:
                val = sym.upper().replace('-', '_')
            cname = prefix + val
            map_inits.append('{{&{}, {{.{} = {}}}}}'.format(sym_to_c(sym), mapname(secname, raw=True), cname))
        map_inits.append('{NULL, {0}}')
        map_inits = join_indent(map_inits, levels=1, sep=',\n')
        declarations.append('esym_map {}[{}] = {{\n'.format(
            mname, len(syms)+1) + map_inits + '\n};')

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
