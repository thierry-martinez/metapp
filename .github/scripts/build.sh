set -e

# Compatibility with OCaml 5.4 and 5.5
opam pin add https://github.com/ocamllibs/stdcompat.git#refs/pull/75/head

opam install --yes . --deps-only

dune build
dune runtest
