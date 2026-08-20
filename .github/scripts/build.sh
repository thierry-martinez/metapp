set -e

opam pin add https://github.com/ocamllibs/stdcompat.git

opam install --yes . --deps-only

dune build
dune runtest
