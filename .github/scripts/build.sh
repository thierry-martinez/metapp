set -e

opam pin add https://github.com/thierry-martinez/stdcompat.git#compatibility_with_OCaml_5.4_and_5.5

opam install --yes . --deps-only

dune build
dune runtest
