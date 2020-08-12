(lang dune 1.11)

(generate_opam_files true)

(name metapp)
(license "BSD-2-Clause")
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://github.com/thierry-martinez/metapp.git"))
(homepage "https://github.com/thierry-martinez/metapp")
(bug_reports "https://github.com/thierry-martinez/metapp")
(documentation "https://github.com/thierry-martinez/metapp")
(version "0.2.0")

(package
 (name metapp)
 (synopsis "Meta-preprocessor for OCaml")
 (description "\
Meta-preprocessor for OCaml: extends the language with [%meta ... ]
construction where ... stands for OCaml code evaluated at
compile-time.
")
 (depends
  (ocaml (and (>= 4.03.0) (< 4.12.0)))
  (stdcompat (>= 12))
  (ocaml-migrate-parsetree (and (>= 1.7.3) (< 2.0.0)))
  (ocamlfind (>= 1.8.1))
  (dune (>= 1.11.0))
  (odoc (and :with-doc (>= 1.5.1)))))
