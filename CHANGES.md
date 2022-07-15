# Version 0.4.4, 2022-07-15

- Port to ppxlib 0.26.0
  (suggested by @nilsbecker, https://github.com/thierry-martinez/ocaml-in-python/issues/1)

- Add `Pat.Construct.Arg.{construct,destruct}` for constructing and
  destructing `Ppat_construct` argument (for compatibility between OCaml 4.14
  and older versions of OCaml).

- Add `Te.destruct_decl` for destructing `Pext_decl` (for compatibility between
  OCaml 4.14 and older versions of OCaml).

# Version 0.4.3, 2022-03-21

- Add getters `Metapp.Types.get_{desc,level,scope,id}` since `Types.type_expr`
  is abstract in OCaml 4.14.

# Version 0.4.2, 2021-10-08

- Add `Metapp.Types.destruct_tpackage`: returns a pair `Some (path, list)`
  if a `Types.type_desc` is a `Tpackage`, for compatibility between OCaml 4.13
  and older versions of OCaml.

# Version 0.4.1, 2021-10-04

- Add `Metapp.Types.destruct_type_variant`: returns a pair `Some (ctors, repr)`
  if a `Types.type_kind` is a `Type_variant`, for compatibility between OCaml 4.13
  and older versions of OCaml.

# Version 0.4.0, 2021-02-19

- Port to ppxlib 0.18 and OCaml 4.12 (by kit-ty-kate, #2)

# Version 0.3.0, 2020-09-22

- Port to ppxlib 0.16 / ocaml-migrate-parsetree 2.0.0

- `Metapp.ExtensibleS.destruct_extension` returns a pair `Some (e, attrs)` where
  `e` is the extension and `attrs` is the list of optional attributes.

- Fix bug with nested calls in meta-quotes

- Add `?optional` argument to `Metapp.apply`

- Add `[%%metaverbose]` for logging compilation commands.

# Version 0.2.0, 2020-05-11

- Compatibility with OCaml 4.11.0

- More utility functions in `Metapp` module to construct and destruct OCaml's
  parsetree in a version-independent way

- Functions for invoking OCaml compiler and loading modules dynamically are
  exported in the package `metapp.dyncompile`.

# Version 0.1.0, 2020-02-27

- First release
