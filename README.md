# `metapp`: meta-preprocessor for OCaml

`metapp` is a PPX rewriter that provides a `[%meta ...]` extension,
where the dots `...` are arbitrary OCaml expressions that are
substituted at compile-time by the AST nodes they evaluate into.
These expressions build AST nodes either by quoting some code directly,
or by using `compiler-libs` ([`Parsetree`], [`Ast_helper`], ...).

[`Parsetree`]: https://caml.inria.fr/pub/docs/manual-ocaml/compilerlibref/Parsetree.html
[`Ast_helper`]: https://caml.inria.fr/pub/docs/manual-ocaml/compilerlibref/Ast_helper.html

In particular, this preprocessor is easy to use for conditional
compilation, and is an alternative to [`cppo`] and [`ppx_optcomp`].

[`cppo`]: https://github.com/ocaml-community/cppo
[`ppx_optcomp`]: https://github.com/janestreet/ppx_optcomp

```ocaml
let option_get o =
  [%meta if Sys.ocaml_version >= "4.08.0" then
     [%e Option.get o]
  else
     [%e match o with
     | None -> invalid_arg "option_get"
     | Some x -> x]]
```

`metapp` can be used with [`dune`] by using the [`preprocess`] field.

[`dune`]: https://github.com/ocaml/dune
[`preprocess`]: https://dune.readthedocs.io/en/latest/concepts.html#preprocessing-with-ppx-rewriters

```lisp
(executable
  ...
  (preprocess (pps metapp.ppx))
  ...)
```

Inside `[%meta ...]` code, the `[%e ...]` extension quotes expressions
(of type [`Parsetree.expression`]). There are other quotations
available: the full list is given below.

|Quotation                    |Type                      |
|-----------------------------|--------------------------|
|`[%e ...]` or `[%expr ...]`  |`Parsetree.expression`    |
|`[%p? ...]` or `[%pat? ...]` |`Parsetree.pattern`       |
|`[%t: ...]` or `[%type: ...]`|`Parsetree.core_type`     |
|`[%sig: ...]`                |`Parsetree.signature`     |
|`[%sigi: ...]`               |`Parsetree.signature_item`|
|`[%str ...]`                 |`Parsetree.structure`     |
|`[%stri ...]`                |`Parsetree.structure_item`|

Quoted expressions can in turn contain further `[%meta ...]` code.
Moreover, `[%meta ...]` code can itself contain other levels of
`[%meta ...]` code, for multi-stage programming.

In addition to this syntax extension, the [`Metapp`] module
provided by the [`metapp`] package provides convenient functions
for AST constructions.  In particular, this module provides an
OCaml-version-independent interface.  Moveover, this module provides a
common signature `ValueS` for constructing and transforming
expressions (module `Exp`), patterns (module `Pat`) or both at the
same time (module `Value`).

[`Metapp`]: https://github.com/thierry-martinez/metapp/blob/master/metapp/metapp.mli

The [`Metapp`] module also provides a `filter` mapper, which handles
`[@if <bool>]` attributes _à la_ `ppx_optcomp`. The `[@if <bool>]`
attribute can appear mostly everywhere syntax elements are enumerated,
including tuples, function applications, arrays, etc.

```ocaml
[%%metapackage metapp]
[%%meta Metapp.Stri.of_list @@ (new Metapp.filter)#structure [%str
  type t =
    | A of int
    | B of int * int
        [@if [%meta Metapp.Exp.of_bool (Sys.ocaml_version >= "4.04.0")]]
    (* ... *)

  let somefunction v =
    match (v: t) with
    | A x -> something x
    | B (y,z)
      [@if [%meta Metapp.Exp.of_bool (Sys.ocaml_version >= "4.04.0")]] ->
        something' y z
    (* ... *) ]]
```

Usually, when `[@if false]` appears in the components of a tuple (in
an expression or a pattern), these components are removed: if the
tuple becomes empty, the tuple is rewritten as the unit constructor
`()`; if the tuple is reduced to a singleton `(v)`, the tuple is
rewritten as the value `v` itself.

Multiple arguments of a variant constructor are considered as a tuple
syntactically: `C (a, b [@if false])` is rewritten as `C a` and
`C (a [@if false], b [@if false])` is rewritten as `C ()` (a constructor `C`
with a single argument of type `unit`), whereas
`C ((a, b) [@if false])` is rewritten as `C` (a constant constructor `C`).
There is a special case for the list constructor `::`: `a [@if false]::b`
is rewritten as `b` (instead of applying the constructor `::` to the
single argument `b`, which is most probably incorrect). This allows
`[@if false]` to be used to filter list elements as in
`[e1; ...; ei [@if false]; ...; en]`.

Global definitions for meta-code can be included with `[%%metadef
...]`.
By default, the meta-code is compiled with the `compiler-libs` package.
Other packages can be loaded with `[%%metapackage ...]`.
More generally, flags can be passed to the compiler to compile meta-code
with `[%%metaflag ...]` (there is another convenient notation for
adding interface directories: `[%%metadir ...]`).
`[%%metaload ...]` loads a particular compilation unit.
For instance, `[%%metapackage metapp]` links the meta-code with the
`metapp` package in order to use the [`Metapp`] module.
All these notations can be applied to multiple arguments at once by using
comma as separator.

Note that dynamic package loading is broken in PPX with dune
([#3214]).  When packages are loaded with `[%%metapackage ...]`, a
workaround ([see discussion]) is used to load the packages
correctly, but only with OCaml >=4.08. If you need to use
`[%%metapackage ...]` with a prior version of OCaml, you still can
statically link the packages you need by listing them in the
`(preprocess (pps ...))` list. You will still have to import them with
`[%%metapackage ...]` to let the compiler see their interface when
compiling the meta-code.

[#3214]: https://github.com/ocaml/dune/issues/3214
[see discussion]: https://discuss.ocaml.org

Compilation commands can be logged by adding `[%%metaverbose]` to the
preprocessed file.

## How does `metapp` differ from [`ppx_stage`]

(Discussed in [this thread](https://discuss.ocaml.org/t/creating-a-ppx-that-transforms-record-updates/10189/4))

[`ppx_stage`]: https://github.com/stedolan/ppx_stage

- By using `ppx_stage`, a program can generate and execute other
  programs at runtime (by invoking the compiler and dynamic loader at
  runtime). By using metapp, a program can contain portion of codes
  that are generated at compile time by executing other programs (by
  invoking the compiler and dynamic loader during the preprocessing
  phase).

- With `ppx_stage`, each piece of code that is manipulated is checked
  to be well-typed: a value of type `'a code` can be seen as a `unit
  -> 'a` closure, with an efficient composition (by compiling). With
  `metapp`, meta-programs directly manipulate the parse tree, with the
  only constraint that the parse tree produced at the end should lead
  to a well-typed program: this is more error prone, but that allows
  to describe syntax extensions where we can manipulate as first-class
  values some piece of syntax like record fields, that are not
  first-class citizens in the language.
