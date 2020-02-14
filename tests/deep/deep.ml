[%%metapackage "stdcompat"]

let () =
  assert
    ([%meta
      Ast_helper.Exp.tuple (Stdcompat.List.init 4 (fun i ->
        [%e ([%meta Metapp_preutils.expression_of_int i], false)]))]
      = ((0, false), (1, false), (2, false), (3, false)))
