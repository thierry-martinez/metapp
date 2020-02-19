[%%metadir "utils/.metapp_utils.objs/byte/"]

let () =
  Test_framework.assert_eq Stdcompat.Int.equal Format.pp_print_int
    [%meta Metapp_utils.filter.expr Metapp_utils.filter
       [%e (1, 2 [@if false])]] 1

[%%meta Metapp_utils.filter.structure_item Metapp_utils.filter
  [%stri let a = 1 and[@if false] b = c]]

let () =
  Test_framework.assert_eq Stdcompat.Int.equal Format.pp_print_int a 1
