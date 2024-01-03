[%%metadir "metapp/.metapp.objs/byte/"]
[%%metaload "metapp/metapp.cmxs"]
[%%metapackage "stdcompat"]
[%%metaflag "-open", "Stdcompat"]

let () =
  Test_framework.assert_eq Int.equal Format.pp_print_int
    [%meta (new Metapp.filter)#expression
       [%e (1, 2 [@if false])]] 1

[%%meta (new Metapp.filter)#structure_item
  [%stri let a = 1 and[@if false] b = c]]

type other = AX | CX

[%%meta (new Metapp.filter)#structure_item
  [%stri type test = other =  AX | BX [@if false] | CX]
]

type other2 = { a: int; b: float }

[%%meta (new Metapp.filter)#structure_item
  [%stri type test2 = other2 = { a: int; b: float; c: bool [@if false]}]
]

let make2 x =
  [%meta (new Metapp.filter)#expression [%e
    { a = 4; b = 5.0; c = 4 [@if false] }
  ]]

let something' _ _ = ()
let something _ = ()

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

[%%meta Metapp.Stri.of_list ((new Metapp.filter)#structure [%str
let () =
  assert (List.length [0; 1 [@if false]; 2] = 2)

let () =
  match [0; 2] with
  | [0; 1 [@if false]; 2] -> ()
  | _ -> assert false
])]

let () =
  Test_framework.assert_eq Int.equal Format.pp_print_int a 1

let () =
  assert
    [%meta
       Metapp.Exp.of_bool (Option.equal Metapp.Longident.equal
         (Metapp.Longident.of_expression_opt [%e M.(+)])
         (Some (Longident.Ldot (Lident "M", "+"))))]
