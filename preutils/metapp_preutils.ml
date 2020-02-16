let expression_of_payload (payload : Parsetree.payload) : Parsetree.expression =
  match payload with
  | PStr [{ pstr_desc = Pstr_eval (expr, []); _ }] ->
      expr
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "Expression expected"

let structure_of_expression (e : Parsetree.expression) : Parsetree.structure =
  [Ast_helper.Str.eval e]

let payload_of_expression (e : Parsetree.expression) : Parsetree.payload =
  PStr (structure_of_expression e)

let pattern_of_payload (payload : Parsetree.payload) : Parsetree.pattern =
  match payload with
  | PPat (pat, None) -> pat
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc "Pattern expected"

let structure_of_payload (payload : Parsetree.payload) : Parsetree.structure =
  match payload with
  | PStr str -> str
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc "Structure expected"

let structure_item_of_payload (payload : Parsetree.payload)
    : Parsetree.structure_item =
  match payload with
  | PStr [item] -> item
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "Single structure item expected"

let signature_of_payload (payload : Parsetree.payload) : Parsetree.signature =
  match payload with
  | PSig sgn -> sgn
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc "Signature expected"

let signature_item_of_payload (payload : Parsetree.payload)
    : Parsetree.signature_item =
  match payload with
  | PSig [item] -> item
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "Single signature item expected"

let core_type_of_payload (payload : Parsetree.payload) : Parsetree.core_type =
  match payload with
  | PTyp typ -> typ
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc "Type expected"

let loc (txt : 'a) : 'a Location.loc =
  { txt; loc = !Ast_helper.default_loc }

let ident ?attrs (ident : Longident.t) : Parsetree.expression =
  Ast_helper.Exp.ident ?attrs (loc ident)

let nolabel arg =
  (Asttypes.Nolabel, arg)

let nolabels args =
  List.map nolabel args

let apply_labels (f : Parsetree.expression)
    (labels : (string * Parsetree.expression) list)
    (args : Parsetree.expression list) : Parsetree.expression =
  Ast_helper.Exp.apply f
    (List.map (fun (l, e) -> (Asttypes.Labelled l, e)) labels @ nolabels args)

let apply (f : Parsetree.expression) (args : Parsetree.expression list)
    : Parsetree.expression =
  Ast_helper.Exp.apply f (nolabels args)

module type BaseValueS = sig
  type t

  val var : ?attrs:Parsetree.attributes -> string -> t

  val of_constant : ?attrs:Parsetree.attributes -> Parsetree.constant -> t

  val force_tuple : ?attrs:Parsetree.attributes -> t list -> t

  val force_construct :
      ?attrs:Parsetree.attributes -> Ast_helper.lid -> t option -> t

  val record : ?attrs:Parsetree.attributes -> (Longident.t * t) list -> t
end

module type ValueS = sig
  include BaseValueS

  val of_int : ?attrs:Parsetree.attributes -> int -> t

  val of_string : ?attrs:Parsetree.attributes -> string -> t

  val of_char : ?attrs:Parsetree.attributes -> char -> t

  val of_unit : ?attrs:Parsetree.attributes -> unit -> t

  val of_bool : ?attrs:Parsetree.attributes -> bool -> t

  val none : ?attrs:Parsetree.attributes -> unit -> t

  val some : ?attrs:Parsetree.attributes -> t -> t

  val of_option : ?attrs:Parsetree.attributes -> t option -> t

  val of_longident : Longident.t -> t

  val construct : ?attrs:Parsetree.attributes -> Longident.t -> t list -> t

  val tuple : ?attrs:Parsetree.attributes -> t list -> t

  val nil : ?attrs:Parsetree.attributes -> unit -> t

  val cons : ?attrs:Parsetree.attributes -> t -> t -> t

  val of_list : ?attrs:Parsetree.attributes -> t list -> t
end

let unit_ctor = "()"

let none_ctor = "None"

let some_ctor = "Some"

let nil_ctor = "[]"

let cons_ctor = "::"

let longident = Longident.Lident "Longident"

module ExtendValue (Base : BaseValueS) : ValueS with type t = Base.t = struct
  include Base

  let of_int ?attrs i =
    of_constant ?attrs (Ast_helper.Const.int i)

  let of_string ?attrs s =
    of_constant ?attrs (Ast_helper.Const.string s)

  let of_char ?attrs s =
    of_constant ?attrs (Ast_helper.Const.char s)

  let of_unit ?attrs () =
    force_construct ?attrs (loc (Longident.Lident unit_ctor)) None

  let tuple ?attrs (args : t list) : t =
    match args with
    | [] -> of_unit ?attrs ()
    | [arg] -> arg
    | _ -> force_tuple ?attrs args

  let construct ?attrs (ident : Longident.t) (args : t list) : t =
    let arg =
      match args with
      | [] -> None
      | [arg] -> Some arg
      | _ -> Some (tuple args) in
    force_construct ?attrs (loc ident) arg

  let of_bool ?attrs b =
    construct ?attrs (Lident (string_of_bool b)) []

  let none ?attrs () =
    construct ?attrs (Lident none_ctor) []

  let some ?attrs x =
    construct ?attrs (Lident some_ctor) [x]

  let of_option ?attrs opt =
    match opt with
    | None -> none ?attrs ()
    | Some x -> some ?attrs x

  let rec of_longident (ident : Longident.t) : t =
    match ident with
    | Ldot (m, v) ->
        construct (Ldot (longident, "Ldot"))
          [of_longident m; of_string v]
    | Lident ident ->
        construct (Ldot (longident, "Lident")) [of_string ident]
    | Lapply (f, x) ->
        construct (Ldot (longident, "Lapply"))
          [of_longident f; of_longident x]

  let nil ?attrs () =
    construct ?attrs (Lident nil_ctor) []

  let cons ?attrs hd tl =
    construct ?attrs (Lident cons_ctor) [hd; tl]

  let rec of_list ?attrs l =
    match l with
    | [] -> nil ?attrs ()
    | hd :: tl -> cons ?attrs hd (of_list tl)
end

module Exp : ValueS with type t = Parsetree.expression =
    ExtendValue (struct
  type t = Parsetree.expression

  let var ?attrs x =
    ident ?attrs (Lident x)

  let of_constant ?attrs cst =
    Ast_helper.Exp.constant ?attrs cst

  let force_tuple ?attrs (args : t list) : t =
    Ast_helper.Exp.tuple ?attrs args

  let force_construct ?attrs (lid : Ast_helper.lid) (args : t option) : t =
    Ast_helper.Exp.construct ?attrs lid args

  let record ?attrs (fields : (Longident.t * t) list) : t =
    Ast_helper.Exp.record ?attrs
      (List.map (fun (field, value) -> (loc field, value)) fields)
      None
end)

module Pat : ValueS with type t = Parsetree.pattern =
    ExtendValue (struct
  type t = Parsetree.pattern

  let var ?attrs x =
    Ast_helper.Pat.var ?attrs (loc x)

  let of_constant ?attrs cst =
    Ast_helper.Pat.constant ?attrs cst

  let force_tuple ?attrs (args : t list) : t =
    Ast_helper.Pat.tuple ?attrs args

  let force_construct ?attrs (lid : Ast_helper.lid) (args : t option) : t =
    Ast_helper.Pat.construct ?attrs lid args

  let record ?attrs (fields : (Longident.t * t) list) : t =
    Ast_helper.Pat.record ?attrs
      (List.map (fun (field, value) -> (loc field, value)) fields)
      Closed
end)

let int_of_expression (e : Parsetree.expression) =
  Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match
    match e.pexp_desc with
    | Pexp_constant (Pconst_integer (value, _)) ->
        Stdcompat.int_of_string_opt value
    | _ ->
        None
  with
  | Some result -> result
  | None ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "Integer value expected"

let payload_of_int (i : int) : Parsetree.payload =
  payload_of_expression (Exp.of_int i)

let int_of_payload (payload : Parsetree.payload) : int =
  int_of_expression (expression_of_payload payload)

let string_of_expression (e : Parsetree.expression) =
  Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match e.pexp_desc with
  | Pexp_constant (Pconst_string (value, _)) ->
      value
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "String value expected"

let string_of_payload (payload : Parsetree.payload) : string =
  string_of_expression (expression_of_payload payload)

let bool_of_expression (e : Parsetree.expression) : bool =
  Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "false"; _ }, None) ->
      false
  | Pexp_construct ({ txt = Lident "true"; _ }, None) ->
      true
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "Boolean value expected"

let bool_of_payload (payload : Parsetree.payload) : bool =
  bool_of_expression (expression_of_payload payload)

let pair_of_expression (e : Parsetree.expression)
    : Parsetree.expression * Parsetree.expression =
  Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match e.pexp_desc with
  | Pexp_tuple [a; b] -> (a, b)
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "Pair expected"

let rec list_of_expression (e : Parsetree.expression)
    : Parsetree.expression list =
  Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> []
  | Pexp_construct ({ txt = Lident "::"; _ }, Some pair) ->
      let (hd, tl) = pair_of_expression pair in
      hd :: list_of_expression tl
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "List expected"

let update f ref =
  let (result, new_contents) = f !ref in
  ref := new_contents;
  result

let mutate f ref =
  ref := f !ref

let sequence (list : Parsetree.expression list) : Parsetree.expression =
  match list with
  | [] -> Exp.of_unit ()
  | [singleton] -> singleton
  | hd :: tl ->
      List.fold_left Ast_helper.Exp.sequence hd tl

let include_signature (signature : Parsetree.signature)
    : Parsetree.signature_item =
  Ast_helper.Sig.include_ (Ast_helper.Incl.mk
    (Ast_helper.Mty.signature signature))

let include_structure (structure : Parsetree.structure)
    : Parsetree.structure_item =
  Ast_helper.Str.include_ (Ast_helper.Incl.mk
    (Ast_helper.Mod.structure structure))

let bool = Exp.of_bool

let lid_of_str (str : Ast_helper.str) : Ast_helper.lid =
  Location.mkloc (Longident.Lident str.txt) str.loc

let map_loc (f : 'a -> 'b) (l : 'a Location.loc) : 'b Location.loc =
  { l with txt = f l.txt }
