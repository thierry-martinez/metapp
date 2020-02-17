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

let mkloc (txt : 'a) : 'a Location.loc =
  { txt; loc = !Ast_helper.default_loc }

let ident ?attrs (ident : Longident.t) : Parsetree.expression =
  Ast_helper.Exp.ident ?attrs (mkloc ident)

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

  val to_loc : t -> Location.t

  val var : ?attrs:Parsetree.attributes -> string -> t

  val of_constant : ?attrs:Parsetree.attributes -> Parsetree.constant -> t

  val force_tuple : ?attrs:Parsetree.attributes -> t list -> t

  val force_construct :
      ?attrs:Parsetree.attributes -> Ast_helper.lid -> t option -> t

  val record : ?attrs:Parsetree.attributes -> (Longident.t * t) list -> t

  val variant : ?attrs:Parsetree.attributes -> string -> t option -> t

  val choice :
      (unit -> Parsetree.expression) -> (unit -> Parsetree.pattern) -> t

  val mapper : Ast_mapper.mapper -> Ast_mapper.mapper -> t -> t

  val destruct_extension : t -> Parsetree.extension option

  val of_payload : Parsetree.payload -> t
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

  val nil : ?attrs:Parsetree.attributes -> ?prefix:Longident.t -> unit -> t

  val cons : ?attrs:Parsetree.attributes -> ?prefix:Longident.t -> t -> t -> t

  val of_list :
      ?attrs:Parsetree.attributes -> ?prefix:Longident.t -> t list -> t
end

let unit_ctor = "()"

let none_ctor = "None"

let some_ctor = "Some"

let nil_ctor = "[]"

let cons_ctor = "::"

let longident = Longident.Lident "Longident"

let make_ident ?(prefix : Longident.t option) (s : string) : Longident.t =
  match prefix with
  | None -> Lident s
  | Some prefix -> Ldot (prefix, s)

module ExtendValue (Base : BaseValueS) : ValueS with type t = Base.t = struct
  include Base

  let of_int ?attrs i =
    of_constant ?attrs (Ast_helper.Const.int i)

  let of_string ?attrs s =
    of_constant ?attrs (Ast_helper.Const.string s)

  let of_char ?attrs s =
    of_constant ?attrs (Ast_helper.Const.char s)

  let of_unit ?attrs () =
    force_construct ?attrs (mkloc (Longident.Lident unit_ctor)) None

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
    force_construct ?attrs (mkloc ident) arg

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

  let nil ?attrs ?prefix () =
    construct ?attrs (make_ident ?prefix nil_ctor) []

  let cons ?attrs ?prefix hd tl =
    construct ?attrs (make_ident ?prefix cons_ctor) [hd; tl]

  let rec of_list ?attrs ?prefix l =
    match l with
    | [] -> nil ?attrs ?prefix ()
    | hd :: tl -> cons ?attrs ?prefix hd (of_list ?prefix tl)
end

module Exp : ValueS with type t = Parsetree.expression =
    ExtendValue (struct
  type t = Parsetree.expression

  let to_loc (e : Parsetree.expression) : Location.t =
    e.pexp_loc

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
      (List.map (fun (field, value) -> (mkloc field, value)) fields)
      None

  let variant ?attrs (ctor : string) (arg : t option) : t =
    Ast_helper.Exp.variant ?attrs ctor arg

  let choice (e : unit -> Parsetree.expression) (_p : unit ->Parsetree.pattern)
      : t =
    e ()

  let mapper (mapper : Ast_mapper.mapper) =
    mapper.expr

  let destruct_extension (e : Parsetree.expression)
      : Parsetree.extension option =
    match e.pexp_desc with
    | Pexp_extension extension -> Some extension
    | _ -> None

  let of_payload = expression_of_payload
end)

module Pat : ValueS with type t = Parsetree.pattern =
    ExtendValue (struct
  type t = Parsetree.pattern

  let to_loc (p : Parsetree.pattern) : Location.t =
    p.ppat_loc

  let var ?attrs x =
    Ast_helper.Pat.var ?attrs (mkloc x)

  let of_constant ?attrs cst =
    Ast_helper.Pat.constant ?attrs cst

  let force_tuple ?attrs (args : t list) : t =
    Ast_helper.Pat.tuple ?attrs args

  let force_construct ?attrs (lid : Ast_helper.lid) (args : t option) : t =
    Ast_helper.Pat.construct ?attrs lid args

  let record ?attrs (fields : (Longident.t * t) list) : t =
    Ast_helper.Pat.record ?attrs
      (List.map (fun (field, value) -> (mkloc field, value)) fields)
      Closed

  let variant ?attrs (ctor : string) (arg : t option) : t =
    Ast_helper.Pat.variant ?attrs ctor arg

  let choice (_e : unit -> Parsetree.expression) (p : unit -> Parsetree.pattern)
      : t =
    p ()

  let mapper (mapper : Ast_mapper.mapper) =
    mapper.pat

  let destruct_extension (e : Parsetree.pattern) : Parsetree.extension option =
    match e.ppat_desc with
    | Ppat_extension extension -> Some extension
    | _ -> None

  let of_payload = pattern_of_payload
end)

type value = {
    exp : Parsetree.expression;
    pat : Parsetree.pattern;
  }

module Val : ValueS with type t = value = ExtendValue (struct
  type t = value

  let rec split (l : value list)
      : Parsetree.expression list * Parsetree.pattern list =
    match l with
    | [] -> ([], [])
    | hd :: tl ->
        let (tl_exp, tl_pat) = split tl in
        (hd.exp :: tl_exp, hd.pat :: tl_pat)

  let split_option (o : value option)
      : Parsetree.expression option * Parsetree.pattern option =
      match o with
      | None -> (None, None)
      | Some { exp; pat } -> (Some exp, Some pat)

  let rec split_assoc (l : ('a * value) list)
      : ('a * Parsetree.expression) list * ('a * Parsetree.pattern) list =
    match l with
    | [] -> ([], [])
    | (key, hd) :: tl ->
        let (tl_exp, tl_pat) = split_assoc tl in
        ((key, hd.exp) :: tl_exp, (key, hd.pat) :: tl_pat)

  let to_loc (v : value) : Location.t =
    v.exp.pexp_loc

  let var ?attrs x =
    { exp = Exp.var ?attrs x; pat = Pat.var ?attrs x }

  let of_constant ?attrs cst =
    { exp = Exp.of_constant ?attrs cst; pat = Pat.of_constant ?attrs cst }

  let force_tuple ?attrs (args : t list) : t =
    let args_exp, args_pat = split args in
    { exp = Exp.force_tuple ?attrs args_exp;
      pat = Pat.force_tuple ?attrs args_pat; }

  let force_construct ?attrs (lid : Ast_helper.lid) (args : t option) : t =
    let args_exp, args_pat = split_option args in
    { exp = Exp.force_construct ?attrs lid args_exp;
      pat = Pat.force_construct ?attrs lid args_pat; }

  let record ?attrs (fields : (Longident.t * t) list) : t =
    let fields_exp, fields_pat = split_assoc fields in
    { exp = Exp.record ?attrs fields_exp;
      pat = Pat.record ?attrs fields_pat; }

  let variant ?attrs (ctor : string) (arg : t option) : t =
    let arg_exp, arg_pat = split_option arg in
    { exp = Exp.variant ?attrs ctor arg_exp;
      pat = Pat.variant ?attrs ctor arg_pat; }

  let choice (e : unit -> Parsetree.expression) (p : unit -> Parsetree.pattern)
      : t =
    { exp = e ();
      pat = p (); }

  let mapper (_mapper : Ast_mapper.mapper) =
    failwith "value cannot be mapped"

  let destruct_extension (v : value) : Parsetree.extension option =
    match v.exp.pexp_desc with
    | Pexp_extension extension -> Some extension
    | _ -> None

  let of_payload _ =
    failwith "value cannot be obtained from payload"
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
