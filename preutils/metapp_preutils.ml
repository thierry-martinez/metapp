let expression_of_payload (payload : Parsetree.payload) : Parsetree.expression =
  match payload with
  | PStr [{ pstr_desc = Pstr_eval (expr, []); _ }] ->
      expr
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "Single expression expected"

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

let expression_of_ident (ident : Longident.t) : Parsetree.expression =
  Ast_helper.Exp.ident (loc ident)

let ident = expression_of_ident

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

let expression_construct (ident : Longident.t)
    (args : Parsetree.expression list) : Parsetree.expression =
  let arg =
    match args with
    | [] -> None
    | [arg] -> Some arg
    | _ -> Some (Ast_helper.Exp.tuple args) in
  Ast_helper.Exp.construct (loc ident) arg

let construct = expression_construct

let pattern_construct (ident : Longident.t) (args : Parsetree.pattern list)
    : Parsetree.pattern =
  let arg =
    match args with
    | [] -> None
    | [arg] -> Some arg
    | _ -> Some (Ast_helper.Pat.tuple args) in
  Ast_helper.Pat.construct (loc ident) arg

let expression_record (fields : (Longident.t * Parsetree.expression) list)
    : Parsetree.expression =
  Ast_helper.Exp.record
    (List.map (fun (field, value) -> (loc field, value)) fields)
    None

let pattern_record (fields : (Longident.t * Parsetree.pattern) list)
    : Parsetree.pattern =
  Ast_helper.Pat.record
    (List.map (fun (field, value) -> (loc field, value)) fields)
    Closed

let expression_of_default_loc () : Parsetree.expression =
  apply (ident (Lident "!")) [ident (Ldot (Lident "Ast_helper", "default_loc"))]

let expression_of_loc (quote_txt : 'a -> Parsetree.expression)
    (loc : 'a Location.loc) : Parsetree.expression =
  expression_record [
    (Ldot (Lident "Location", "txt"), quote_txt loc.txt);
    (Lident "loc", expression_of_default_loc ())]

let expression_of_int (i : int) : Parsetree.expression =
  Ast_helper.Exp.constant (Ast_helper.Const.int i)

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
  payload_of_expression (expression_of_int i)

let int_of_payload (payload : Parsetree.payload) : int =
  int_of_expression (expression_of_payload payload)

let expression_of_string (s : string) : Parsetree.expression =
  Ast_helper.Exp.constant (Ast_helper.Const.string s)

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

let expression_of_char (c : char) : Parsetree.expression =
  Ast_helper.Exp.constant (Ast_helper.Const.char c)

let unit_cstr = "()"

let expression_of_unit () : Parsetree.expression =
  construct (Lident unit_cstr) []

let pattern_of_unit () : Parsetree.pattern =
  pattern_construct (Lident unit_cstr) []

let unit () = expression_of_unit ()

let none_cstr = "None"

let some_cstr = "Some"

let expression_none = construct (Lident none_cstr) []

let expression_some item = construct (Lident some_cstr) [item]

let pattern_none = pattern_construct (Lident none_cstr) []

let pattern_some item = pattern_construct (Lident some_cstr) [item]

let expression_of_bool b = construct (Lident (string_of_bool b)) []

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

let bool = expression_of_bool

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
  | [] -> unit ()
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
