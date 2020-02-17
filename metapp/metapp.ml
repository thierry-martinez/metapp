module Counter = struct
  type t = int ref

  let make () =
    ref 0

  let count counter =
    let result = !counter in
    counter := succ result;
    result
end

let extension_of_index (i : int) : Parsetree.extension =
  (Metapp_preutils.mkloc "meta", Metapp_preutils.payload_of_int i)

let deref (e : Parsetree.expression) : Parsetree.expression =
  Metapp_preutils.apply (Metapp_preutils.ident (Lident "!")) [e]

let array_get (a : Parsetree.expression) (index : int) : Parsetree.expression =
  let i = Metapp_preutils.Exp.of_int index in
  Metapp_preutils.apply (Metapp_preutils.ident (Ldot (Lident "Array", "get")))
    [a; i]

let array_set (a : Parsetree.expression) (index : int)
    (v : Parsetree.expression) : Parsetree.expression =
  let i = Metapp_preutils.Exp.of_int index in
  Metapp_preutils.apply (Metapp_preutils.ident (Ldot (Lident "Array", "set")))
    [a; i; v]

type instruction =
  | Load of string
  | Package of string
  | Directory of string
  | Flags of string list
  | Exp of Parsetree.expression
  | Definition of Parsetree.structure
  | Plainsource

let get_expression (instruction : instruction Location.loc)
    : Parsetree.expression =
  match instruction.txt with
  | Exp expression -> expression
  | _ ->
      Location.raise_errorf ~loc:instruction.loc
        "Definitions are only allowed at top-level"

module rec AccuTypes : sig
  type escape = {
      instructions : instruction Location.loc list;
      context : Metapp_api.context;
    }

  type 'a holes = ((unit -> 'a) * escape) Accu.t ref

  type 'a context = Location.t Accu.t ref
end = struct
  include AccuTypes
end
and MutableHoles : Metapp_api.HolesWithMakeS with type 'a x = 'a AccuHole.t =
    Metapp_api.HolesWithMake (AccuHole)
and AccuHole : Metapp_api.UnaryMake
with type 'a t = 'a AccuTypes.holes
    = struct
  type 'a t = 'a AccuTypes.holes

  let make () =
    ref Accu.empty
end
and MutableContext : Metapp_api.HolesWithMakeS
with type 'a x = 'a AccuContext.t = Metapp_api.HolesWithMake (AccuContext)
and AccuContext : Metapp_api.UnaryMake
with type 'a t = 'a AccuTypes.context = struct
  type 'a t = 'a AccuTypes.context

  let make () =
    ref Accu.empty
end

module UnaryCounter = struct
  type 'a t = Counter.t

  let make = Counter.make
end

module MutableHolesCount = Metapp_api.HolesWithMake (UnaryCounter)

module HoleMapper = struct
  type 'a t = Parsetree.payload -> 'a
end

module HolesMapper = Metapp_api.Holes (HoleMapper)

let holes_mapper ?(def : (instruction Location.loc -> unit) option)
    (holes : HolesMapper.t) : Ast_mapper.mapper =
  let expr (mapper : Ast_mapper.mapper) (e : Parsetree.expression)
      : Parsetree.expression =
    Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
    match e.pexp_desc with
    | Pexp_extension ({ txt = "meta"; _ }, payload) ->
        holes.expr payload
    | _ -> Ast_mapper.default_mapper.expr mapper e in
  let pat (mapper : Ast_mapper.mapper) (pat : Parsetree.pattern)
      : Parsetree.pattern =
    Ast_helper.with_default_loc pat.ppat_loc @@ fun () ->
    match pat.ppat_desc with
    | Ppat_extension ({ txt = "meta"; _ }, payload) ->
        holes.pat payload
    | _ -> Ast_mapper.default_mapper.pat mapper pat in
  let structure_item (mapper : Ast_mapper.mapper)
      (item : Parsetree.structure_item) : Parsetree.structure_item =
    Ast_helper.with_default_loc item.pstr_loc @@ fun () ->
    match item.pstr_desc with
    | Pstr_extension (({ txt = "meta"; _ }, payload), _) ->
        holes.structure_item payload
    | Pstr_extension (({ txt = "metadef"; _ }, payload), _) ->
        Stdcompat.Option.get def (Metapp_preutils.mkloc
          (Definition (Metapp_preutils.structure_of_payload payload)));
        Metapp_preutils.include_structure []
    | Pstr_extension (({ txt = "metaload"; _ }, payload), _) ->
        Stdcompat.Option.get def (Metapp_preutils.mkloc
          (Load (Metapp_preutils.string_of_payload payload)));
        Metapp_preutils.include_structure []
    | Pstr_extension (({ txt = "metapackage"; _ }, payload), _) ->
        Stdcompat.Option.get def (Metapp_preutils.mkloc
          (Package (Metapp_preutils.string_of_payload payload)));
        Metapp_preutils.include_structure []
    | Pstr_extension (({ txt = "metadir"; _ }, payload), _) ->
        Stdcompat.Option.get def (Metapp_preutils.mkloc
          (Directory (Metapp_preutils.string_of_payload payload)));
        Metapp_preutils.include_structure []
    | Pstr_extension (({ txt = "metaflags"; _ }, payload), _) ->
        Stdcompat.Option.get def (Metapp_preutils.mkloc
          (Flags (List.map Metapp_preutils.string_of_expression
            (Metapp_preutils.list_of_expression
              (Metapp_preutils.expression_of_payload payload)))));
        Metapp_preutils.include_structure []
    | Pstr_extension (({ txt = "metaplainsource"; _ }, _payload), _) ->
        Stdcompat.Option.get def (Metapp_preutils.mkloc Plainsource);
        Metapp_preutils.include_structure []
    | _ -> Ast_mapper.default_mapper.structure_item mapper item in
  { Ast_mapper.default_mapper with expr; pat; structure_item }


(*
module UnaryInt = struct
  type 'a t = int
end

module HolesCount = Metapp_api.Holes (UnaryInt)
*)

let unmut_context (context : MutableContext.t) : Metapp_api.OptionArrayHoles.t =
  let module Map = Metapp_api.HolesMap (MutableContext)
      (Metapp_api.OptionArrayHoles)
      (struct
        type 'a x = 'a AccuContext.t

        type 'a y = 'a option array

        let map accu = Array.make (Accu.length !accu) None
      end) in
  Map.map context

let unmut_loc (context : MutableContext.t) : Metapp_api.LocationHoles.t =
  let module Map = Metapp_api.HolesMap (MutableContext)
      (Metapp_api.LocationHoles)
      (struct
        type 'a x = 'a AccuContext.t

        type _ y = Location.t array

        let map accu = Accu.to_array !accu
      end) in
  Map.map context

let unmut_subholes (context : MutableHoles.t) : Metapp_api.ArrayHoles.t =
  let module Map = Metapp_api.HolesMap (MutableHoles) (Metapp_api.ArrayHoles)
      (struct
        type 'a x = 'a AccuHole.t

        type 'a y = 'a Metapp_api.ArrayHole.t

        let map accu = Array.map
            (fun (fill, { AccuTypes.context; _ }) ->
              ({ context; fill } : 'a Metapp_api.ArrayHole.hole))
            (Accu.to_array !accu)
      end) in
  Map.map context

(*
let unmut_holes_count (holes_count : MutableHolesCount.t) : HolesCount.t =
  let module Map = Metapp_api.HolesMap (MutableHolesCount.UnaryCounter)
      (UnaryInt) (struct
    type 'a x = Counter.t

    type 'a y = int

    let map = (!)
  end) in
  Map.map holes_count
*)

let context_var = "__context"

let fill_var = "__fill"

let holes_field = "holes"

let loc_field = "loc"

let sub_holes_field = "sub_holes"

let field_get (expr : Parsetree.expression) (field : string)
    : Parsetree.expression =
  Ast_helper.Exp.field expr (Metapp_preutils.mkloc (Longident.Lident field))

let context_get (field : string) : Parsetree.expression =
  field_get (Metapp_preutils.ident (Lident context_var)) field

let unhandled_hole _ =
  assert false

let replace_holes (contents : Metapp_api.OptionArrayHoles.t)
    : Ast_mapper.mapper =
  let expr (payload : Parsetree.payload) : Parsetree.expression =
    Stdcompat.Option.get
      contents.expr.(Metapp_preutils.int_of_payload payload) in
  let pat (payload : Parsetree.payload) : Parsetree.pattern =
    Stdcompat.Option.get
      contents.pat.(Metapp_preutils.int_of_payload payload) in
  let typ (payload : Parsetree.payload) : Parsetree.core_type =
    Stdcompat.Option.get
      contents.typ.(Metapp_preutils.int_of_payload payload) in
  let class_type (payload : Parsetree.payload) : Parsetree.class_type =
    Stdcompat.Option.get
      contents.class_type.(Metapp_preutils.int_of_payload payload) in
  let class_type_field (payload : Parsetree.payload)
      : Parsetree.class_type_field =
    Stdcompat.Option.get
      contents.class_type_field.(Metapp_preutils.int_of_payload payload) in
  let class_expr (payload : Parsetree.payload) : Parsetree.class_expr =
    Stdcompat.Option.get
      contents.class_expr.(Metapp_preutils.int_of_payload payload) in
  let class_field (payload : Parsetree.payload) : Parsetree.class_field =
    Stdcompat.Option.get
      contents.class_field.(Metapp_preutils.int_of_payload payload) in
  let module_type (payload : Parsetree.payload) : Parsetree.module_type =
    Stdcompat.Option.get
      contents.module_type.(Metapp_preutils.int_of_payload payload) in
  let module_expr (payload : Parsetree.payload) : Parsetree.module_expr =
    Stdcompat.Option.get
      contents.module_expr.(Metapp_preutils.int_of_payload payload) in
  let signature_item (payload : Parsetree.payload) : Parsetree.signature_item =
    Stdcompat.Option.get
      contents.signature_item.(Metapp_preutils.int_of_payload payload) in
  let structure_item (payload : Parsetree.payload) : Parsetree.structure_item =
    Stdcompat.Option.get
      contents.structure_item.(Metapp_preutils.int_of_payload payload) in
  holes_mapper {
    expr; pat; typ; class_type; class_type_field; class_expr; class_field;
    module_type; module_expr; signature_item; structure_item;
    signature = unhandled_hole; structure = unhandled_hole; }

let metapp_api = Longident.Lident "Metapp_api"

let rec extract_subexpressions (holes : MutableHoles.t) : Ast_mapper.mapper =
  let push_hole (type hole) (hole : hole)
      (get_mapper : Ast_mapper.mapper -> Ast_mapper.mapper -> hole -> hole)
      (accu : hole AccuTypes.holes) (field_name : string)
      : Parsetree.expression =
    let (mapper, k) = extract_holes () in
    let hole = get_mapper mapper mapper hole in
    let escape = k () in
    let fill () =
      let mapper = replace_holes escape.context.holes in
      get_mapper mapper mapper hole in
    let index = Metapp_preutils.update (Accu.add (fill, escape)) accu in
    Ast_helper.Exp.let_ Nonrecursive
      [Ast_helper.Vb.mk (Metapp_preutils.Pat.record [
         (Ldot (Ldot (metapp_api, "ArrayHole"), "context"),
           Metapp_preutils.Pat.var context_var);
           (Lident "fill", Metapp_preutils.Pat.var fill_var)])
        (array_get (field_get (context_get sub_holes_field) field_name) index)]
      (Metapp_preutils.sequence (List.map get_expression escape.instructions @
        [Metapp_preutils.apply (Metapp_preutils.ident (Lident fill_var))
          [Metapp_preutils.Exp.of_unit ()]])) in
  let expr (mapper : Ast_mapper.mapper) (e : Parsetree.expression)
      : Parsetree.expression =
    Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
    match e.pexp_desc with
    | Pexp_extension ({ txt = ("e" | "expr"); _ }, payload) ->
        push_hole (Metapp_preutils.expression_of_payload payload)
          (fun mapper -> mapper.expr) holes.expr Metapp_api.holes_name.expr
    | Pexp_extension ({ txt = ("p" | "pat"); _ }, payload) ->
        push_hole (Metapp_preutils.pattern_of_payload payload)
          (fun mapper -> mapper.pat) holes.pat Metapp_api.holes_name.pat
    | Pexp_extension ({ txt = ("t" | "type"); _ }, payload) ->
        push_hole (Metapp_preutils.core_type_of_payload payload)
          (fun mapper -> mapper.typ) holes.typ Metapp_api.holes_name.typ
    | Pexp_extension ({ txt = "sig"; _ }, payload) ->
        push_hole (Metapp_preutils.signature_of_payload payload)
          (fun mapper -> mapper.signature) holes.signature
          Metapp_api.holes_name.signature
    | Pexp_extension ({ txt = "sigi"; _ }, payload) ->
        push_hole (Metapp_preutils.signature_item_of_payload payload)
          (fun mapper -> mapper.signature_item) holes.signature_item
          Metapp_api.holes_name.signature_item
    | Pexp_extension ({ txt = "str"; _ }, payload) ->
        push_hole (Metapp_preutils.structure_of_payload payload)
          (fun mapper -> mapper.structure) holes.structure
          Metapp_api.holes_name.structure
    | Pexp_extension ({ txt = "stri"; _ }, payload) ->
        push_hole (Metapp_preutils.structure_item_of_payload payload)
          (fun mapper -> mapper.structure_item) holes.structure_item
          Metapp_api.holes_name.structure_item
    | _ -> Ast_mapper.default_mapper.expr mapper e in
  { Ast_mapper.default_mapper with expr }

and extract_holes () : Ast_mapper.mapper * (unit -> AccuTypes.escape) =
  let accu = ref [] in
  let push (instruction : instruction Location.loc) : unit =
    Metapp_preutils.mutate (List.cons instruction) accu in
  let holes = MutableContext.make () in
  let sub_holes = MutableHoles.make () in
  let push_hole (field : string)
      (holes_accu : _ AccuTypes.context)
      (payload : Parsetree.payload) : Parsetree.extension =
    let e = Metapp_preutils.expression_of_payload payload in
    let mapper = extract_subexpressions sub_holes in
    let extracted_expr = mapper.expr mapper e in
    let index =
      Metapp_preutils.update (Accu.add !Ast_helper.default_loc) holes_accu in
    let hole_field = field_get (context_get holes_field) field in
    let extracted_expr =
      Metapp_preutils.Exp.some
        (Metapp_preutils.apply
          (Metapp_preutils.ident
            (Ldot (Lident "Ast_helper", "with_default_loc")))
          [array_get (field_get (context_get loc_field) field) index;
            Ast_helper.Exp.function_
              [Ast_helper.Exp.case (Metapp_preutils.Pat.of_unit ())
                extracted_expr]]) in
    push (Metapp_preutils.mkloc (Exp
      (array_set hole_field index extracted_expr)));
    extension_of_index index in
  let push_instruction (instruction : instruction Location.loc) : unit =
    let instruction =
      match instruction.txt with
      | Definition structure ->
          let mapper = extract_subexpressions sub_holes in
          { instruction with txt =
            Definition (mapper.structure mapper structure) }
      | _ -> instruction in
    push instruction in
  let expr (payload : Parsetree.payload) : Parsetree.expression =
    Ast_helper.Exp.extension
      (push_hole Metapp_api.holes_name.expr holes.expr payload) in
  let pat (payload : Parsetree.payload) : Parsetree.pattern =
    Ast_helper.Pat.extension
      (push_hole Metapp_api.holes_name.pat holes.pat payload) in
  let typ (payload : Parsetree.payload) : Parsetree.core_type =
    Ast_helper.Typ.extension
      (push_hole Metapp_api.holes_name.typ holes.typ payload) in
  let class_type (payload : Parsetree.payload) : Parsetree.class_type =
    Ast_helper.Cty.extension
      (push_hole Metapp_api.holes_name.class_type
        holes.class_type payload) in
  let class_type_field (payload : Parsetree.payload)
      : Parsetree.class_type_field =
    Ast_helper.Ctf.extension
      (push_hole Metapp_api.holes_name.class_type_field
        holes.class_type_field payload) in
  let class_expr (payload : Parsetree.payload) : Parsetree.class_expr =
    Ast_helper.Cl.extension
      (push_hole Metapp_api.holes_name.class_expr
        holes.class_expr payload) in
  let class_field (payload : Parsetree.payload) : Parsetree.class_field =
    Ast_helper.Cf.extension
      (push_hole Metapp_api.holes_name.class_field
        holes.class_field payload) in
  let module_type (payload : Parsetree.payload) : Parsetree.module_type =
    Ast_helper.Mty.extension
      (push_hole Metapp_api.holes_name.module_type
        holes.module_type payload) in
  let module_expr (payload : Parsetree.payload) : Parsetree.module_expr =
    Ast_helper.Mod.extension
      (push_hole Metapp_api.holes_name.module_expr
        holes.module_expr payload) in
  let signature_item (payload : Parsetree.payload) : Parsetree.signature_item =
    Ast_helper.Sig.extension
      (push_hole Metapp_api.holes_name.signature_item
        holes.signature_item payload) in
  let structure_item (payload : Parsetree.payload) : Parsetree.structure_item =
    Ast_helper.Str.extension
      (push_hole Metapp_api.holes_name.structure_item
        holes.structure_item payload) in
  let mapper =
    holes_mapper ~def:push_instruction
      { expr; pat; typ; class_type; class_type_field; class_expr; class_field;
        module_type; module_expr; signature_item; structure_item;
        signature = unhandled_hole; structure = unhandled_hole } in
  let k () : AccuTypes.escape = {
    instructions = List.rev !accu;
    context = {
      holes = unmut_context holes;
      loc = unmut_loc holes;
      sub_holes = unmut_subholes sub_holes; }} in
  (mapper, k)

let output_structure (channel : out_channel) (s : Parsetree.structure) =
  let fmt = Format.formatter_of_out_channel channel in
  Pprintast.structure fmt s;
  Format.pp_print_flush fmt ()

type compiler = {
    command : string;
    archive_option : string;
    archive_suffix : string;
  }

let compiler : compiler =
  if Dynlink.is_native then {
    command = "ocamlopt";
    archive_option = "-shared";
    archive_suffix = ".cmxs";
  }
  else {
    command = "ocamlc";
    archive_option = "-a";
    archive_suffix = ".cma";
  }

type options = {
    packages : string list;
    directories : string list;
    flags : string list;
    plainsource : bool;
  }

let empty_options = {
  packages = [];
  directories = [];
  flags = [];
  plainsource = false;
}

let rev_options ({ packages; directories; flags; plainsource } : options) = {
  packages = List.rev packages;
  directories = List.rev directories;
  flags = List.rev flags;
  plainsource;
}

let compile (options : options) (source_filename : string)
    (object_filename : string) : unit =
  let flags =
    options.flags @
    Stdcompat.List.concat_map (fun directory -> ["-I"; directory])
      options.directories @
    ["-I"; "+compiler-libs"; "-w"; "-40"; compiler.archive_option;
      source_filename; "-o"; object_filename] in
  let preutils_cmi = "metapp_preutils.cmi" in
  let api_cmi = "metapp_api.cmi" in
  let dune_preutils_path = "preutils/.metapp_preutils.objs/byte/" in
  let dune_api_path = "api/.metapp_api.objs/byte/" in
  let (flags, packages) =
    if Sys.file_exists preutils_cmi && Sys.file_exists api_cmi then
      (flags, options.packages)
    else if Sys.file_exists (Filename.concat dune_preutils_path preutils_cmi) &&
      Sys.file_exists (Filename.concat dune_api_path api_cmi) then
      (["-I"; dune_preutils_path; "-I"; dune_api_path] @ flags,
        options.packages)
    else
      (flags, ["metapp.preutils"; "metapp.api"] @ options.packages) in
  let commands =
    match packages with
    | [] ->
        [(compiler.command ^ ".opt", flags); (compiler.command, flags)]
    | _ ->
        [("ocamlfind",
          [compiler.command; "-package"; String.concat "," packages] @
          flags)] in
  let rec try_commands list =
    match list with
    | [] -> assert false
    | (command, args) :: tl ->
        let command_line = Stdcompat.Filename.quote_command command args in
        match Sys.command command_line with
        | 0 -> ()
        | 127 when tl <> [] -> try_commands tl
        | exit_code ->
            Location.raise_errorf ~loc:!Ast_helper.default_loc
              "@[Unable@ to@ compile@ preprocessor:@ command-line@ \"%s\"@ \
                failed@ with@ exit-code@ %d@]@."
              (String.escaped command_line) exit_code in
  try_commands commands

(* Code taken from pparse.ml (adapted for a channel instead of a filename to use
   open_temp_file), because Pparse.write_ast is introduced in OCaml 4.04.0. *)
let write_ast (plainsource : bool) (channel : out_channel)
    (structure : Parsetree.structure) : unit =
  if plainsource then
    Format.fprintf (Format.formatter_of_out_channel channel)
      "%a@." Pprintast.structure structure
  else
    begin
      output_string channel Config.ast_impl_magic_number;
      output_value channel !Location.input_name;
      output_value channel structure
    end

let compile_and_load (options : options) (structure : Parsetree.structure)
  : unit =
  let (source_filename, channel) = Filename.open_temp_file "metapp" ".ml" in
  Stdcompat.Fun.protect (fun () ->
    Stdcompat.Fun.protect (fun () ->
      write_ast options.plainsource channel structure)
      ~finally:(fun () -> close_out channel);
    let object_filename =
      Stdcompat.Filename.remove_extension source_filename ^
      compiler.archive_suffix in
    compile options source_filename object_filename;
    Stdcompat.Fun.protect (fun () -> Dynlink.loadfile object_filename)
      ~finally:(fun () -> Sys.remove object_filename))
    ~finally:(fun () -> Sys.remove source_filename)

let structure (_mapper : Ast_mapper.mapper) (s : Parsetree.structure)
    : Parsetree.structure =
  let (mapper, k) = extract_holes () in
  let s = mapper.structure mapper s in
  match k () with { instructions; context } ->
  Metapp_api.top_context := Some context;
  let initial_parsetree =
    [Ast_helper.Str.value Nonrecursive
      [Ast_helper.Vb.mk (Metapp_preutils.Pat.var context_var)
        (Ast_helper.Exp.match_ (deref (Metapp_preutils.ident
          (Ldot (metapp_api, "top_context"))))
          [Ast_helper.Exp.case (Metapp_preutils.Pat.none ())
            (Ast_helper.Exp.assert_ (Metapp_preutils.Exp.of_bool false));
            Ast_helper.Exp.case
              (Metapp_preutils.Pat.some (Metapp_preutils.Pat.var context_var))
              (Metapp_preutils.Exp.var context_var)])]] in
  let make_instruction
    ((accu_parsetree : Parsetree.structure), (accu_options : options))
    (instruction : instruction Location.loc)
      : Parsetree.structure * options =
    match instruction.txt with
    | Exp expr ->
        let item =
          Ast_helper.Str.value Nonrecursive
            [Ast_helper.Vb.mk (Metapp_preutils.Pat.of_unit ()) expr] in
        (item :: accu_parsetree, accu_options)
    | Definition definition ->
        (List.rev_append definition accu_parsetree, accu_options)
    | Package package ->
        (accu_parsetree,
          { accu_options with packages = package :: accu_options.packages })
    | Load object_file ->
        Dynlink.loadfile object_file;
        let dir_name = Filename.dirname object_file in
        let accu_options =
          if dir_name = Filename.current_dir_name ||
              List.mem dir_name accu_options.directories then
            accu_options
          else
            { accu_options with
              directories = dir_name :: accu_options.directories } in
        (accu_parsetree, accu_options)
    | Directory directory ->
        (accu_parsetree,
          { accu_options with
            directories = directory :: accu_options.directories })
    | Flags flags ->
        (accu_parsetree,
          { accu_options with
            flags = List.rev_append flags accu_options.flags })
    | Plainsource ->
        (accu_parsetree,
          { accu_options with
            plainsource = true }) in
  let (accu_parsetree, accu_options) =
    List.fold_left make_instruction (initial_parsetree, empty_options)
      instructions in
  let parsetree = List.rev accu_parsetree in
  let options = rev_options accu_options in
  if options.packages <> [] then
    begin
      Findlib.init ();
      Fl_dynload.load_packages options.packages;
    end;
  compile_and_load options parsetree;
  let mapper = replace_holes context.holes in
  mapper.structure mapper s

let mapper : Ast_mapper.mapper =
  { Ast_mapper.default_mapper with structure }

let rewriter _config _cookies : Ast_mapper.mapper =
  mapper

let () =
  Migrate_parsetree.Driver.register ~name:"conditional"
    (module Migrate_parsetree.OCaml_current)
    rewriter
