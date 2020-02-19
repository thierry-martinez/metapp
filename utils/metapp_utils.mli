(** {1 Coercions} *)

val int_of_expression : Parsetree.expression -> int

val string_of_expression : Parsetree.expression -> string

val string_of_arbitrary_expression : Parsetree.expression -> string

val bool_of_expression : Parsetree.expression -> bool

val pair_of_expression :
    Parsetree.expression -> Parsetree.expression * Parsetree.expression

val list_of_expression : Parsetree.expression -> Parsetree.expression list

val list_of_tuple : Parsetree.expression -> Parsetree.expression list

val structure_of_expression : Parsetree.expression -> Parsetree.structure

val include_signature : Parsetree.signature -> Parsetree.signature_item

val include_structure : Parsetree.structure -> Parsetree.structure_item

val lid_of_str : Ast_helper.str -> Ast_helper.lid

val sequence : Parsetree.expression list -> Parsetree.expression

(** {1 Payload construction and extraction} *)

val expression_of_payload : Parsetree.payload -> Parsetree.expression

val payload_of_expression : Parsetree.expression -> Parsetree.payload

val pattern_of_payload : Parsetree.payload -> Parsetree.pattern

val structure_of_payload : Parsetree.payload -> Parsetree.structure

val structure_item_of_payload : Parsetree.payload -> Parsetree.structure_item

val signature_of_payload : Parsetree.payload -> Parsetree.signature

val signature_item_of_payload : Parsetree.payload -> Parsetree.signature_item

val core_type_of_payload : Parsetree.payload -> Parsetree.core_type

val int_of_payload : Parsetree.payload -> int

val payload_of_int : int -> Parsetree.payload

val string_of_payload : Parsetree.payload -> string

val bool_of_payload : Parsetree.payload -> bool

(** {1 Location management} *)

val mkloc : 'a -> 'a Location.loc

val map_loc : ('a -> 'b) -> 'a Location.loc -> 'b Location.loc

val with_loc : ('a -> 'b) -> 'a Location.loc -> 'b

(** {1 Constructing identifiers } *)

val make_ident : ?prefix : Longident.t -> string -> Longident.t

val ident : ?attrs : Parsetree.attributes -> Longident.t -> Parsetree.expression

(** {1 Constructing function application} *)

val nolabel : 'a -> Asttypes.arg_label * 'a

val nolabels : 'a list -> (Asttypes.arg_label * 'a) list

val apply :
    ?attrs : Parsetree.attributes -> Parsetree.expression ->
      ?labels : (string * Parsetree.expression) list ->
        Parsetree.expression list -> Parsetree.expression

(** {1 Generic signature for expressions and patterns} *)

type 'a mapper_item = Ast_mapper.mapper -> 'a -> 'a

module type ValueS = sig
  type t

  val to_loc : t -> Location.t

  val var : ?attrs:Parsetree.attributes -> string -> t

  val of_constant : ?attrs:Parsetree.attributes -> Parsetree.constant -> t

  val of_bytes : ?attrs:Parsetree.attributes -> bytes -> t

  val force_tuple : ?attrs:Parsetree.attributes -> t list -> t

  val force_construct :
      ?attrs:Parsetree.attributes -> Ast_helper.lid -> t option -> t

  val array : ?attrs:Parsetree.attributes -> t list -> t

  val record : ?attrs:Parsetree.attributes -> (Longident.t * t) list -> t

  val variant : ?attrs:Parsetree.attributes -> string -> t option -> t

  val lazy_ :  ?attrs:Parsetree.attributes -> t -> t

  val choice :
      (unit -> Parsetree.expression) -> (unit -> Parsetree.pattern) -> t

  val mapper : Ast_mapper.mapper -> t mapper_item

  val set_mapper : t mapper_item -> Ast_mapper.mapper -> Ast_mapper.mapper

  val extension : ?attrs:Parsetree.attributes -> Parsetree.extension -> t

  val destruct_extension : t -> Parsetree.extension option

  val of_payload : Parsetree.payload -> t

  val of_int : ?attrs:Parsetree.attributes -> int -> t

  val of_string : ?attrs:Parsetree.attributes -> string -> t

  val of_char : ?attrs:Parsetree.attributes -> char -> t

  val of_unit : ?attrs:Parsetree.attributes -> unit -> t

  val of_bool : ?attrs:Parsetree.attributes -> bool -> t

  val of_float : ?attrs:Parsetree.attributes -> float -> t

  val of_int32 : ?attrs:Parsetree.attributes -> int32 -> t

  val of_int64 : ?attrs:Parsetree.attributes -> int64 -> t

  val of_nativeint : ?attrs:Parsetree.attributes -> nativeint -> t

  val none : ?attrs:Parsetree.attributes -> unit -> t

  val some : ?attrs:Parsetree.attributes -> t -> t

  val option : ?attrs:Parsetree.attributes -> t option -> t

  val of_longident : Longident.t -> t

  val construct : ?attrs:Parsetree.attributes -> Longident.t -> t list -> t

  val tuple : ?attrs:Parsetree.attributes -> t list -> t

  val nil : ?attrs:Parsetree.attributes -> ?prefix:Longident.t -> unit -> t

  val cons : ?attrs:Parsetree.attributes -> ?prefix:Longident.t -> t -> t -> t

  val list :
      ?attrs:Parsetree.attributes -> ?prefix:Longident.t -> t list -> t
end

module Exp : ValueS with type t = Parsetree.expression

module Pat : ValueS with type t = Parsetree.pattern

type value = {
    exp : Parsetree.expression;
    pat : Parsetree.pattern;
  }

module Value : ValueS with type t = value

(** {1 Attribute management} *)

module Attr : sig
  val mk : Ast_helper.str -> Parsetree.payload -> Parsetree.attribute

  val name : Parsetree.attribute -> Ast_helper.str

  val payload : Parsetree.attribute -> Parsetree.payload

  val to_loc : Parsetree.attribute -> Location.t

  val find : string -> Parsetree.attributes -> Parsetree.attribute option

  val chop :
      string -> Parsetree.attributes ->
        (Parsetree.attribute * Parsetree.attributes) option
end

(** {1 Module binding and declaration} *)

module Md : sig
  val mk :
      string option Location.loc -> Parsetree.module_type ->
        Parsetree.module_declaration
end

module Mb : sig
  val mk :
      string option Location.loc -> Parsetree.module_expr ->
        Parsetree.module_binding
end

(** {1 Signature type destruction} *)

[%%meta if Sys.ocaml_version >= "4.08.0" then
  [%sigi: type sig_type = {
    id : Ident.t;
    decl : Types.type_declaration;
    rec_status : Types.rec_status;
    visibility : Types.visibility;
  }]
else
  [%sigi: type sig_type = {
    id : Ident.t;
    decl : Types.type_declaration;
    rec_status : Types.rec_status;
  }]]

val destruct_sig_type : Types.signature_item -> sig_type option

(** {1 Mapper for [[@when bool]] notation} *)

val filter : Ast_mapper.mapper

(** {1 Type construction} *)

module Typ : sig
  val poly : string list -> Parsetree.core_type -> Parsetree.core_type
end

(** {1 General purpose functions} *)

val update : ('a -> 'b * 'a) ->  'a ref -> 'b

val mutate : ('a -> 'a) -> 'a ref -> unit

val extract_first : ('a -> 'b option) -> 'a list -> ('b * 'a list) option
