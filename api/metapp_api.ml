module type Unary = sig
  type 'a t
end

module type UnaryMake = sig
  type 'a t

  val make : unit -> 'a t
end

module type HolesS = sig
  type 'a x

  type t = {
      expr : Parsetree.expression x;
      pat : Parsetree.pattern x;
      typ : Parsetree.core_type x;
      class_type : Parsetree.class_type x;
      class_type_field : Parsetree.class_type_field x;
      class_expr : Parsetree.class_expr x;
      class_field : Parsetree.class_field x;
      module_type : Parsetree.module_type x;
      module_expr : Parsetree.module_expr x;
      signature : Parsetree.signature x;
      signature_item : Parsetree.signature_item x;
      structure : Parsetree.structure x;
      structure_item : Parsetree.structure_item x;
    }
end

module type HolesMakeS = sig
  include HolesS

  module Make (X : UnaryMake with type 'a t = 'a x) : sig
    val make : unit -> t
  end
end

module type HolesWithMakeS = sig
  include HolesS

  val make : unit -> t
end

module Holes (X : Unary) : HolesMakeS with type 'a x = 'a X.t = struct
  module rec Sub : sig
    include HolesS with type 'a x = 'a X.t
  end = struct
    include Sub
  end

  include Sub

  module Make (X : UnaryMake with type 'a t = 'a X.t) = struct
    let make () = {
      expr = X.make ();
      pat = X.make ();
      typ = X.make ();
      class_type = X.make ();
      class_type_field = X.make ();
      class_expr = X.make ();
      class_field = X.make ();
      module_type = X.make ();
      module_expr = X.make ();
      signature = X.make ();
      signature_item = X.make ();
      structure = X.make ();
      structure_item = X.make ();
    }
  end
end

module HolesName = Holes (struct type 'a t = string end)

let holes_name : HolesName.t =
  {
    expr = "expr";
    pat = "pat";
    typ = "typ";
    class_type = "class_type";
    class_type_field = "class_type_field";
    class_expr = "class_expr";
    class_field = "class_field";
    module_type = "module_type";
    module_expr = "module_expr";
    signature = "signature";
    signature_item = "signature_item";
    structure = "structure";
    structure_item = "structure_item";
  }

module HolesWithMake (X : UnaryMake) : HolesWithMakeS
with type 'a x = 'a X.t = struct
  include Holes (X)

  include Make (X)
end

module type Map = sig
  type 'a x

  type 'a y

  val map : 'a x -> 'a y
end

module HolesMap (X : HolesS) (Y : HolesS)
    (M : Map with type 'a x = 'a X.x and type 'a y = 'a Y.x) = struct
  let map (x : X.t) : Y.t = {
    expr = M.map x.expr;
    pat = M.map x.pat;
    typ = M.map x.typ;
    class_type = M.map x.class_type;
    class_type_field = M.map x.class_type_field;
    class_expr = M.map x.class_expr;
    class_field = M.map x.class_field;
    module_type = M.map x.module_type;
    module_expr = M.map x.module_expr;
    signature = M.map x.signature;
    signature_item = M.map x.signature_item;
    structure = M.map x.structure;
    structure_item = M.map x.structure_item;
  }
end

module OptionArray = struct
  type 'a t = 'a option array
end

module OptionArrayHoles = Holes (OptionArray)

module LocationHole = struct
  type _ t = Location.t array
end

module LocationHoles = Holes (LocationHole)

module rec ArrayHole : sig
  type context = {
      holes : OptionArrayHoles.t;
      loc : LocationHoles.t;
      sub_holes : ArrayHoles.t;
    }

  type 'a hole = {
      context : context;
      fill : unit -> 'a;
    }

  type 'a t = 'a hole array
end = struct
  include ArrayHole
end
and ArrayHoles : HolesS with type 'a x = 'a ArrayHole.t = Holes (ArrayHole)

type context = ArrayHole.context

let top_context : context option ref =
  ref None
