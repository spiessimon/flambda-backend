(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Ulysse Gérard, Thomas Refis, Tarides                    *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Shapes are an abstract representation of modules' implementations which
    allow the tracking of definitions through functor applications and other
    module-level operations.

    The Shape of a compilation unit is elaborated during typing, partially
    reduced (without loading external shapes) and written to the [cmt] file.

    External tools can retrieve the definition of any value (or type, or module,
    etc) by following this procedure:

    - Build the Shape corresponding to the value's path:
      [let shape = Env.shape_of_path ~namespace env path]

    - Instantiate the [Shape_reduce.Make] functor with a way to load shapes from
      external units and to looks for shapes in the environment (usually using
      [Env.shape_of_path]).

    - Completely reduce the shape:
      [let shape = My_reduce.(weak_)reduce env shape]

    - The [Uid.t] stored in the reduced shape should be the one of the
      definition. However, if the [approximate] field of the reduced shape is
      [true] then the [Uid.t] will not correspond to the definition, but to the
      closest parent module's uid. This happens when Shape reduction gets stuck,
      for example when hitting first-class modules.

    - The location of the definition can be easily found with the
      [cmt_format.cmt_uid_to_decl] table of the corresponding compilation unit.

  See:
  - {{:https://icfp22.sigplan.org/details/mlfamilyworkshop-2022-papers/10/Module-Shapes-for-Modern-Tooling}
    the design document}
  - {{:https://www.lix.polytechnique.fr/Labo/Gabriel.Scherer/research/shapes/2022-ml-workshop-shapes-talk.pdf}
    a talk about the reduction strategy
*)

(** A [Uid.t] is associated to every declaration in signatures and
    implementations. They uniquely identify bindings in the program. When
    associated with these bindings' locations they are useful to external tools
    when trying to jump to an identifier's declaration or definition. They are
    stored to that effect in the [uid_to_decl] table of cmt files. *)
module Uid : sig
  type t = private
    | Compilation_unit of string
    | Item of {
        comp_unit: string;
        id: int;
        from: Unit_info.intf_or_impl }
    | Internal
    | Predef of string
    | Unboxed_version of t

  val reinit : unit -> unit

  val mk : current_unit:Unit_info.t option -> t
  val of_compilation_unit_id : Compilation_unit.t -> t
  val of_compilation_unit_name : Compilation_unit.Name.t -> t
  val of_predef_id : Ident.t -> t
  val internal_not_actually_unique : t
  val unboxed_version : t -> t

  val for_actual_declaration : t -> bool

  include Identifiable.S with type t := t
end

module Sig_component_kind : sig
  type t =
    | Value
    | Type
    | Constructor
    | Label
    | Unboxed_label
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  val to_string : t -> string

  (** Whether the name of a component of that kind can appear in a type. *)
  val can_appear_in_types : t -> bool
end

(** Shape's items are elements of a structure or, in the case of constructors
  and labels, elements of a record or variants definition seen as a structure.
  These structures model module components and nested types' constructors and
  labels. *)
module Item : sig
  type t = string * Sig_component_kind.t
  val name : t -> string
  val kind : t -> Sig_component_kind.t

  val make : string -> Sig_component_kind.t -> t

  val value : Ident.t -> t
  val type_ : Ident.t -> t
  val constr : Ident.t -> t
  val label : Ident.t -> t
  val unboxed_label : Ident.t -> t
  val module_ : Ident.t -> t
  val module_type : Ident.t -> t
  val extension_constructor : Ident.t -> t
  val class_ : Ident.t -> t
  val class_type : Ident.t -> t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  module Map : Map.S with type key = t
end


module Layout = Jkind_types.Sort.Const
type base_layout = Jkind_types.Sort.base


module Predef : sig
  type simd_vec_split =
    (* 128 bit *)
    | Int8x16
    | Int16x8
    | Int32x4
    | Int64x2
    | Float32x4
    | Float64x2
    (* 256 bit *)
    | Int8x32
    | Int16x16
    | Int32x8
    | Int64x4
    | Float32x8
    | Float64x4
    (* 512 bit *)
    | Int8x64
    | Int16x32
    | Int32x16
    | Int64x8
    | Float32x16
    | Float64x8

  type unboxed =
    | Unboxed_float
    | Unboxed_float32
    | Unboxed_nativeint
    | Unboxed_int64
    | Unboxed_int32
    | Unboxed_simd of simd_vec_split

  type t =
    | Array
    | Bytes
    | Char
    | Extension_constructor
    | Float
    | Float32
    | Floatarray
    | Int
    | Int32
    | Int64
    | Lazy_t
    | Nativeint
    | String
    | Simd of simd_vec_split
    | Exception
    | Unboxed of unboxed

  val to_string : t -> string

  val of_string : string -> t option

  val unboxed_type_to_base_layout : unboxed -> base_layout

  val predef_to_layout : t -> Layout.t
end



(* Unlike in [types.ml], we use [base_layout] entries here, because we can
   represent flattened floats simply as float64 in the debugger.  *)
type mixed_product_shape = base_layout array

and constructor_representation =
  | Constructor_uniform_value
  | Constructor_mixed of mixed_product_shape


type 'a complex_constructor =
  { name : string;
    kind : constructor_representation;
    args : 'a complex_constructor_arguments list
  }

and 'a complex_constructor_arguments =
  { field_name : string option;
    field_value : 'a
  }


type record_kind =
  | Record_unboxed
      (** [Record_unboxed] is the case for single-field records declared with
          [@@unboxed], whose runtime representation is simply its contents
          without any indirection. *)
  | Record_unboxed_product
      (** [Record_unboxed_product] is the truly unboxed record that corresponds to
          [#{ ... }]. *)
  | Record_boxed
  | Record_mixed of mixed_product_shape
  | Record_floats
      (** Basically the same as [Record_mixed], but we don't reorder the fields. *)

type without_layout = Layout_to_be_determined

type 'a poly_variant_constructor =
  { pv_constr_name : string;
    pv_constr_args : 'a list
  }

(* CR sspies: This is incorrect. We should follow the printing code in [printtyp.ml]
    for determining which kind of variant we are looking at. For the intersections, we
    also have to add the boolean to indicate that the type constructor could be intersected
    with one that has no type argument. *)
type poly_variant_kind =
  | Open
  | Closed

type var = Ident.t
type t = private { hash: int; uid: Uid.t option; desc: desc; approximated: bool }
and desc =
  | Var of var
  | Abs of var * t
  | App of t * t
  | Struct of t Item.Map.t
  | Alias of t
  | Leaf
  | Type_decl of tds
  | Proj of t * Item.t
  | Comp_unit of string
  | Error of string

(** For type substitution to work as expected, we store the layouts in the declaration
    alongside the shapes instead of directly going for the substituted version. *)
and tds_desc =
  | Tds_variant of
      { simple_constructors : string list;
            (** The string is the name of the constructor. The runtime representation of
              the constructor at index [i] in this list is [2 * i + 1]. See
              [dwarf_type.ml] for more details. *)
        complex_constructors :
          (without_layout ts * Layout.t)
          complex_constructor
          list
            (** All constructors in this category are represented as blocks. The index [i]
              in the list indicates the tag at runtime. The length of the constructor
              argument list [args] determines the size of the block. *)
      }
      (** Note that this variant representation split up variants into immediates
        (simple constructors) and blocks (complex constructors). Thus, even though the
        order is disturbed by separating them into two lists, the runtime shape is still
        uniquely determined, because the two representations are disjoint. *)
  | Tds_variant_unboxed of
      { name : string;
        arg_name : string option;
            (** if this is [None], we are looking at a singleton tuple;
              otherwise, it is a singleton record. *)
        arg_shape : without_layout ts;
        arg_layout : Layout.t
      }
      (** An unboxed variant corresponds to the [@@unboxed] annotation.
        It must have a single, complex constructor. *)
  | Tds_record of
      { fields :
          (string * without_layout ts * Layout.t) list;
        kind : record_kind
      }
  | Tds_alias of without_layout ts
  | Tds_other
and tds =
  {
    definition : tds_desc;
    type_params : without_layout ts list
  }

and 'a ts =
  | Ts_constr of (Uid.t * t * 'a) * without_layout ts list
  | Ts_tuple of 'a ts list
  | Ts_unboxed_tuple of 'a ts list
  | Ts_var of string option * 'a
  | Ts_predef of Predef.t * without_layout ts list
  | Ts_arrow of without_layout ts * without_layout ts
  | Ts_variant of 'a ts poly_variant_constructor list * poly_variant_kind
  | Ts_other of 'a


val print : Format.formatter -> t -> unit

val print_tds : Format.formatter -> tds -> unit

val print_ts : Format.formatter -> 'a ts -> unit

val strip_head_aliases : t -> t

val equal : t -> t -> bool
val equal_tds : tds -> tds -> bool
val equal_ts : ('a -> 'a -> bool) -> 'a ts -> 'a ts -> bool


val complex_constructor_map : ('a -> 'b) -> 'a complex_constructor -> 'b complex_constructor
val shape_layout : Layout.t ts -> Layout.t
val shape_with_layout : layout:Layout.t -> without_layout ts -> Layout.t ts


(* Smart constructors *)

val for_unnamed_functor_param : var
val fresh_var : ?name:string -> Uid.t -> var * t

val var : Uid.t -> Ident.t -> t
val abs : ?uid:Uid.t -> var -> t -> t
val app : ?uid:Uid.t -> t -> arg:t -> t
val str : ?uid:Uid.t -> t Item.Map.t -> t
val alias : ?uid:Uid.t -> t -> t
val error : ?uid:Uid.t -> string -> t
val proj : ?uid:Uid.t -> t -> Item.t -> t
val leaf : Uid.t -> t
val leaf' : Uid.t option -> t
val type_decl : Uid.t option -> tds -> t
val no_fuel_left : ?uid:Uid.t -> t -> t
val comp_unit : ?uid:Uid.t -> string -> t

val set_approximated : approximated:bool -> t -> t

val decompose_abs : t -> (var * t) option

(* CR lmaurer: Should really take a [Compilation_unit.t] *)
val for_persistent_unit : string -> t
val leaf_for_unpack : t

module Map : sig
  type shape = t
  type nonrec t = t Item.Map.t

  val empty : t

  val print: (shape -> shape) -> Format.formatter -> t -> unit
  (* CR sspies: Remove this again. Helpful for debugging. *)

  val add : t -> Item.t -> shape -> t

  val add_value : t -> Ident.t -> Uid.t -> t
  val add_value_proj : t -> Ident.t -> shape -> t

  val add_type : t -> Ident.t -> shape -> t
  val add_type_proj : t -> Ident.t -> shape -> t

  val add_constr : t -> Ident.t -> shape -> t
  val add_constr_proj : t -> Ident.t -> shape -> t

  val add_label : t -> Ident.t -> Uid.t -> t
  val add_label_proj : t -> Ident.t -> shape -> t

  val add_unboxed_label : t -> Ident.t -> Uid.t -> t
  val add_unboxed_label_proj : t -> Ident.t -> shape -> t

  val add_module : t -> Ident.t -> shape -> t
  val add_module_proj : t -> Ident.t -> shape -> t

  val add_module_type : t -> Ident.t -> Uid.t -> t
  val add_module_type_proj : t -> Ident.t -> shape -> t

  val add_extcons : t -> Ident.t -> shape -> t
  val add_extcons_proj : t -> Ident.t -> shape -> t

  val add_class : t -> Ident.t -> Uid.t -> t
  val add_class_proj : t -> Ident.t -> shape -> t

  val add_class_type : t -> Ident.t -> Uid.t -> t
  val add_class_type_proj : t -> Ident.t -> shape -> t
end

val dummy_mod : t

(** This function returns the shape corresponding to a given path. It requires a
    callback to find shapes in the environment. It is generally more useful to
    rely directly on the [Env.shape_of_path] function to get the shape
    associated with a given path. *)
val of_path :
  find_shape:(Sig_component_kind.t -> Ident.t -> t) ->
  namespace:Sig_component_kind.t -> Path.t -> t

val set_uid_if_none : t -> Uid.t -> t
