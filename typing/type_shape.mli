module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

module Type_shape : sig
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

    val unboxed_type_to_layout : unboxed -> Jkind_types.Sort.base

    val predef_to_layout : t -> Layout.t
  end

  type without_layout

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

  type 'a t =
    | Ts_constr of (Uid.t * Path.t * 'a) * without_layout t list
    | Ts_tuple of 'a t list
    | Ts_unboxed_tuple of 'a t list
    | Ts_var of string option * 'a
    | Ts_predef of Predef.t * without_layout t list
    | Ts_arrow of without_layout t * without_layout t
    | Ts_variant of 'a t poly_variant_constructor list * poly_variant_kind
    | Ts_other of 'a

  val shape_layout : Layout.t t -> Layout.t

  val shape_with_layout : layout:Layout.t -> without_layout t -> Layout.t t

  module With_layout : sig
    include Identifiable.S with type t := Jkind_types.Sort.Const.t t
  end
end

module Type_decl_shape : sig
  type 'a complex_constructor =
    { name : string;
      kind : Types.constructor_representation;
      args : 'a complex_constructor_arguments list
    }

  and 'a complex_constructor_arguments =
    { field_name : string option;
      field_value : 'a
    }

  val complex_constructor_map :
    ('a -> 'b) -> 'a complex_constructor -> 'b complex_constructor

  type record_kind =
    | Record_unboxed
        (** [Record_unboxed] is the case for single-field records declared with
            [@@unboxed], whose runtime representation is simply its contents
            without any indirection. *)
    | Record_unboxed_product
        (** [Record_unboxed_product] is the truly unboxed record that corresponds to
            [#{ ... }]. *)
    | Record_boxed
    | Record_mixed of Types.mixed_product_shape
    | Record_floats
        (** Basically the same as [Record_mixed], but we don't reorder the fields. *)

  (** For type substitution to work as expected, we store the layouts in the declaration
     alongside the shapes instead of directly going for the substituted version. *)
  type tds =
    | Tds_variant of
        { simple_constructors : string list;
              (** The string is the name of the constructor. The runtime
                  representation of the constructor at index [i] in this list is
                  [2 * i + 1]. See [dwarf_type.ml] for more details. *)
          complex_constructors :
            (Type_shape.without_layout Type_shape.t * Layout.t)
            complex_constructor
            list
              (** All constructors in this category are represented as blocks.
                  The index [i] in the list indicates the tag at runtime. The
                  length of the constructor argument list [args] determines the
                  size of the block. *)
        }
    | Tds_variant_unboxed of
        { name : string;
          arg_name : string option;
              (** if this is [None], we are looking at a singleton tuple;
                otherwise, it is a singleton record. *)
          arg_shape : Type_shape.without_layout Type_shape.t;
          arg_layout : Layout.t
        }
        (** An unboxed variant corresponds to the [@@unboxed] annotation.
          It must have a single, complex constructor. *)
    | Tds_record of
        { fields :
            (string * Type_shape.without_layout Type_shape.t * Layout.t) list;
          kind : record_kind
        }
    | Tds_alias of Type_shape.without_layout Type_shape.t
    | Tds_other

  type t =
    { path : Path.t;
      definition : tds;
      type_params : Type_shape.without_layout Type_shape.t list
    }

  val print : Format.formatter -> t -> unit

  val replace_tvar : t -> Type_shape.without_layout Type_shape.t list -> t
end

val all_type_decls : Type_decl_shape.t Uid.Tbl.t

val all_type_shapes : Layout.t Type_shape.t Uid.Tbl.t

(* Passing [Path.t -> Uid.t] instead of [Env.t] to avoid a dependency cycle. *)
val add_to_type_decls :
  Path.t -> Types.type_declaration -> (Path.t -> Uid.t option) -> unit

val add_to_type_shapes :
  Uid.t ->
  Types.type_expr ->
  Jkind_types.Sort.Const.t ->
  (Path.t -> Uid.t option) ->
  unit

val find_in_type_decls : Uid.t -> Type_decl_shape.t option

val type_name : 'a. 'a Type_shape.t -> string

val print_table_all_type_decls : Format.formatter -> unit

val print_table_all_type_shapes : Format.formatter -> unit
