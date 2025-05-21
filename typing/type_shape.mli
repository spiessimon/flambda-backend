module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

module Type_shape : sig
  module Predef : sig
    type unboxed =
      | Unboxed_float
      | Unboxed_float32
      | Unboxed_nativeint
      | Unboxed_int64
      | Unboxed_int32

    type t =
      | Array
      | Bytes
      | Char
      | Extension_constructor
      | Float
      | Floatarray
      | Int
      | Int32
      | Int64
      | Lazy_t
      | Nativeint
      | String
      | Unboxed of unboxed

    val to_string : t -> string

    val unboxed_type_to_layout : unboxed -> Jkind_types.Sort.base

    val predef_to_layout : t -> Layout.t
  end

  type without_layout

  type 'a t =
    | Ts_constr of (Uid.t * Path.t * 'a) * without_layout t list
    | Ts_tuple of 'a t list
    | Ts_unboxed_tuple of 'a t list
    | Ts_var of string option * 'a
    | Ts_predef of Predef.t * without_layout t list
    | Ts_arrow of without_layout t * without_layout t
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
        (** Somewhat counterintuitively, [Record_unboxed] is the case for single-field
            records declared with [@@unboxed], whose runtime representation is simply its
            contents without any indirection. *)
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
              (** The string is the name of the constructor. The runtime representation of
                the constructor at index [i] in this list is [2 * i + 1]. See
                [dwarf_type.ml] for more details. *)
          complex_constructors :
            (Type_shape.without_layout Type_shape.t * Layout.t)
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

val find_in_type_decls :
  Uid.t ->
  Path.t ->
  load_decls_from_cms:(string -> Type_decl_shape.t Uid.Tbl.t) ->
  Type_decl_shape.t option

val type_name :
  'a.
  'a Type_shape.t ->
  load_decls_from_cms:(string -> Type_decl_shape.t Uid.Tbl.t) ->
  string

val attach_compilation_unit_to_paths :
  Type_decl_shape.t -> compilation_unit:Compilation_unit.t -> Type_decl_shape.t

val compilation_unit_from_path : Path.t -> string option

val print_table_all_type_decls : Format.formatter -> unit

val print_table_all_type_shapes : Format.formatter -> unit
