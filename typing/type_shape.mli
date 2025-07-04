module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

type base_layout = Jkind_types.Sort.base

module Type_shape : sig

  val of_type_expr :
    Types.type_expr ->
    (Path.t -> Uid.t option) ->
    Shape.without_layout Shape.ts

end

module Type_decl_shape : sig

  val of_type_declaration :
    Path.t ->
    Types.type_declaration ->
    (Path.t -> Uid.t option) ->
    Shape.tds

end

type type_shape_with_name =
  { type_shape : Layout.t Shape.ts;
    type_name : string
  }

val all_type_decls : Shape.tds Uid.Tbl.t

val all_type_shapes : type_shape_with_name Uid.Tbl.t

(* Passing [Path.t -> Uid.t] instead of [Env.t] to avoid a dependency cycle. *)
val add_to_type_decls :
  Path.t -> Types.type_declaration -> (Path.t -> Uid.t option) -> unit

val add_to_type_shapes :
  Uid.t ->
  Types.type_expr ->
  Jkind_types.Sort.Const.t ->
  name:string ->
  (Path.t -> Uid.t option) ->
  unit

val find_in_type_decls : Uid.t -> Shape.tds option

val print_table_all_type_decls : Format.formatter -> unit

val print_table_all_type_shapes : Format.formatter -> unit
