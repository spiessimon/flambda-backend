module Uid = Shape.Uid

module Type_shape : sig

  val of_type_expr : Types.type_expr -> (Path.t -> Uid.t option) -> (Path.t -> Shape.t option) -> Shape.without_layout Shape.ts

  module With_layout : sig
    include Identifiable.S with type t := Shape.Layout.t Shape.ts
  end
end

module Type_decl_shape : sig

  val of_type_declaration : Path.t ->Types.type_declaration -> (Path.t -> Uid.t option) -> (Path.t -> Shape.t option) -> Shape.tds

  val replace_tvar : Shape.tds -> Shape.without_layout Shape.ts list -> Shape.tds
end

val all_type_decls : Shape.tds Uid.Tbl.t

val all_type_shapes : Shape.Layout.t Shape.ts Uid.Tbl.t

(* Passing [Path.t -> Uid.t] instead of [Env.t] to avoid a dependency cycle. *)
val add_to_type_decls :
  Path.t -> Types.type_declaration -> (Path.t -> Uid.t option) -> (Path.t -> Shape.t option) -> unit

val add_to_type_shapes :
  Uid.t ->
  Types.type_expr ->
  Jkind_types.Sort.Const.t ->
  (Path.t -> Uid.t option) ->
  (Path.t -> Shape.t option) ->
  unit

val find_in_type_decls :
  Uid.t ->
  Path.t option ->
  load_decls_from_cms:(string -> Shape.tds Uid.Tbl.t) ->
  Shape.tds option

val type_name :
  'a.
  'a Shape.ts ->
  load_decls_from_cms:(string -> Shape.tds Uid.Tbl.t) ->
  string

val attach_compilation_unit_to_paths :
  Shape.tds -> compilation_unit:Compilation_unit.t -> Shape.tds

val compilation_unit_from_path : Path.t -> string option

val print_table_all_type_decls : Format.formatter -> unit

val print_table_all_type_shapes : Format.formatter -> unit
