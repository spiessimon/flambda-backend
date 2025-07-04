(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Ulysse Gérard, Thomas Refis, Tarides                   *)
(*                    Nathanaëlle Courant, OCamlPro                       *)
(*              Gabriel Scherer, projet Picube, INRIA Paris               *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Layout = Jkind_types.Sort.Const

(** The result of reducing a shape and looking for its uid *)
type result =
  | Resolved of Shape.Uid.t (** Shape reduction succeeded and a uid was found *)
  | Resolved_alias of Shape.Uid.t * result (** Reduction led to an alias *)
  | Unresolved of Shape.t (** Result still contains [Comp_unit] terms *)
  | Approximated of Shape.Uid.t option
    (** Reduction failed: it can arrive with first-class modules for example *)
  | Internal_error_missing_uid
    (** Reduction succeeded but no uid was found, this should never happen *)

val print_result : Format.formatter -> result -> unit

(** The [Make] functor is used to generate a reduction function for
    shapes.

    It is parametrized by:
    - a function to load the shape of an external compilation unit
    - some fuel, which is used to bound recursion when dealing with recursive
      shapes introduced by recursive modules. (FTR: merlin currently uses a
      fuel of 10, which seems to be enough for most practical examples)
    - a shape lookup function, which can be used to provide shapes of type
      declarations. Always returning [None] in this function is perfectly fine.
      If shapes are provided for a [Shape.Uid.t], then reduction will insert
      this declaration during reduction and reduce further inside the
      declaration.

    Usage warning: To ensure good performances, every reduction made with the
    same instance of that functor share the same ident-based memoization tables.
    Such an instance should only be used to perform reduction inside a unique
    compilation unit to prevent conflicting entries in these memoization tables.
*)
module Make(_ : sig
    val fuel : int

    val read_unit_shape : unit_name:string -> Shape.t option

    val type_shape_compression : bool

    val lookup_shape_for_uid : Shape.Uid.t -> Shape.t option
    (* CR sspies: In practice, this function depends on global state that is
       augmented whenever we call [read_unit_shape]. An alternative would be
       to make this state passing explicit. *)
  end) : sig
  val reduce : Env.t -> Shape.t -> Shape.t

  val reduce_tds : Env.t -> Shape.tds -> Shape.tds

  val reduce_ts : Env.t -> Shape.without_layout Shape.ts -> Shape.without_layout Shape.ts

  (** Perform weak reduction and return the head's uid if any. If reduction was
    incomplete the partially reduced shape is returned. *)
  val reduce_for_uid : Env.t -> Shape.t -> result
end

(** [local_reduce] will not reduce shapes that require loading external
  compilation units. *)
val local_reduce : Env.t -> Shape.t -> Shape.t

val local_reduce_tds : Env.t -> Shape.tds -> Shape.tds

val local_reduce_ts : Env.t -> Shape.without_layout Shape.ts -> Shape.without_layout Shape.ts

(** [local_reduce_for_uid] will not reduce shapes that require loading external
  compilation units. *)
val local_reduce_for_uid : Env.t -> Shape.t -> result
