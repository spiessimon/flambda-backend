(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Augmented version of [Shape.Uid.t] that can track variables forming parts
    of unboxed products. This Uid is used for tracking debugging information
    about variables from the typed tree down to the lower level intermeditate
    representations. Warning: These identifiers are not actually unique; see
    [none] below. *)

type t = private
  | Uid of Shape.Uid.t
  | Proj of Shape.Uid.t * int

(* Use this UID when the correct uid is either unknown or does not exist at the
   source level. Since several places in the compiler use [none], the uids
   introduced here are not actually unique. *)
val none : t

val uid : Shape.Uid.t -> t

val proj : Shape.Uid.t -> field:int -> t

include Identifiable.S with type t := t
