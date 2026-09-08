(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                   Miriam Vellacott, Jane Street Europe                 *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

<<<<<<< HEAD
module Serialisable_solution : sig
  type t

  val deserialise : t -> Unboxing_analysis.result
end

module File_contents : sig
  type t =
    { id_stamp_counters : Id_stamp_counters.t;
      solution : Serialisable_solution.t
    }
end
||||||| parent of 64ed0bfb00 (file sections for ltosol)
module Serialisable_solution : sig
  type t

  val deserialise : t -> Unboxing_analysis.result
end

module File_contents : sig
  type t =
    { id_stamp_counters : Id_stamp_counters.t;
      participants : Compilation_unit.t list;
      solution : Serialisable_solution.t
    }
end
=======
(** An .ltosol file whose header has been read. The solution itself is stored in
    one file section per compilation unit and is only read by
    [solution_for_members]. *)
type t
>>>>>>> 64ed0bfb00 (file sections for ltosol)

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

<<<<<<< HEAD
(** Write an .ltosol file with the given solution to disk. *)
val save : filename:string -> solution:Unboxing_analysis.result -> unit
||||||| parent of 64ed0bfb00 (file sections for ltosol)
(** Write an .ltosol file with the given solution to disk. [participants] should
    list the compilation units included in the solution. *)
val save :
  filename:string ->
  participants:Compilation_unit.t list ->
  solution:Unboxing_analysis.result ->
  unit
=======
(** Write an .ltosol file with the given solution to disk, sharded into one file
    section per compilation unit. [participants] should list the compilation
    units included in the solution, each paired with the compilation units its
    dependency graph references; these determine which sections the unit's
    rebuild will need to read. *)
val save :
  filename:string ->
  participants:(Compilation_unit.t * Compilation_unit.Set.t) list ->
  solution:Unboxing_analysis.result ->
  unit
>>>>>>> 64ed0bfb00 (file sections for ltosol)

(** Read the header of an ltosol file from disk. *)
val load : string -> t

val id_stamp_counters : t -> Id_stamp_counters.t

val participants : t -> Compilation_unit.t list

(** Deserialise the solution needed to rebuild [members], inserting the
    necessary objects into the global hashcons tables. *)
val solution_for_members :
  t -> members:Compilation_unit.t list -> Unboxing_analysis.result
