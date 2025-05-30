(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Common functions for emitting assembly code *)

[@@@ocaml.warning "+a-40-41-42"]

val output_channel : out_channel ref

val femit_string : out_channel -> string -> unit

val emit_string : string -> unit

val femit_int : out_channel -> int -> unit

val emit_int : int -> unit

val femit_nativeint : out_channel -> nativeint -> unit

val emit_nativeint : nativeint -> unit

val femit_int32 : out_channel -> int32 -> unit

val emit_int32 : int32 -> unit

val femit_symbol : out_channel -> string -> unit

val emit_symbol : string -> unit

(* CR sspies: use types to distinguish the two string versions *)
val symbol_to_string : string -> string

val emit_printf : ('a, out_channel, unit) format -> 'a

val femit_char : out_channel -> char -> unit

val emit_char : char -> unit

val femit_string_literal : out_channel -> string -> unit

val emit_string_literal : string -> unit

val emit_string_directive : string -> string -> unit

val emit_bytes_directive : string -> string -> unit

val emit_float64_directive : string -> int64 -> unit

val emit_float64_split_directive : string -> int64 -> unit

val emit_float32_directive : string -> int32 -> unit

val reset : unit -> unit

val reset_debug_info : unit -> unit

val femit_debug_info : ?discriminator:int -> out_channel -> Debuginfo.t -> unit

val emit_debug_info : ?discriminator:int -> Debuginfo.t -> unit

val emit_debug_info_gen :
  ?discriminator:int ->
  Debuginfo.t ->
  (file_num:int -> file_name:string -> unit) ->
  (file_num:int -> line:int -> col:int -> ?discriminator:int -> unit -> unit) ->
  unit

(** Get the file number associated with the filename (or allocate one) *)
val get_file_num :
  file_emitter:(file_num:int -> file_name:string -> unit) -> string -> int

type frame_debuginfo =
  | Dbg_alloc of Cmm.alloc_dbginfo
  | Dbg_raise of Debuginfo.t
  | Dbg_other of Debuginfo.t

val record_frame_descr :
  label:Label.t ->
  (* Return address *)
  frame_size:int ->
  (* Size of stack frame *)
  live_offset:int list ->
  (* Offsets/regs of live addresses *)
  frame_debuginfo ->
  (* Location, if any *)
  unit

type emit_frame_actions =
  { efa_code_label : Label.t -> unit;
    efa_data_label : Label.t -> unit;
    efa_8 : int -> unit;
    efa_16 : int -> unit;
    efa_32 : int32 -> unit;
    efa_word : int -> unit;
    efa_align : int -> unit;
    efa_label_rel : Label.t -> int32 -> unit;
    efa_def_label : Label.t -> unit;
    efa_string : string -> unit
  }

val emit_frames : emit_frame_actions -> unit

val is_generic_function : string -> bool

val cfi_startproc : unit -> unit

val cfi_endproc : unit -> unit

val cfi_adjust_cfa_offset : int -> unit

val cfi_offset : reg:int -> offset:int -> unit

val cfi_def_cfa_offset : int -> unit

val cfi_remember_state : unit -> unit

val cfi_restore_state : unit -> unit

val cfi_def_cfa_register : reg:int -> unit

(** Is a binary backend available.  If yes, we don't need
        to generate the textual assembly file (unless the user
        request it with -S). *)
val binary_backend_available : bool ref

(** Clear global state and compact the heap, so that an external program
    (such as the assembler or linker) may have more memory available to it.

    When this frees up around 1.1GB of memory, it takes around 0.6s. We only
    take this time when the job is large enough that we're worried that we'll
    either run out of memory or constrain the number of parallel jobs. We
    heuristically measure how big the job is by how much heap we're using
    ourselves.

    The [reset] parameter will be called before [Gc.compact] if we go ahead
    with the compaction. It should clear as much as possible from the global
    state, since the fewer live words there are after GC, the smaller the new
    heap can be. *)
val reduce_heap_size : reset:(unit -> unit) -> unit

type error =
  | Stack_frame_too_large of int
  | Stack_frame_way_too_large of int
  | Inconsistent_probe_init of string * Debuginfo.t

module Dwarf_helpers : sig
  val init : disable_dwarf:bool -> sourcefile:string option -> unit

  val begin_dwarf :
    code_begin:string ->
    code_end:string ->
    file_emitter:(file_num:int -> file_name:string -> unit) ->
    unit

  val emit_dwarf : unit -> unit

  val emit_delayed_dwarf : unit -> unit

  val record_dwarf_for_fundecl : Linear.fundecl -> Dwarf.fundecl option
end

exception Error of error

val report_error : Format.formatter -> error -> unit

type preproc_stack_check_result =
  { max_frame_size : int;
    contains_nontail_calls : bool
  }

val preproc_stack_check :
  fun_body:Linear.instruction ->
  frame_size:int ->
  trap_size:int ->
  preproc_stack_check_result

val add_stack_checks_if_needed :
  Linear.fundecl ->
  stack_offset:int ->
  stack_threshold_size:int ->
  trap_size:int ->
  Linear.fundecl
