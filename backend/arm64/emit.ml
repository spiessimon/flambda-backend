(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-9-40-41-42"]
(* Emission of ARM assembly code, 64-bit mode *)

(* Correctness: carefully consider any use of [Config], [Clflags],
   [Flambda_backend_flags] and shared variables.
   For details, see [asmgen.mli]. *)

(* CR-soon mshinwell/mslater: needs updating for locals + effects *)

open Misc
open Arch
open Proc
open Reg
open Simple_operation
open Linear

module I = Arm64_ast.Instruction_name

(* Basic emitters with output streams *)
let emitp_string out s = output_string out s
let emitp_char out c = output_char out c
let emitp_int out i = output_string out (Int.to_string i)
let emitp_nativeint out i = output_string out (Nativeint.to_string i)

let emitp_int32 out n = Printf.fprintf out "0x%lx" n
let emitp_float64_directive out directive x = Printf.fprintf out "\t%s\t0x%Lx\n" directive x
let emitp_float32_directive out directive x = Printf.fprintf out "\t%s\t0x%lx\n" directive x


let emitp_symbol out s =
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' ->
      output_char out c
    | _ -> Printf.fprintf out "$%02x" (Char.code c)
  done

let emitp_string_literal out s =
  let last_was_escape = ref false in
  emitp_string out "\"";
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c >= '0' && c <= '9'
    then
      if !last_was_escape
      then Printf.fprintf out "\\%o" (Char.code c)
      else output_char out c
    else if c >= ' ' && c <= '~' && c <> '"' (* '"' *) && c <> '\\'
    then (
      output_char out c;
      last_was_escape := false)
    else (
      Printf.fprintf out "\\%o" (Char.code c);
      last_was_escape := true)
  done;
  emitp_string out "\""

let emitp_string_directive out directive s =
  let l = String.length s in
  if l = 0
  then ()
  else if l < 80
  then (
    emitp_string out directive;
    emitp_string_literal out s;
    emitp_char out '\n')
  else
    let i = ref 0 in
    while !i < l do
      let n = min (l - !i) 80 in
      emitp_string out directive;
      emitp_string_literal out (String.sub s !i n);
      emitp_char out '\n';
      i := !i + n
    done


let emitp_format = Printf.fprintf

let emitp_debug_info ?discriminator out dbg =
  ignore discriminator;
  Emitaux.emit_debug_info_gen dbg
    (fun ~file_num ~file_name ->
      emitp_string out "\t.file\t";
      emitp_int out file_num;
      emitp_char out '\t';
      emitp_string_literal out file_name;
      emitp_char out '\n')
    (fun ~file_num ~line ~col:_ ?discriminator () ->
      emitp_string out "\t.loc\t";
      emitp_int out file_num;
      emitp_char out '\t';
      emitp_int out line;
      emitp_char out '\t';
      (match discriminator with
      | None -> ()
      | Some k ->
        emitp_string out "discriminator ";
        emitp_int out k);
      emitp_char out '\n')


let cfi_adjust_cfa_offset out n =
  if Emitaux.is_cfi_enabled ()
  then (
    emitp_string out "\t.cfi_adjust_cfa_offset\t";
    emitp_int out n;
    emitp_string out "\n")

(* let cfi_def_cfa_offset out n =
  if Emitaux.is_cfi_enabled ()
  then (
    emitp_string out "\t.cfi_def_cfa_offset\t";
    emitp_int out n;
    emitp_string out "\n") *)

let cfi_offset out ~reg ~offset =
  if Emitaux.is_cfi_enabled ()
  then (
    emitp_string out "\t.cfi_offset ";
    emitp_int out reg;
    emitp_string out ", ";
    emitp_int out offset;
    emitp_string out "\n")

let cfi_def_cfa_register out ~reg =
  if Emitaux.is_cfi_enabled ()
  then (
    emitp_string out "\t.cfi_def_cfa_register ";
    emitp_int out reg;
    emitp_string out "\n")

let cfi_startproc out () =
  if Emitaux.is_cfi_enabled () then emitp_string out "\t.cfi_startproc\n"

let cfi_endproc out () = if Emitaux.is_cfi_enabled () then emitp_string out "\t.cfi_endproc\n"

let cfi_remember_state out () =
  if Emitaux.is_cfi_enabled () then emitp_string out "\t.cfi_remember_state\n"

let cfi_restore_state out () =
  if Emitaux.is_cfi_enabled () then emitp_string out "\t.cfi_restore_state\n"



(* Tradeoff between code size and code speed *)

let fastcode_flag = ref true

(* Names for special regs *)

let reg_domain_state_ptr = phys_reg Int 25 (* x28 *)
let reg_trap_ptr = phys_reg Int 23 (* x26 *)
let reg_alloc_ptr = phys_reg Int 24 (* x27 *)
let reg_tmp1 = phys_reg Int 26 (* x16 *)
let reg_x8 = phys_reg Int 8 (* x8 *)
let reg_stack_arg_begin = phys_reg Int 17  (* x20 *)
let reg_stack_arg_end  = phys_reg Int 18 (* x21 *)


(* Output a label *)

let label_prefix =
  if macosx then "L" else ".L"

let emitp_label lbl =
  emitp_string label_prefix; emitp_string (Label.to_string lbl)

(* Symbols *)

(* CR sdolan: Support local symbol definitions & references on arm64 *)

let emitp_symbol s =
  if macosx then emitp_string "_";
  Emitaux.emitp_symbol s

(* Object types *)

let emitp_symbol_type emitp_lbl_or_sym lbl_or_sym ty =
  if not macosx then begin
    emitp_format out "	.type	%a, %%%a\n" emitp_lbl_or_sym lbl_or_sym emitp_string ty
  end


let emitp_symbol_size sym =
  if not macosx then begin
    emitp_format out "	.size	%a, .-%a\n" emitp_symbol sym emitp_symbol sym
  end

(* Output a pseudo-register *)

let emitp_reg = function
    {loc = Reg r; typ; _} -> emitp_string (register_name typ r)
  | {loc = (Stack _ | Unknown); _}  -> fatal_error "Emit.emitp_reg"

(* Likewise, but with the 32-bit name of the register *)

let int_reg_name_w =
  [| "w0";  "w1";  "w2";  "w3";  "w4";  "w5";  "w6";  "w7";
     "w8";  "w9";  "w10"; "w11"; "w12"; "w13"; "w14"; "w15";
     "w19"; "w20"; "w21"; "w22"; "w23"; "w24"; "w25";
     "w26"; "w27"; "w28"; "w16"; "w17" |]

let emitp_wreg = function
    {loc = Reg r; _} -> emitp_string int_reg_name_w.(r)
  | {loc = (Stack _ | Unknown); _}  -> fatal_error "Emit.emitp_wreg"

(* Layout of the stack frame *)

let stack_offset = ref 0

let num_stack_slots = Array.make Proc.num_stack_slot_classes 0

let prologue_required = ref false

let contains_calls = ref false

let initial_stack_offset () =
  Proc.initial_stack_offset ~contains_calls:!contains_calls
    ~num_stack_slots

let frame_size () =
  Proc.frame_size
    ~stack_offset:!stack_offset
    ~contains_calls:!contains_calls
    ~num_stack_slots

let slot_offset loc stack_class =
  let offset =
    Proc.slot_offset loc ~stack_class ~stack_offset:!stack_offset
      ~fun_contains_calls:!contains_calls ~fun_num_stack_slots:num_stack_slots
  in
  match offset with
  | Bytes_relative_to_stack_pointer n -> n
  | Bytes_relative_to_domainstate_pointer _ ->
    Misc.fatal_errorf "Not a stack slot"

(* Output a stack reference *)

let emitp_stack r =
  match r.loc with
  | Stack (Domainstate n) ->
      let ofs = n + Domainstate.(idx_of_field Domain_extra_params) * 8 in
      emitp_format out "[%a, #%a]" emitp_reg reg_domain_state_ptr emitp_int ofs
  | Stack ((Local _ | Incoming _ | Outgoing _) as s) ->
      let ofs = slot_offset s (stack_slot_class r.typ) in
      emitp_format out "[sp, #%a]" emitp_int ofs
  | Reg _ | Unknown -> fatal_error "Emit.emitp_stack"

(* Output an addressing mode *)

let emitp_symbol_offset s ofs =
  emitp_symbol s;
  if ofs > 0 then emitp_format out "+%a" emitp_int ofs
  else if ofs < 0 then emitp_format out "-%a" emitp_int (-ofs)
  else ()

let emitp_addressing addr r =
  match addr with
  | Iindexed ofs ->
      emitp_format out "[%a, #%a]" emitp_reg r emitp_int ofs
  | Ibased(s, ofs) ->
      assert (not !Clflags.dlcode);  (* see selection.ml *)
      emitp_format out "[%a, #:lo12:%a]" emitp_reg r emitp_symbol_offset s ofs

(* Record live pointers at call points *)

let record_frame_label live dbg =
  let lbl = Cmm.new_label () in
  let live_offset = ref [] in
  Reg.Set.iter
    (function
      | {typ = Val; loc = Reg r} ->
          live_offset := ((r lsl 1) + 1) :: !live_offset
      | {typ = Val; loc = Stack s} as reg ->
          live_offset := slot_offset s (stack_slot_class reg.typ) :: !live_offset
      | {typ = Addr} as r ->
          Misc.fatal_error ("bad GC root " ^ Reg.name r)
      | { typ = Valx2; } as r ->
          (* CR mslater: (SIMD) arm64 *)
          Misc.fatal_error ("Unexpected Valx2 type of reg " ^ Reg.name r)
      | { typ = Val; loc = Unknown ; } as r ->
          Misc.fatal_error ("Unknown location " ^ Reg.name r)
      | { typ = Int | Float | Float32 | Vec128; _ } -> ())
    live;
  record_frame_descr ~label:lbl ~frame_size:(frame_size())
    ~live_offset:!live_offset dbg;
  lbl

let record_frame live dbg =
  let lbl = record_frame_label live dbg in emitp_format out "%a:" emitp_label lbl

(* Record calls to the GC -- we've moved them out of the way *)

type gc_call =
  { gc_lbl: label;                      (* Entry label *)
    gc_return_lbl: label;               (* Where to branch after GC *)
    gc_frame_lbl: label }               (* Label of frame descriptor *)

let call_gc_sites = ref ([] : gc_call list)

let emitp_call_gc gc =
  emitp_format out "%a:	bl	%a\n" emitp_label gc.gc_lbl emitp_symbol "caml_call_gc";
  emitp_format out "%a:	b	%a\n" emitp_label gc.gc_frame_lbl emitp_label gc.gc_return_lbl

(* Record calls to local stack reallocation *)

type local_realloc_call =
  { lr_lbl: label;
    lr_return_lbl: label;
    lr_dbg: Debuginfo.t
  }

let local_realloc_sites = ref ([] : local_realloc_call list)

let emitp_local_realloc lr =
  emitp_format out "%a:\n" emitp_label lr.lr_lbl;
  emitp_format out "	%a\n" emitp_debug_info lr.lr_dbg;
  emitp_format out "	bl	%a\n" emitp_symbol "caml_call_local_realloc";
  emitp_format out "	b	%a\n" emitp_label lr.lr_return_lbl

(* Local stack reallocation *)

type stack_realloc = {
  sc_label : Label.t; (* Label of the reallocation code. *)
  sc_return : Label.t; (* Label to return to after reallocation. *)
  sc_max_frame_size_in_bytes : int; (* Size for reallocation. *)
}

let stack_realloc = ref (None : stack_realloc option)

let clear_stack_realloc () =
  stack_realloc := None

let emitp_stack_realloc () =
  match !stack_realloc with
  | None -> ()
  | Some { sc_label; sc_return; sc_max_frame_size_in_bytes; } ->
    emitp_format out "%a:\n" emitp_label sc_label;
    (* Pass the desired frame size on the stack, since all of the
       argument-passing registers may be in use. *)
    emitp_format out "    mov %a, #%a\n" emitp_reg reg_tmp1 emitp_int sc_max_frame_size_in_bytes;
    emitp_format out "    stp %a, x30, [sp, #-16]!\n" emitp_reg reg_tmp1;
    emitp_format out "    bl %a\n" emitp_symbol "caml_call_realloc_stack";
    emitp_format out "    ldp %a, x30, [sp], #16\n" emitp_reg reg_tmp1;
    emitp_format out "    b %a\n" emitp_label sc_return

(* Names of various instructions *)

let name_for_comparison = function
  | Isigned Ceq -> "eq" | Isigned Cne -> "ne" | Isigned Cle -> "le"
  | Isigned Cge -> "ge" | Isigned Clt -> "lt" | Isigned Cgt -> "gt"
  | Iunsigned Ceq -> "eq" | Iunsigned Cne -> "ne" | Iunsigned Cle -> "ls"
  | Iunsigned Cge -> "cs" | Iunsigned Clt -> "cc" | Iunsigned Cgt -> "hi"

let name_for_int_operation = function
  | Iadd -> "add"
  | Isub -> "sub"
  | Imul -> "mul"
  | Idiv -> "sdiv"
  | Iand -> "and"
  | Ior  -> "orr"
  | Ixor -> "eor"
  | Ilsl -> "lsl"
  | Ilsr -> "lsr"
  | Iasr -> "asr"
  | Iclz { arg_is_non_zero = _ } -> "clz"
  | Ipopcnt -> "cnt"
  | Ictz _ | Icomp _ | Imod | Imulh _-> assert false

(* Decompose an integer constant into four 16-bit shifted fragments.
   Omit the fragments that are equal to "default" (16 zeros or 16 ones). *)

let decompose_int default n =
  let rec decomp n pos =
    if pos >= 64 then [] else begin
      let frag = Nativeint.logand n 0xFFFFn
      and rem  = Nativeint.shift_right_logical n 16 in
      if frag = default
      then decomp rem (pos + 16)
      else (frag, pos) :: decomp rem (pos + 16)
    end
  in decomp n 0

(* Load an integer constant into a register *)

let emitp_movk dst (f, p) =
    emitp_format out "	movk	%a, #%a, lsl #%a\n" emitp_reg dst emitp_nativeint f emitp_int p

let emitp_intconst dst n =
  if is_logical_immediate n then
    emitp_format out "	orr	%a, xzr, #%a\n" emitp_reg dst emitp_nativeint n
  else begin
    let dz = decompose_int 0x0000n n
    and dn = decompose_int 0xFFFFn n in
    if List.length dz <= List.length dn then begin
      match dz with
      | [] ->
          emitp_format out "	mov	%a, xzr\n" emitp_reg dst
      | (f, p) :: l ->
          emitp_format out "	movz	%a, #%a, lsl #%a\n" emitp_reg dst emitp_nativeint f emitp_int p;
          List.iter (emitp_movk dst) l
    end else begin
      match dn with
      | [] ->
          emitp_format out "	movn	%a, #0\n" emitp_reg dst
      | (f, p) :: l ->
          let nf = Nativeint.logxor f 0xFFFFn in
          emitp_format out "	movn	%a, #%a, lsl #%a\n" emitp_reg dst emitp_nativeint nf emitp_int p;
          List.iter (emitp_movk dst) l
    end
  end

let num_instructions_for_intconst n =
  if is_logical_immediate n then 1 else begin
    let dz = decompose_int 0x0000n n
    and dn = decompose_int 0xFFFFn n in
    max 1 (min (List.length dz) (List.length dn))
  end

(* Recognize float constants appropriate for FMOV dst, #fpimm instruction:
   "a normalized binary floating point encoding with 1 sign bit, 4
    bits of fraction and a 3-bit exponent" *)

let is_immediate_float bits =
  let exp = (Int64.(to_int (shift_right_logical bits 52)) land 0x7FF) - 1023 in
  let mant = Int64.logand bits 0xF_FFFF_FFFF_FFFFL in
  exp >= -3 && exp <= 4 && Int64.logand mant 0xF_0000_0000_0000L = mant

let is_immediate_float32 bits =
  let exp = (Int32.(to_int (shift_right_logical bits 23)) land 0x7F) - 63 in
  let mant = Int32.logand bits 0x7F_FFFFl in
  exp >= -3 && exp <= 4 && Int32.logand mant 0x78_0000l = mant

(* Adjust sp (up or down) by the given byte amount *)

let emitp_stack_adjustment n =
  let instr = if n < 0 then "sub" else "add" in
  let m = abs n in
  assert (m < 0x1_000_000);
  let ml = m land 0xFFF and mh = m land 0xFFF_000 in
  if mh <> 0 then emitp_format out "	%a	sp, sp, #%a\n" emitp_string instr emitp_int mh;
  if ml <> 0 then emitp_format out "	%a	sp, sp, #%a\n" emitp_string instr emitp_int ml;
  if n <> 0 then cfi_adjust_cfa_offset (-n)

(* Deallocate the stack frame and reload the return address
   before a return or tail call *)

let output_epilogue f =
  let n = frame_size() in
  if !contains_calls then
    emitp_format out "	ldr	x30, [sp, #%a]\n" emitp_int (n-8);
  if n > 0 then
    emitp_stack_adjustment n;
  f();
  (* reset CFA back because function body may continue *)
  if n > 0 then cfi_adjust_cfa_offset n

(* Output add-immediate / sub-immediate / cmp-immediate instructions *)

let rec emitp_addimm rd rs n =
  if n < 0 then emitp_subimm rd rs (-n)
  else if n <= 0xFFF then
    emitp_format out "	add	%a, %a, #%a\n" emitp_reg rd emitp_reg rs emitp_int n
  else begin
    assert (n <= 0xFFF_FFF);
    let nl = n land 0xFFF and nh = n land 0xFFF_000 in
    emitp_format out "	add	%a, %a, #%a\n" emitp_reg rd emitp_reg rs emitp_int nh;
    if nl <> 0 then
      emitp_format out "	add	%a, %a, #%a\n" emitp_reg rd emitp_reg rd emitp_int nl
  end

and emitp_subimm rd rs n =
  if n < 0 then emitp_addimm rd rs (-n)
  else if n <= 0xFFF then
    emitp_format out "	sub	%a, %a, #%a\n" emitp_reg rd emitp_reg rs emitp_int n
  else begin
    assert (n <= 0xFFF_FFF);
    let nl = n land 0xFFF and nh = n land 0xFFF_000 in
    emitp_format out "	sub	%a, %a, #%a\n" emitp_reg rd emitp_reg rs emitp_int nh;
    if nl <> 0 then
      emitp_format out "	sub	%a, %a, #%a\n" emitp_reg rd emitp_reg rd emitp_int nl
  end

let emitp_cmpimm rs n =
  if n >= 0
  then emitp_format out "	cmp	%a, #%a\n" emitp_reg rs emitp_int n
  else emitp_format out "	cmn	%a, #%a\n" emitp_reg rs emitp_int (-n)

(* Name of current function *)
let function_name = ref ""
(* Entry point for tail recursive calls *)
let tailrec_entry_point = ref None
(* Pending floating-point literals *)
let float_literals = ref ([] : (int64 * label) list)
let vec128_literals = ref ([] : (Cmm.vec128_bits * label) list)

(* Label a floating-point literal *)
let add_literal p f =
  try
    List.assoc f !p
  with Not_found ->
    let lbl = Cmm.new_label() in
    p := (f, lbl) :: !p;
    lbl

let float_literal f = add_literal float_literals f
let vec128_literal f = add_literal vec128_literals f

(* Emit all pending literals *)
let emitp_literals p align emitp_literal =
  if !p <> [] then begin
    if macosx then
    emitp_format out "	.section	__TEXT,__literal%a,%abyte_literals\n" emitp_int align emitp_int align;
    emitp_format out "	.balign	%a\n" emitp_int align;
    List.iter emitp_literal !p;
    p := []
  end

let emitp_float_literal (f, lbl) =
     emitp_format out "%a:" emitp_label lbl; emitp_float64_directive ".quad" f

let emitp_vec128_literal (({ high; low; } : Cmm.vec128_bits), lbl) =
     emitp_format out "%a:\n" emitp_label lbl;
     emitp_float64_directive ".quad" low;
     emitp_float64_directive ".quad" high

let emitp_literals () =
  emitp_literals float_literals size_float emitp_float_literal;
  emitp_literals vec128_literals size_vec128 emitp_vec128_literal

(* Emit code to load the address of a symbol *)

let emitp_load_symbol_addr dst s =
  if macosx then begin
    emitp_format out "	adrp	%a, %a%@GOTPAGE\n" emitp_reg dst emitp_symbol s;
    emitp_format out "	ldr	%a, [%a, %a%@GOTPAGEOFF]\n" emitp_reg dst emitp_reg dst emitp_symbol s
  end else if not !Clflags.dlcode then begin
    emitp_format out "	adrp	%a, %a\n" emitp_reg dst emitp_symbol s;
    emitp_format out "	add	%a, %a, #:lo12:%a\n" emitp_reg dst emitp_reg dst emitp_symbol s
  end else begin
    emitp_format out "	adrp	%a, :got:%a\n" emitp_reg dst emitp_symbol s;
    emitp_format out "	ldr	%a, [%a, #:got_lo12:%a]\n" emitp_reg dst emitp_reg dst emitp_symbol s
  end

(* The following functions are used for calculating the sizes of the
   call GC and bounds check points emitted out-of-line from the function
   body.  See branch_relaxation.mli. *)

let num_call_gc_points instr =
  let rec loop instr call_gc =
    match instr.desc with
    | Lend -> call_gc
    | Lop (Alloc { mode = Heap; _ }) when !fastcode_flag ->
      loop instr.next (call_gc + 1)
    | Lop Poll ->
      loop instr.next (call_gc + 1)
    (* The following four should never be seen, since this function is run
       before branch relaxation. *)
    | Lop (Specific (Ifar_alloc _))
    | Lop (Specific (Ifar_poll _)) -> assert false
    | Lop (Alloc { mode = (Local | Heap); _ })
    | Lop (Specific
             (Imuladd|Imulsub|Inegmulf|Imuladdf|Inegmuladdf|Imulsubf|Inegmulsubf|
              Isqrtf|Imove32|Ishiftarith (_, _)|Ibswap _|Isignext _|Isimd _))
    | Lop (Move|Spill|Reload|Opaque|Begin_region|End_region|Dls_get|Const_int _|
           Const_float32 _|Const_float _|Const_symbol _|Const_vec128 _|Stackoffset _|
           Load _|Store (_, _, _)|Intop _|Intop_imm (_, _)|Intop_atomic _|
           Floatop (_, _)|Csel _|Reinterpret_cast _|Static_cast _|Probe_is_enabled _|
           Name_for_debugger _)
    | Lprologue|Lreloadretaddr|Lreturn|Lentertrap|Lpoptrap|Lcall_op _|Llabel _|
    Lbranch _|Lcondbranch (_, _)|Lcondbranch3 (_, _, _)|Lswitch _|
    Ladjust_stack_offset _|Lpushtrap _|Lraise _|Lstackcheck _
      -> loop instr.next call_gc
  in
  loop instr 0

let max_out_of_line_code_offset ~num_call_gc =
  if num_call_gc < 1 then 0
  else begin
    let size_of_call_gc = 2 in
    let size_of_last_thing = size_of_call_gc in
    let total_size = size_of_call_gc*num_call_gc in
    let max_offset = total_size - size_of_last_thing in
    assert (max_offset >= 0);
    max_offset
  end

module DSL : sig

  val check_reg : Cmm.machtype_component -> Reg.t -> unit
  val emitp_reg : Reg.t -> Arm64_ast.Operand.t
  val emitp_reg_d : Reg.t -> Arm64_ast.Operand.t
  val emitp_reg_s : Reg.t -> Arm64_ast.Operand.t
  val emitp_reg_w : Reg.t -> Arm64_ast.Operand.t
  val emitp_reg_v2d : Reg.t -> Arm64_ast.Operand.t
  val imm : int -> Arm64_ast.Operand.t
  val ins : I.t -> Arm64_ast.Operand.t array -> unit

  val simd_instr : Simd.operation -> Linear.instruction -> unit
  val simd_instr_size : Simd.operation -> int

end [@warning "-32"]  = struct
  include Arm64_ast.DSL
  let check_reg typ reg =
   (* same type and not on stack *)
   assert (Cmm.equal_machtype_component typ reg.typ);
   assert (Reg.is_reg reg);
   ()

  (* See [Proc.int_reg_name]. *)
  let int_reg_name_to_arch_index =
  [| 0;  1;  2; 3; 4; 5; 6 ; 7;    (* 0 - 7 *)
     8; 9; 10; 11; 12; 13; 14; 15; (* 8 - 15 *)
     19; 20; 21; 22; 23; 24; 25;   (* 16 - 22 *)
     26; 27; 28;                   (* 23 - 25 *)
     16; 17; |]                    (* 26 - 27 *)

  let reg_name_to_arch_index reg_class name_index =
    match reg_class with
    | 0  (* general-purpose registers *) -> int_reg_name_to_arch_index.(name_index)
    | 1  (* neon registers *) -> name_index
    | _ -> assert false

  let reg_index reg =
    match reg with
    | {loc = Reg r; _} ->
      let reg_class = Proc.register_class reg in
      let name_index = r - Proc.first_available_register.(reg_class) in
      reg_name_to_arch_index reg_class name_index
    | {loc = (Stack _ | Unknown); _}  -> fatal_error "Emit.reg"

  let emitp_reg_v2s reg = reg_v2s (reg_index reg)

  let emitp_reg_v4s reg = reg_v4s (reg_index reg)

  let emitp_reg_v2d reg = reg_v2d (reg_index reg)

  let emitp_reg_w reg = reg_w (reg_index reg)

  let emitp_reg_s reg = reg_s (reg_index reg)

  let emitp_reg_d reg = reg_d (reg_index reg)

  let emitp_reg reg =
    (* use machtype to select register name *)
    let index = reg_index reg in
    match reg.typ with
    | Val | Int | Addr ->
      reg_x index
    | Float ->
      reg_d index
    | Float32 ->
      reg_s index
    | Vec128
    | Valx2 ->
      reg_q index

  let check_instr (register_behavior : Simd_proc.register_behavior) i =
    (* Ensure that operation size and register size match.
       On arm64, operation size is encoded solely into the operands
       (unlike amd64 where the opcode itself usually indicates operation size). *)
    match register_behavior with
    | Rf32x2_Rf32x2_to_Rf32x2 ->
      (* float32x2 is represented as Float machtype *)
      check_reg Float i.arg.(0);
      check_reg Float i.arg.(1);
      check_reg Float i.res.(0);
    | Rf32x4_Rf32x4_to_Rf32x4
    | Rf64x2_Rf64x2_to_Rf64x2
    | Ri64x2_Ri64x2_to_Ri64x2  ->
      check_reg Vec128 i.arg.(0);
      check_reg Vec128 i.arg.(1);
      check_reg Vec128 i.res.(0);
    | Rf32_Rf32_to_Rf32 ->
      check_reg Float32 i.arg.(0);
      check_reg Float32 i.arg.(1);
      check_reg Float32 i.res.(0)
    | Rf32_to_Rf32 ->
      check_reg Float32 i.arg.(0);
      check_reg Float32 i.res.(0)
    | Rf32_to_Ri64 ->
      check_reg Float32 i.arg.(0);
      check_reg Int i.res.(0)

  let src_operands ops =
    (* returns a copy of [ops] without the first operand, which is assumed to be the
       destination operand. *)
    Array.sub ops 1 (Array.length ops - 1)

  let emitp_regs_binary i =
    [| emitp_reg i.res.(0); emitp_reg i.arg.(0); emitp_reg i.arg.(1) |]

  let emitp_regs_unary i =
    [| emitp_reg i.res.(0); emitp_reg i.arg.(0); |]

  let ins name ops = print_ins name ops |> emitp_string

  let ins_cond name cond ops = print_ins_cond name cond ops |> emitp_string

  let emitp_operands (register_behavior : Simd_proc.register_behavior) i =
    match register_behavior with
    | Rf32x2_Rf32x2_to_Rf32x2 ->
      (* Special case: f32 argument is represented by machtype Float (to avoid classifying
         it as a reinterpret cast), and uses vector f32x2 register in the instruction
         encoding. *)
      [| emitp_reg_v2s i.res.(0); emitp_reg_v2s i.arg.(0); emitp_reg_v2s i.arg.(1)|]
    | Rf32x4_Rf32x4_to_Rf32x4 ->
      [| emitp_reg_v4s i.res.(0); emitp_reg_v4s i.arg.(0); emitp_reg_v4s i.arg.(1)|]
    | Ri64x2_Ri64x2_to_Ri64x2
    | Rf64x2_Rf64x2_to_Rf64x2 ->
      [| emitp_reg_v2d i.res.(0); emitp_reg_v2d i.arg.(0); emitp_reg_v2d i.arg.(1)|]
    | Rf32_Rf32_to_Rf32 ->
      emitp_regs_binary i
    | Rf32_to_Rf32
    | Rf32_to_Ri64 ->
      emitp_regs_unary i

  let simd_instr_size (op : Simd.operation)  =
    match op with
    | Min_scalar_f32 | Max_scalar_f32 -> 2
    | Round_f32 _ | Round_f32_i64 | Zip1_f32 | Zip1q_f32 | Zip1q_f64 | Zip2q_f64
    | Fmin_f32 | Fmax_f32 | Addq_i64 | Subq_i64 -> 1

  let emitp_rounding_mode (rm : Simd.Rounding_mode.t) : I.Rounding_mode.t =
    match rm with
    | Neg_inf -> I.Rounding_mode.M
    | Pos_inf -> I.Rounding_mode.P
    | Zero -> I.Rounding_mode.Z
    | Current -> I.Rounding_mode.X

  let simd_instr (op : Simd.operation) i =
    let b = Simd_proc.register_behavior op in
    check_instr b i;
    let operands = emitp_operands b i in
    match op with
    (* min/max: generate a sequence that matches the weird semantics of amd64 instruction
       "minss", even when the flag [FPCR.AH] is not set. A separate intrinsics generates
       fmin/fmax arm64 instructions directly. *)
    | Min_scalar_f32 ->
      ins I.FCMP (src_operands operands);
      ins_cond I.FCSEL I.Cond.MI operands;
    | Max_scalar_f32 ->
      ins I.FCMP (src_operands operands);
      ins_cond I.FCSEL I.Cond.GT operands;
    | Round_f32 rm ->
      ins (I.FRINT (emitp_rounding_mode rm)) operands
    | Round_f32_i64 ->
      ins I.FCVTNS operands
    | Fmin_f32 ->
      ins I.FMIN operands
    | Fmax_f32 ->
      ins I.FMAX operands
    | Zip1_f32    | Zip1q_f32
    | Zip1q_f64  ->
      ins I.ZIP1 operands
    | Zip2q_f64  ->
      ins I.ZIP2 operands
    | Addq_i64 ->
      ins I.ADD operands
    | Subq_i64 ->
      ins I.SUB operands
end

module BR = Branch_relaxation.Make (struct
  (* CR-someday mshinwell: B and BL have +/- 128Mb ranges; for the moment we
     assume we will never exceed this.  It would seem to be most likely to
     occur for branches between functions; in this case, the linker should be
     able to insert veneers anyway.  (See section 4.6.7 of the document
     "ELF for the ARM 64-bit architecture (AArch64)".) *)

  type distance = int

  module Cond_branch = struct
    type t = TB | CB | Bcc

    let all = [TB; CB; Bcc]

    (* AArch64 instructions are 32 bits wide, so [distance] in this module
       means units of 32-bit words. *)
    let max_displacement = function
      | TB -> 32 * 1024 / 4  (* +/- 32Kb *)
      | CB | Bcc -> 1 * 1024 * 1024 / 4  (* +/- 1Mb *)

    let classify_instr = function
      | Lop (Alloc _)
      | Lop Poll -> Some Bcc
      (* The various "far" variants in [specific_operation] don't need to
         return [Some] here, since their code sequences never contain any
         conditional branches that might need relaxing. *)
      | Lcondbranch (Itruetest, _)
      | Lcondbranch (Ifalsetest, _) -> Some CB
      | Lcondbranch (Iinttest _, _)
      | Lcondbranch (Iinttest_imm _, _)
      | Lcondbranch (Ifloattest _, _) -> Some Bcc
      | Lcondbranch (Ioddtest, _)
      | Lcondbranch (Ieventest, _) -> Some TB
      | Lcondbranch3 _ -> Some Bcc
      | Lop (Specific _|Move|Spill|Reload|Opaque|Begin_region|End_region
            |Dls_get|Const_int _|
            Const_float32 _|Const_float _|Const_symbol _|Const_vec128 _|Stackoffset _|
            Load _|Store (_, _, _)|Intop _|Intop_imm (_, _)|Intop_atomic _|
            Floatop (_, _)|Csel _|Reinterpret_cast _|Static_cast _|Probe_is_enabled _|
            Name_for_debugger _)
      | Lprologue|Lend|Lreloadretaddr|Lreturn|Lentertrap|Lpoptrap|Lcall_op _
      | Llabel _|Lbranch _|Lswitch _|Ladjust_stack_offset _|Lpushtrap _|Lraise _
      |Lstackcheck _
        -> None
  end

  let offset_pc_at_branch = 0

  let prologue_size () =
    (if initial_stack_offset () > 0 then 2 else 0)
      + (if !contains_calls then 1 else 0)

  let epilogue_size () =
    if !contains_calls then 3 else 2

  let memory_access_size (memory_chunk : Cmm.memory_chunk) =
    match memory_chunk with
    | Single { reg = Float64 } -> 2
    | Single { reg = Float32 } -> 1
    | Byte_unsigned|Byte_signed|Sixteen_unsigned|Sixteen_signed|
      Thirtytwo_unsigned|Thirtytwo_signed|Word_int|Word_val|Double|
      Onetwentyeight_unaligned|Onetwentyeight_aligned -> 1

  let instr_size = function
    | Lend -> 0
    | Lprologue -> prologue_size ()
    | Lop (Move | Spill | Reload) -> 1
    | Lop (Const_int n) ->
      num_instructions_for_intconst n
    | Lop (Const_float32 _) -> 2
    | Lop (Const_float _) -> 2
    | Lop (Const_vec128 _) -> 2
    | Lop (Const_symbol _) -> 2
    | Lop (Intop_atomic _) ->
      (* Never generated; builtins are not yet translated to atomics *)
      assert false
    | Lcall_op (Lcall_ind) -> 1
    | Lcall_op (Lcall_imm _) -> 1
    | Lcall_op (Ltailcall_ind) -> epilogue_size ()
    | Lcall_op (Ltailcall_imm { func; _ }) ->
      if func.sym_name = !function_name then 1 else epilogue_size ()
    | Lcall_op (Lextcall {alloc; stack_ofs; func=_; ty_res=_; ty_args=_; returns=_; } ) ->
      if Config.runtime5 && stack_ofs > 0 then 5
      else if alloc then 3
      else 5
    | Lop (Stackoffset _) -> 2
    | Lop (Load  { memory_chunk; addressing_mode; is_atomic; mutability=_ }) ->
      let based = match addressing_mode with Iindexed _ -> 0 | Ibased _ -> 1
      and barrier = if is_atomic then 1 else 0
      and single = memory_access_size memory_chunk in
      based + barrier + single
    | Lop (Store (memory_chunk, addressing_mode, assignment)) ->
      let based = match addressing_mode with Iindexed _ -> 0 | Ibased _ -> 1
      and barrier =
        match memory_chunk, assignment with
        | (Word_int | Word_val), true -> 1
        | (Word_int | Word_val), false -> 0
        | (Byte_unsigned|Byte_signed|Sixteen_unsigned|Sixteen_signed|
           Thirtytwo_unsigned|Thirtytwo_signed|Single _|Double|
           Onetwentyeight_unaligned|Onetwentyeight_aligned),_ -> 0
      and single = memory_access_size memory_chunk in
      based + barrier + single
    | Lop (Alloc { mode = Local; _ }) -> 9
    | Lop (Alloc { mode = Heap;_ }) when !fastcode_flag -> 5
    | Lop (Specific (Ifar_alloc _)) when !fastcode_flag -> 6
    | Lop Poll -> 3
    | Lop (Specific (Ifar_poll _)) -> 4
    | Lop (Alloc { mode = Heap; bytes = num_bytes; _ })
    | Lop (Specific (Ifar_alloc { bytes = num_bytes; _ })) ->
      begin match num_bytes with
      | 16 | 24 | 32 -> 1
      | _ -> 1 + num_instructions_for_intconst (Nativeint.of_int num_bytes)
      end
    | Lop (Csel _) -> 4
    | Lop (Begin_region | End_region) -> 1
    | Lop (Intop (Icomp _)) -> 2
    | Lop (Floatop (Float64, Icompf _)) -> 2
    | Lop (Floatop (Float32, Icompf _)) -> 2
    | Lop (Intop_imm (Icomp _, _)) -> 2
    | Lop (Intop Imod) -> 2
    | Lop (Intop (Imulh _)) -> 1
    | Lop (Intop (Iclz _)) -> 1
    | Lop (Intop (Ictz _)) -> 2
    | Lop (Intop (Iadd|Isub|Imul|Idiv|Iand|Ior|Ixor|Ilsl|Ilsr|Iasr|Ipopcnt)) -> 1
    | Lop (Intop_imm
             ((Iadd|Isub|Imul|Idiv|Imod|Imulh _|Iand|Ior|Ixor|Ilsl|Ilsr|Iasr
              | Iclz _ | Ictz _ |Ipopcnt),_)) -> 1
    | Lop (Floatop (Float64, (Iabsf | Inegf))) -> 1
    | Lop (Floatop (Float32, (Iabsf | Inegf))) -> 1
    | Lop (Specific Isqrtf) -> 1
    | Lop (Reinterpret_cast (Value_of_int | Int_of_value |
                              Float_of_int64 | Int64_of_float)) -> 1
    | Lop (Reinterpret_cast (Float32_of_float | Float_of_float32 |
                              Float32_of_int32 | Int32_of_float32)) -> 1
    | Lop (Reinterpret_cast V128_of_v128) -> 1
    | Lop (Static_cast (Float_of_int Float64 | Int_of_float Float64)) -> 1
    | Lop (Static_cast (Float_of_int Float32 | Int_of_float Float32 |
                         Float_of_float32 | Float32_of_float)) -> 1
    | Lop (Static_cast (Scalar_of_v128 (Int8x16 | Int16x8))) -> 2
    | Lop (Static_cast (Scalar_of_v128 (Int32x4 | Int64x2 | Float32x4 | Float64x2))) -> 1
    | Lop (Static_cast (V128_of_scalar _ )) -> 1
    | Lop (Floatop (Float64, (Iaddf | Isubf | Imulf | Idivf))) -> 1
    | Lop (Floatop (Float32, (Iaddf | Isubf | Imulf | Idivf))) -> 1
    | Lop (Specific Inegmulf) -> 1
    | Lop (Opaque) -> 0
    | Lop (Specific (Imuladdf | Inegmuladdf | Imulsubf | Inegmulsubf)) -> 1
    | Lop (Specific (Ishiftarith _)) -> 1
    | Lop (Specific (Imuladd | Imulsub)) -> 1
    | Lop (Specific (Ibswap { bitwidth = Sixteen } )) -> 2
    | Lop (Specific (Ibswap { bitwidth = (Thirtytwo | Sixtyfour) })) -> 1
    | Lop (Specific Imove32) -> 1
    | Lop (Specific (Isignext _)) -> 1
    | Lop (Name_for_debugger _) -> 0
    | Lcall_op (Lprobe _) | Lop (Probe_is_enabled _) ->
      fatal_error ("Probes not supported.")
    | Lop (Dls_get) -> 1
    | Lreloadretaddr -> 0
    | Lreturn -> epilogue_size ()
    | Llabel _ -> 0
    | Lbranch _ -> 1
    | Lcondbranch (tst, _) ->
      begin match tst with
      | Itruetest -> 1
      | Ifalsetest -> 1
      | Iinttest _ -> 2
      | Iinttest_imm _ -> 2
      | Ifloattest _ -> 2
      | Ioddtest -> 1
      | Ieventest -> 1
      end
    | Lcondbranch3 (lbl0, lbl1, lbl2) ->
      1 + begin match lbl0 with None -> 0 | Some _ -> 1 end
        + begin match lbl1 with None -> 0 | Some _ -> 1 end
        + begin match lbl2 with None -> 0 | Some _ -> 1 end
    | Lswitch jumptbl -> 3 + Array.length jumptbl
    | Lentertrap -> 0
    | Ladjust_stack_offset _ -> 0
    | Lpushtrap _ -> 4
    | Lpoptrap -> 1
    | Lraise k ->
      begin match k with
      | Lambda.Raise_regular -> 1
      | Lambda.Raise_reraise -> 1
      | Lambda.Raise_notrace -> 4
      end
    | Lstackcheck _ -> 5
    | Lop (Specific (Isimd simd)) ->
      DSL.simd_instr_size simd

  let relax_poll ~return_label =
    Lop (Specific (Ifar_poll { return_label }))

  let relax_allocation ~num_bytes ~dbginfo =
    Lop (Specific (Ifar_alloc { bytes = num_bytes; dbginfo }))
end)

let name_for_float_comparison : Cmm.float_comparison -> string = function
  | CFeq -> "eq"
  | CFneq -> "ne"
  | CFlt -> "cc"
  | CFnlt -> "cs"
  | CFle -> "ls"
  | CFnle -> "hi"
  | CFgt -> "gt"
  | CFngt -> "le"
  | CFge -> "ge"
  | CFnge -> "lt"

(* Output the assembly code for allocation. *)

let assembly_code_for_allocation i ~local ~n ~far ~dbginfo =
  if local then begin
    let r = i.res.(0) in
    let module DS = Domainstate in
    let domain_local_sp_offset = DS.(idx_of_field Domain_local_sp) * 8 in
    let domain_local_limit_offset = DS.(idx_of_field Domain_local_limit) * 8 in
    let domain_local_top_offset = DS.(idx_of_field Domain_local_top) * 8 in
    emitp_format out "	ldr	%a, [%a, #%a]\n" emitp_reg reg_tmp1 emitp_reg reg_domain_state_ptr emitp_int domain_local_limit_offset;
    emitp_format out "	ldr	%a, [%a, #%a]\n" emitp_reg r emitp_reg reg_domain_state_ptr emitp_int domain_local_sp_offset;
    emitp_subimm r r n;
    emitp_format out "	str	%a, [%a, #%a]\n" emitp_reg r emitp_reg reg_domain_state_ptr emitp_int domain_local_sp_offset;
    emitp_format out "	cmp	%a, %a\n" emitp_reg r emitp_reg reg_tmp1;
    let lbl_call = Cmm.new_label () in
    emitp_format out "	b.lt	%a\n" emitp_label lbl_call;
    let lbl_after_alloc = Cmm.new_label () in
    emitp_format out "%a:\n" emitp_label lbl_after_alloc;
    emitp_format out "	ldr	%a, [%a, #%a]\n" emitp_reg reg_tmp1 emitp_reg reg_domain_state_ptr emitp_int domain_local_top_offset;
    emitp_format out "	add	%a, %a, %a\n" emitp_reg r emitp_reg r emitp_reg reg_tmp1;
    emitp_format out "	add	%a, %a, #%a\n" emitp_reg r emitp_reg r emitp_int 8;
    local_realloc_sites :=
      { lr_lbl = lbl_call;
        lr_dbg = i.dbg;
        lr_return_lbl = lbl_after_alloc } :: !local_realloc_sites
  end else begin
    let lbl_frame =
      record_frame_label i.live (Dbg_alloc dbginfo)
    in
    if !fastcode_flag then begin
      let lbl_after_alloc = Cmm.new_label() in
      let lbl_call_gc = Cmm.new_label() in
      (* n is at most Max_young_whsize * 8, i.e. currently 0x808,
         so it is reasonable to assume n < 0x1_000.  This makes
         the generated code simpler. *)
      assert (16 <= n && n < 0x1_000 && n land 0x7 = 0);
      let offset = Domainstate.(idx_of_field Domain_young_limit) * 8 in
      emitp_format out "	ldr	%a, [%a, #%a]\n" emitp_reg reg_tmp1 emitp_reg reg_domain_state_ptr emitp_int offset;
      emitp_subimm reg_alloc_ptr reg_alloc_ptr n;
      emitp_format out "	cmp	%a, %a\n" emitp_reg reg_alloc_ptr emitp_reg reg_tmp1;
      if not far then begin
        emitp_format out "	b.lo	%a\n" emitp_label lbl_call_gc
      end else begin
        let lbl = Cmm.new_label () in
        emitp_format out "	b.cs	%a\n" emitp_label lbl;
        emitp_format out "	b	%a\n" emitp_label lbl_call_gc;
        emitp_format out "%a:\n" emitp_label lbl
      end;
      emitp_format out "%a:" emitp_label lbl_after_alloc;
      emitp_format out "	add	%a, %a, #8\n" emitp_reg i.res.(0) emitp_reg reg_alloc_ptr;
      call_gc_sites :=
        { gc_lbl = lbl_call_gc;
          gc_return_lbl = lbl_after_alloc;
          gc_frame_lbl = lbl_frame } :: !call_gc_sites
    end else begin
      begin match n with
      | 16 -> emitp_format out "	bl	%a\n" emitp_symbol "caml_alloc1"
      | 24 -> emitp_format out "	bl	%a\n" emitp_symbol "caml_alloc2"
      | 32 -> emitp_format out "	bl	%a\n" emitp_symbol "caml_alloc3"
      | _  -> emitp_intconst reg_x8 (Nativeint.of_int n);
              emitp_format out "	bl	%a\n" emitp_symbol "caml_allocN"
      end;
      emitp_format out "%a:	add	%a, %a, #8\n" emitp_label lbl_frame emitp_reg i.res.(0) emitp_reg reg_alloc_ptr
    end
  end

let assembly_code_for_poll i ~far ~return_label =
  let lbl_frame = record_frame_label i.live (Dbg_alloc []) in
  let lbl_call_gc = Cmm.new_label() in
  let lbl_after_poll = match return_label with
  | None -> Cmm.new_label()
  | Some lbl -> lbl in
  let offset = Domainstate.(idx_of_field Domain_young_limit) * 8 in
    emitp_format out "	ldr	%a, [%a, #%a]\n" emitp_reg reg_tmp1 emitp_reg reg_domain_state_ptr emitp_int offset;
    emitp_format out "	cmp	%a, %a\n" emitp_reg reg_alloc_ptr emitp_reg reg_tmp1;
  if not far then begin
    match return_label with
    | None ->
        emitp_format out "	b.ls	%a\n" emitp_label lbl_call_gc;
        emitp_format out "%a:\n" emitp_label lbl_after_poll
    | Some return_label ->
        emitp_format out "	b.hi	%a\n" emitp_label return_label;
        emitp_format out "	b	%a\n" emitp_label lbl_call_gc;
  end else begin
    match return_label with
    | None ->
        emitp_format out "	b.hi	%a\n" emitp_label lbl_after_poll;
        emitp_format out "	b	%a\n" emitp_label lbl_call_gc;
        emitp_format out "%a:\n" emitp_label lbl_after_poll
    | Some return_label ->
        let lbl = Cmm.new_label () in
        emitp_format out "	b.ls	%a\n" emitp_label lbl;
        emitp_format out "	b	%a\n" emitp_label return_label;
        emitp_format out "%a:	b	%a\n" emitp_label lbl emitp_label lbl_call_gc
  end;
  call_gc_sites :=
    { gc_lbl = lbl_call_gc;
      gc_return_lbl = lbl_after_poll;
      gc_frame_lbl = lbl_frame; } :: !call_gc_sites

(* Output .text section directive, or named .text.caml.<name> if enabled. *)

let emitp_named_text_section func_name =
  if !Clflags.function_sections then begin
    emitp_format out "	.section .text.caml.%a,%a,%%progbits\n" emitp_symbol func_name emitp_string_literal "ax"
  end
  else
    emitp_format out "	.text\n"

(* Emit code to load an emitted literal *)

let emitp_load_literal dst lbl =
  if macosx then begin
    emitp_format out "	adrp	%a, %a%@PAGE\n" emitp_reg reg_tmp1 emitp_label lbl;
    emitp_format out "	ldr	%a, [%a, %a%@PAGEOFF]\n" emitp_reg dst emitp_reg reg_tmp1 emitp_label lbl
  end else begin
    emitp_format out "	adrp	%a, %a\n" emitp_reg reg_tmp1 emitp_label lbl;
    emitp_format out "	ldr	%a, [%a, #:lo12:%a]\n" emitp_reg dst emitp_reg reg_tmp1 emitp_label lbl
  end

let move (src : Reg.t) (dst : Reg.t) =
  let distinct = not (Reg.same_loc src dst) in
  if distinct then
  match src.typ, src.loc, dst.typ, dst.loc with
  | Float, Reg _, Float, Reg _
  | Float32, Reg _, Float32, Reg _
     ->
     DSL.ins I.FMOV [| DSL.emitp_reg dst; DSL.emitp_reg src |]
  | (Vec128|Valx2), Reg _, (Vec128|Valx2), Reg _ ->
     DSL.ins I.MOV [| DSL.emitp_reg_v2d dst; DSL.emitp_reg_v2d src |]
  | (Int | Val | Addr), Reg _, (Int | Val | Addr), Reg _ ->
      DSL.ins I.MOV [| DSL.emitp_reg dst; DSL.emitp_reg src |]
  | _, Reg _, _, Stack _ ->
     emitp_format out "	str	%a, %a\n" emitp_reg src emitp_stack dst
  | _, Stack _, _, Reg _ ->
     emitp_format out "	ldr	%a, %a\n" emitp_reg dst emitp_stack src
  | _, Stack _, _, Stack _ ->
      Misc.fatal_errorf
      "Illegal move between registers (%a to %a)\n"
      Printreg.reg src Printreg.reg dst
  | _, Unknown, _, (Reg _ | Stack _ | Unknown)
  | _, (Reg _ | Stack _), _, Unknown ->
      Misc.fatal_errorf
      "Illegal move with an unknown register location (%a to %a)\n"
      Printreg.reg src Printreg.reg dst
  | (Float | Float32 | Vec128 | Int | Val | Addr | Valx2), (Reg _), _, _ ->
      Misc.fatal_errorf
       "Illegal move between registers of differing types (%a to %a)\n"
       Printreg.reg src Printreg.reg dst

let emitp_reinterpret_cast (cast : Cmm.reinterpret_cast) i =
    let src = i.arg.(0) in
    let dst = i.res.(0) in
    let distinct = not (Reg.same_loc src dst) in
    match cast with
    | Int64_of_float ->
      DSL.check_reg Float src;
      DSL.ins I.FMOV [| DSL.emitp_reg dst; DSL.emitp_reg src |]
    | Float_of_int64 ->
      DSL.check_reg Float dst;
      DSL.ins I.FMOV [| DSL.emitp_reg dst; DSL.emitp_reg src |]
    | Float32_of_int32 ->
      DSL.check_reg Float32 dst;
      DSL.ins I.FMOV [| DSL.emitp_reg dst; DSL.emitp_reg_w src |]
    | Int32_of_float32 ->
      DSL.check_reg Float32 src;
      DSL.ins I.FMOV [| DSL.emitp_reg_w dst; DSL.emitp_reg src |]
    | Float32_of_float ->
       if distinct then (
        DSL.check_reg Float src;
        DSL.check_reg Float32 dst;
        DSL.ins I.MOV [| DSL.emitp_reg_d dst; DSL.emitp_reg_d src |])
    | Float_of_float32 ->
      if distinct then (
        DSL.check_reg Float32 src;
        DSL.check_reg Float dst;
        DSL.ins I.MOV [| DSL.emitp_reg_d dst; DSL.emitp_reg_d src |])
    | V128_of_v128 ->
      if distinct then (
        DSL.check_reg Vec128 src;
        DSL.check_reg Vec128 dst;
        DSL.ins I.FMOV [| DSL.emitp_reg dst; DSL.emitp_reg src |])
    | Int_of_value | Value_of_int -> move src dst

let emitp_static_cast (cast : Cmm.static_cast) i =
    let dst = i.res.(0) in
    let src = i.arg.(0) in
    let distinct = not (Reg.same_loc src dst) in
    match cast with
    | Int_of_float Float64 ->
       DSL.check_reg Float src;
       DSL.ins I.FCVTZS[| DSL.emitp_reg dst; DSL.emitp_reg src |]
    | Int_of_float Float32 ->
       DSL.check_reg Float32 src;
       DSL.ins I.FCVTZS[| DSL.emitp_reg dst; DSL.emitp_reg src |]
    | Float_of_int Float64 ->
      DSL.check_reg Float dst;
      DSL.ins I.SCVTF [| DSL.emitp_reg dst; DSL.emitp_reg src |];
    | Float_of_int Float32 ->
      DSL.check_reg Float32 dst;
      DSL.ins I.SCVTF [| DSL.emitp_reg dst; DSL.emitp_reg src |];
    | Float_of_float32 ->
      DSL.check_reg Float dst;
      DSL.check_reg Float32 src;
      DSL.ins I.FCVT [| DSL.emitp_reg dst; DSL.emitp_reg src |];
    | Float32_of_float ->
      DSL.check_reg Float32 dst;
      DSL.check_reg Float src;
      DSL.ins I.FCVT [| DSL.emitp_reg dst; DSL.emitp_reg src |];
    | Scalar_of_v128 v ->
      DSL.check_reg Vec128 src;
      begin match v with
       | Int8x16 ->
         DSL.ins I.FMOV [| DSL.emitp_reg_w dst; DSL.emitp_reg_s src |];
         DSL.ins I.UXTB [| DSL.emitp_reg dst; DSL.emitp_reg_w dst; |];
       | Int16x8 ->
         DSL.ins I.FMOV [| DSL.emitp_reg_w dst; DSL.emitp_reg_s src |];
         DSL.ins I.UXTH [| DSL.emitp_reg dst; DSL.emitp_reg_w dst; |];
       | Int32x4 ->
         DSL.ins I.FMOV [| DSL.emitp_reg_w dst; DSL.emitp_reg_s src |]
       | Int64x2 ->
         DSL.ins I.FMOV [| DSL.emitp_reg dst; DSL.emitp_reg_d src |]
       | Float32x4 ->
         if distinct then (
           DSL.check_reg Float32 dst;
           DSL.ins I.FMOV [| DSL.emitp_reg dst; DSL.emitp_reg_s src |])
       | Float64x2 ->
         if distinct then (
           DSL.check_reg Float dst;
           DSL.ins I.FMOV [| DSL.emitp_reg dst ; DSL.emitp_reg_d src |])
      end
    | V128_of_scalar v ->
       DSL.check_reg Vec128 dst;
       begin match v with
       | Int8x16 ->
         DSL.ins I.FMOV [| DSL.emitp_reg_s dst; DSL.emitp_reg_w src |];
       | Int16x8 ->
         DSL.ins I.FMOV [| DSL.emitp_reg_s dst; DSL.emitp_reg_w src |];
       | Int32x4 ->
         DSL.ins I.FMOV [| DSL.emitp_reg_s dst; DSL.emitp_reg_w src |]
       | Int64x2 ->
         DSL.ins I.FMOV [| DSL.emitp_reg_d dst; DSL.emitp_reg src |]
       | Float32x4 ->
         if distinct then (
           DSL.check_reg Float32 src;
           DSL.ins I.FMOV [| DSL.emitp_reg_s dst; DSL.emitp_reg src |])
       | Float64x2 ->
         if distinct then (
           DSL.check_reg Float src;
           DSL.ins I.FMOV [| DSL.emitp_reg_d dst ; DSL.emitp_reg src |])
      end

(* Output the assembly code for an instruction *)

let emitp_instr i =
    emitp_debug_info i.dbg;
    match i.desc with
    | Lend -> ()
    | Lprologue ->
      assert (!prologue_required);
      let n = frame_size() in
      if n > 0 then
        emitp_stack_adjustment (-n);
      if !contains_calls then begin
        cfi_offset ~reg:30 (* return address *) ~offset:(-8);
        emitp_format out "	str	x30, [sp, #%a]\n" emitp_int (n-8)
      end
    | Lop(Intop_atomic _) ->
      (* Never generated; builtins are not yet translated to atomics *)
      assert false
    | Lop (Reinterpret_cast cast) ->
      emitp_reinterpret_cast cast i
    | Lop (Static_cast cast) ->
      emitp_static_cast cast i
    | Lop(Move | Spill | Reload) ->
        move i.arg.(0) i.res.(0)
    | Lop(Specific Imove32) ->
        let src = i.arg.(0) and dst = i.res.(0) in
        if not (Reg.same_loc src dst) then begin
          match (src, dst) with
          | {loc = Reg _}, {loc = Reg _} ->
              emitp_format out "	mov	%a, %a\n" emitp_wreg dst emitp_wreg src
          | {loc = Reg _}, {loc = Stack _} ->
              emitp_format out "	str	%a, %a\n" emitp_wreg src emitp_stack dst
          | {loc = Stack _}, {loc = Reg _} ->
              emitp_format out "	ldr	%a, %a\n" emitp_wreg dst emitp_stack src
          | {loc = Stack _}, {loc = Stack _}
          | _, {loc = Unknown}
          | {loc = Unknown}, _
            -> assert false
        end
    | Lop(Const_int n) ->
        emitp_intconst i.res.(0) n
    | Lop (Const_float32 f) ->
        DSL.check_reg Float32 i.res.(0);
        if f = 0l then
          emitp_format out "	fmov	%a, wzr\n" emitp_reg i.res.(0)
        else if is_immediate_float32 f then
          emitp_format out "	fmov	%a, #%a\n" emitp_reg i.res.(0) emitp_printf "%.7f" (Int32.float_of_bits f)
        else begin
          (* float32 constants still take up 8 bytes; we load the lower half. *)
          let lbl = float_literal (Int64.of_int32 f) in
          emitp_load_literal i.res.(0) lbl
        end
    | Lop(Const_float f) ->
        if f = 0L then
          emitp_format out "	fmov	%a, xzr\n" emitp_reg i.res.(0)
        else if is_immediate_float f then
          emitp_format out "	fmov	%a, #%a\n" emitp_reg i.res.(0) emitp_printf "%.7f" (Int64.float_of_bits f)
        else begin
          let lbl = float_literal f in
          emitp_load_literal i.res.(0) lbl
        end
    | Lop(Const_vec128 ({high; low} as l)) ->
      DSL.check_reg Vec128 i.res.(0);
      begin match (high, low) with
      | 0x0000_0000_0000_0000L, 0x0000_0000_0000_0000L ->
          let dst = DSL.emitp_reg_v2d i.res.(0) in
          DSL.ins I.MOVI [| dst; DSL.imm 0  |]
      | _ ->
          let lbl = vec128_literal l in
          emitp_load_literal i.res.(0) lbl
      end
    | Lop(Const_symbol s) ->
        emitp_load_symbol_addr i.res.(0) s.sym_name
    | Lcall_op(Lcall_ind) ->
        emitp_format out "	blr	%a\n" emitp_reg i.arg.(0);
        emitp_format out "%a\n" record_frame i.live (Dbg_other i.dbg)
    | Lcall_op(Lcall_imm { func; }) ->
        emitp_format out "	bl	%a\n" emitp_symbol func.sym_name;
        emitp_format out "%a\n" record_frame i.live (Dbg_other i.dbg)
    | Lcall_op(Ltailcall_ind) ->
        output_epilogue (fun () -> emitp_format out "	br	%a\n" emitp_reg i.arg.(0))
    | Lcall_op(Ltailcall_imm { func; }) ->
        if func.sym_name = !function_name then
          match !tailrec_entry_point with
          | None -> Misc.fatal_error "jump to missing tailrec entry point"
          | Some tailrec_entry_point -> emitp_format out "	b	%a\n" emitp_label tailrec_entry_point
        else
          output_epilogue (fun () -> emitp_format out "	b	%a\n" emitp_symbol func.sym_name)
    | Lcall_op(Lextcall {func; alloc; stack_ofs}) ->
        if Config.runtime5 && stack_ofs > 0 then begin
          emitp_format out "	mov	%a, sp\n" emitp_reg reg_stack_arg_begin;
          emitp_format out "	add	%a, sp, #%a\n" emitp_reg reg_stack_arg_end emitp_int (Misc.align stack_ofs 16);
          emitp_load_symbol_addr reg_x8 func;
          emitp_format out "	bl	%a\n" emitp_symbol "caml_c_call_stack_args";
          emitp_format out "%a\n" record_frame i.live (Dbg_other i.dbg)
        end else if alloc then begin
          emitp_load_symbol_addr reg_x8 func;
          emitp_format out "	bl	%a\n" emitp_symbol "caml_c_call";
          emitp_format out "%a\n" record_frame i.live (Dbg_other i.dbg)
        end else begin
          (* store ocaml stack in the frame pointer register
             NB: no need to store previous x29 because OCaml frames don't
             maintain frame pointer *)
          if Config.runtime5 then begin
            emitp_format out "	mov	x29, sp\n";
            cfi_remember_state ();
            cfi_def_cfa_register ~reg:29;
            let offset = Domainstate.(idx_of_field Domain_c_stack) * 8 in
            emitp_format out "	ldr	%a, [%a, %a]\n" emitp_reg reg_tmp1 emitp_reg reg_domain_state_ptr emitp_int offset;
              emitp_format out "	mov	sp, %a\n" emitp_reg reg_tmp1
          end;
          emitp_format out "	bl	%a\n" emitp_symbol func;
          if Config.runtime5 then begin
            emitp_format out "	mov	sp, x29\n";
          end;
          cfi_restore_state ()
        end
    | Lop(Stackoffset n) ->
        assert (n mod 16 = 0);
        emitp_stack_adjustment (-n);
        stack_offset := !stack_offset + n
    | Lop(Load { memory_chunk; addressing_mode; is_atomic }) ->
        assert(memory_chunk = Cmm.Word_int || memory_chunk = Cmm.Word_val || is_atomic = false);
        let dst = i.res.(0) in
        let base =
          match addressing_mode with
          | Iindexed _ -> i.arg.(0)
          | Ibased(s, ofs) ->
              assert (not !Clflags.dlcode);  (* see selection_utils.ml *)
              emitp_format out "	adrp	%a, %a\n" emitp_reg reg_tmp1 emitp_symbol_offset s ofs;
              reg_tmp1 in
        begin match memory_chunk with
        | Byte_unsigned ->
            emitp_format out "	ldrb	%a, %a\n" emitp_wreg dst emitp_addressing addressing_mode base
        | Byte_signed ->
            emitp_format out "	ldrsb	%a, %a\n" emitp_reg dst emitp_addressing addressing_mode base
        | Sixteen_unsigned ->
            emitp_format out "	ldrh	%a, %a\n" emitp_wreg dst emitp_addressing addressing_mode base
        | Sixteen_signed ->
            emitp_format out "	ldrsh	%a, %a\n" emitp_reg dst emitp_addressing addressing_mode base
        | Thirtytwo_unsigned ->
            emitp_format out "	ldr	%a, %a\n" emitp_wreg dst emitp_addressing addressing_mode base
        | Thirtytwo_signed ->
            emitp_format out "	ldrsw	%a, %a\n" emitp_reg dst emitp_addressing addressing_mode base
        | Single { reg = Float64 } ->
            DSL.check_reg Float dst;
            emitp_format out "	ldr	s7, %a\n" emitp_addressing addressing_mode base;
            emitp_format out "	fcvt	%a, s7\n" emitp_reg dst
        | Word_int | Word_val ->
          if is_atomic then begin
            assert (addressing_mode = Iindexed 0);
            emitp_format out "	dmb	ishld\n";
            emitp_format out "	ldar	%a, [%a]\n" emitp_reg dst emitp_reg i.arg.(0)
          end else
            emitp_format out "	ldr	%a, %a\n" emitp_reg dst emitp_addressing addressing_mode base
        | Double ->
                      emitp_format out "	ldr	%a, %a\n" emitp_reg dst emitp_addressing addressing_mode base
        | Single { reg = Float32 } ->
            DSL.check_reg Float32 dst;
            emitp_format out " ldr %a, %a\n" emitp_reg dst emitp_addressing addressing_mode base
        | Onetwentyeight_aligned | Onetwentyeight_unaligned ->
            (* CR gyorsh: check alignment *)
            DSL.check_reg Vec128 dst;
            emitp_format out " ldr %a, %a\n" emitp_reg dst emitp_addressing addressing_mode base
        end
    | Lop(Store(size, addr, assignment)) ->
      (* NB: assignments other than Word_int and Word_val do not follow the
      Multicore OCaml memory model and so do not emit a barrier *)
      let src = i.arg.(0) in
        let base =
          match addr with
          | Iindexed _ -> i.arg.(1)
          | Ibased(s, ofs) ->
              assert (not !Clflags.dlcode);
              emitp_format out "	adrp	%a, %a\n" emitp_reg reg_tmp1 emitp_symbol_offset s ofs;
              reg_tmp1 in
        begin match size with
        | Byte_unsigned | Byte_signed ->
            emitp_format out "	strb	%a, %a\n" emitp_wreg src emitp_addressing addr base
        | Sixteen_unsigned | Sixteen_signed ->
            emitp_format out "	strh	%a, %a\n" emitp_wreg src emitp_addressing addr base
        | Thirtytwo_unsigned | Thirtytwo_signed ->
            emitp_format out "	str	%a, %a\n" emitp_wreg src emitp_addressing addr base
        | Single { reg = Float64 } ->
            DSL.check_reg Float src;
            emitp_format out "	fcvt	s7, %a\n" emitp_reg src;
            emitp_format out "	str	s7, %a\n" emitp_addressing addr base;
        | Word_int | Word_val ->
            (* memory model barrier for non-initializing store *)
            if assignment then emitp_format out "	dmb	ishld\n";
            emitp_format out "	str	%a, %a\n" emitp_reg src emitp_addressing addr base
        | Double ->
          emitp_format out "	str	%a, %a\n" emitp_reg src emitp_addressing addr base
        | Single { reg = Float32 } ->
          DSL.check_reg Float32 src;
          emitp_format out " str %a, %a\n" emitp_reg src emitp_addressing addr base
        | Onetwentyeight_aligned | Onetwentyeight_unaligned ->
          (* CR gyorsh: check alignment *)
          DSL.check_reg Vec128 src;
          emitp_format out " str %a, %a\n" emitp_reg src emitp_addressing addr base
        end
    | Lop(Alloc { bytes = n; dbginfo; mode = Heap }) ->
        assembly_code_for_allocation i ~n ~local:false ~far:false ~dbginfo
    | Lop(Specific (Ifar_alloc { bytes = n; dbginfo })) ->
        assembly_code_for_allocation i ~n ~local:false ~far:true ~dbginfo
    | Lop(Alloc { bytes = n; dbginfo; mode = Local }) ->
        assembly_code_for_allocation i ~n ~local:true ~far:false ~dbginfo
    | Lop(Begin_region) ->
        let offset = Domainstate.(idx_of_field Domain_local_sp) * 8 in
        emitp_format out "	ldr	%a, [%a, #%a]\n" emitp_reg i.res.(0) emitp_reg reg_domain_state_ptr emitp_int offset
    | Lop(End_region) ->
        let offset = Domainstate.(idx_of_field Domain_local_sp) * 8 in
        emitp_format out "	str	%a, [%a, #%a]\n" emitp_reg i.arg.(0) emitp_reg reg_domain_state_ptr emitp_int offset
    | Lop(Poll) ->
        assembly_code_for_poll i ~far:false ~return_label:None
    | Lop(Specific (Ifar_poll { return_label })) ->
        assembly_code_for_poll i ~far:true ~return_label
    | Lop(Intop_imm(Iadd, n)) ->
        emitp_addimm i.res.(0) i.arg.(0) n
    | Lop(Intop_imm(Isub, n)) ->
        emitp_subimm i.res.(0) i.arg.(0) n
    | Lop(Intop(Icomp cmp)) ->
        emitp_format out "	cmp	%a, %a\n" emitp_reg i.arg.(0) emitp_reg i.arg.(1);
        emitp_format out "	cset	%a, %a\n" emitp_reg i.res.(0) emitp_string (name_for_comparison cmp)
    | Lop(Floatop(Float64, Icompf cmp)) ->
        let comp = name_for_float_comparison cmp in
        emitp_format out "	fcmp	%a, %a\n" emitp_reg i.arg.(0) emitp_reg i.arg.(1);
        emitp_format out "	cset	%a, %a\n" emitp_reg i.res.(0) emitp_string comp
    | Lop(Floatop(Float32, Icompf cmp)) ->
        let comp = name_for_float_comparison cmp in
        emitp_format out "	fcmp	%a, %a\n" emitp_reg i.arg.(0) emitp_reg i.arg.(1);
        emitp_format out "	cset	%a, %a\n" emitp_reg i.res.(0) emitp_string comp
    | Lop(Intop_imm(Icomp cmp, n)) ->
        emitp_cmpimm i.arg.(0) n;
        emitp_format out "	cset	%a, %a\n" emitp_reg i.res.(0) emitp_string (name_for_comparison cmp)
    | Lop(Intop Imod) ->
        emitp_format out "	sdiv	%a, %a, %a\n" emitp_reg reg_tmp1 emitp_reg i.arg.(0) emitp_reg i.arg.(1);
        emitp_format out "	msub	%a, %a, %a, %a\n" emitp_reg i.res.(0) emitp_reg reg_tmp1 emitp_reg i.arg.(1) emitp_reg i.arg.(0)
    | Lop(Intop (Imulh { signed = true })) ->
        emitp_format out "	smulh	%a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0) emitp_reg i.arg.(1)
    | Lop(Intop (Imulh { signed = false })) ->
        emitp_format out "	umulh	%a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0) emitp_reg i.arg.(1)
    | Lop(Intop (Ictz _)) ->
        (* emitp_format out "ctz Rd, Rn" is optionally supported from Armv8.7, but rbit and clz
           are supported in all ARMv8 CPUs. *)
        emitp_format out " rbit %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0);
        emitp_format out " clz  %a, %a\n" emitp_reg i.res.(0) emitp_reg i.res.(0)
    | Lop(Intop (Iclz _ as op)) ->
        let instr = name_for_int_operation op in
        emitp_format out "	%a	%a, %a\n" emitp_string instr emitp_reg i.res.(0) emitp_reg i.arg.(0)
    | Lop(Intop ((Iadd|Isub|Imul|Idiv|Iand|Ior|Ixor|Ilsl|Ilsr|Iasr|Ipopcnt) as op)) ->
        let instr = name_for_int_operation op in
        emitp_format out "	%a	%a, %a, %a\n" emitp_string instr emitp_reg i.res.(0) emitp_reg i.arg.(0) emitp_reg i.arg.(1)
    | Lop(Intop_imm(op, n)) ->
        let instr = name_for_int_operation op in
        emitp_format out "	%a	%a, %a, #%a\n" emitp_string instr emitp_reg i.res.(0) emitp_reg i.arg.(0) emitp_int n
    | Lop(Specific Isqrtf) ->
      emitp_format out "	fsqrt	%a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0)
    | Lop(Floatop ((Float32 | Float64), Iabsf)) ->
      emitp_format out "	fabs	%a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0)
    | Lop(Floatop ((Float32 | Float64), Inegf)) ->
      emitp_format out "	fneg	%a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0)
    | Lop(Floatop ((Float32 | Float64), Iaddf)) ->
     emitp_format out "	fadd	%a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0) emitp_reg i.arg.(1)
    | Lop(Floatop ((Float32 | Float64), Isubf)) ->
     emitp_format out "	fsub	%a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0) emitp_reg i.arg.(1)
    | Lop(Floatop ((Float32 | Float64), Imulf)) ->
     emitp_format out "	fmul	%a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0) emitp_reg i.arg.(1)
    | Lop(Floatop ((Float32 | Float64), Idivf)) ->
     emitp_format out "	fdiv	%a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0) emitp_reg i.arg.(1)
    | Lop(Specific Inegmulf) ->
     emitp_format out "	fnmul	%a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0) emitp_reg i.arg.(1)
    | Lop(Specific(Imuladdf)) ->
     emitp_format out "	fmadd	%a, %a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(1) emitp_reg i.arg.(2) emitp_reg i.arg.(0)
    | Lop(Specific(Inegmuladdf)) ->
     emitp_format out "	fnmadd	%a, %a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(1) emitp_reg i.arg.(2) emitp_reg i.arg.(0)
    | Lop(Specific(Imulsubf)) ->
     emitp_format out "	fmsub	%a, %a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(1) emitp_reg i.arg.(2) emitp_reg i.arg.(0)
    | Lop(Specific(Inegmulsubf)) ->
     emitp_format out "	fnmsub	%a, %a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(1) emitp_reg i.arg.(2) emitp_reg i.arg.(0)
    | Lop(Opaque) ->
        assert (i.arg.(0).loc = i.res.(0).loc)
    | Lop(Specific(Ishiftarith(op, shift))) ->
        let instr = (match op with
                       Ishiftadd    -> "add"
                     | Ishiftsub    -> "sub") in
        emitp_format out "	%a	%a, %a, %a" emitp_string instr emitp_reg i.res.(0) emitp_reg i.arg.(0) emitp_reg i.arg.(1);
        if shift >= 0
        then emitp_format out ", lsl #%a\n" emitp_int shift
        else emitp_format out ", asr #%a\n" emitp_int (-shift)
    | Lop(Specific(Imuladd)) ->
      emitp_format out "	madd	%a, %a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0) emitp_reg i.arg.(1) emitp_reg i.arg.(2)
    | Lop(Specific(Imulsub)) ->
      emitp_format out "	msub	%a, %a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0) emitp_reg i.arg.(1) emitp_reg i.arg.(2)
    | Lop(Specific(Ibswap { bitwidth })) ->
        begin match bitwidth with
        | Sixteen ->
            emitp_format out "	rev16	%a, %a\n" emitp_wreg i.res.(0) emitp_wreg i.arg.(0);
            emitp_format out "	ubfm	%a, %a, #0, #15\n" emitp_reg i.res.(0) emitp_reg i.res.(0)
        | Thirtytwo ->
            emitp_format out "	rev	%a, %a\n" emitp_wreg i.res.(0) emitp_wreg i.arg.(0)
        | Sixtyfour ->
            emitp_format out "	rev	%a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0)
        end
    | Lop(Specific(Isignext size)) ->
        emitp_format out "	sbfm	%a, %a, #0, #%a\n" emitp_reg i.res.(0) emitp_reg i.arg.(0) emitp_int (size - 1)
    | Lop(Specific(Isimd simd)) ->
      DSL.simd_instr simd i
    | Lop (Name_for_debugger _) -> ()
    | Lcall_op (Lprobe _) | Lop (Probe_is_enabled _) ->
      fatal_error ("Probes not supported.")
    | Lop(Dls_get) ->
      if Config.runtime5 then
        let offset = Domainstate.(idx_of_field Domain_dls_root) * 8 in
        emitp_format out "	ldr	%a, [%a, %a]\n" emitp_reg i.res.(0) emitp_reg reg_domain_state_ptr emitp_int offset
      else Misc.fatal_error "Dls is not supported in runtime4."
    | Lop (Csel tst) ->
      let len = Array.length i.arg in
      let ifso = i.arg.(len - 2) in
      let ifnot = i.arg.(len - 1) in
      if Reg.same_loc ifso ifnot then
        move ifso i.res.(0)
      else
        begin match tst with
        | Itruetest ->
            emitp_format out "	cmp	%a, #0\n" emitp_reg i.arg.(0);
            emitp_format out "	csel	%a, %a, %a, ne\n" emitp_reg i.res.(0) emitp_reg i.arg.(1) emitp_reg i.arg.(2)
        | Ifalsetest ->
            emitp_format out "	cmp	%a, #0\n" emitp_reg i.arg.(0);
            emitp_format out "	csel	%a, %a, %a, eq\n" emitp_reg i.res.(0) emitp_reg i.arg.(1) emitp_reg i.arg.(2)
        | Iinttest cmp ->
            let comp = name_for_comparison cmp in
            emitp_format out "	cmp	%a, %a\n" emitp_reg i.arg.(0) emitp_reg i.arg.(1);
            emitp_format out "	csel	%a, %a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(2) emitp_reg i.arg.(3) emitp_string comp
        | Iinttest_imm(cmp, n) ->
            let comp = name_for_comparison cmp in
            emitp_cmpimm i.arg.(0) n;
            emitp_format out "	csel	%a, %a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(1) emitp_reg i.arg.(2) emitp_string comp
        | Ifloattest ((Float32 | Float64), cmp) ->
            let comp = name_for_float_comparison cmp in
            emitp_format out "	fcmp	%a, %a\n" emitp_reg i.arg.(0) emitp_reg i.arg.(1);
            emitp_format out "	csel	%a, %a, %a, %a\n" emitp_reg i.res.(0) emitp_reg i.arg.(2) emitp_reg i.arg.(3) emitp_string comp
        | Ioddtest ->
            emitp_format out "	tst	%a, #1\n" emitp_reg i.arg.(0);
            emitp_format out "	csel	%a, %a, %a, ne\n" emitp_reg i.res.(0) emitp_reg i.arg.(1) emitp_reg i.arg.(2)
        | Ieventest ->
            emitp_format out "	tst	%a, #1\n" emitp_reg i.arg.(0);
            emitp_format out "	csel	%a, %a, %a, eq\n" emitp_reg i.res.(0) emitp_reg i.arg.(1) emitp_reg i.arg.(2)
        end
    | Lreloadretaddr ->
        ()
    | Lreturn ->
        output_epilogue (fun () -> emitp_format out "	ret\n")
    | Llabel { label = lbl; _ } ->
        emitp_format out "%a:\n" emitp_label lbl
    | Lbranch lbl ->
        emitp_format out "	b	%a\n" emitp_label lbl
    | Lcondbranch(tst, lbl) ->
        begin match tst with
        | Itruetest ->
            emitp_format out "	cbnz	%a, %a\n" emitp_reg i.arg.(0) emitp_label lbl
        | Ifalsetest ->
            emitp_format out "	cbz	%a, %a\n" emitp_reg i.arg.(0) emitp_label lbl
        | Iinttest cmp ->
            emitp_format out "	cmp	%a, %a\n" emitp_reg i.arg.(0) emitp_reg i.arg.(1);
            let comp = name_for_comparison cmp in
            emitp_format out "	b.%a	%a\n" emitp_string comp emitp_label lbl
        | Iinttest_imm(cmp, n) ->
            emitp_cmpimm i.arg.(0) n;
            let comp = name_for_comparison cmp in
            emitp_format out "	b.%a	%a\n" emitp_string comp emitp_label lbl
        | Ifloattest ((Float32 | Float64), cmp) ->
            let comp = name_for_float_comparison cmp in
            emitp_format out "	fcmp	%a, %a\n" emitp_reg i.arg.(0) emitp_reg i.arg.(1);
            emitp_format out "	b.%a	%a\n" emitp_string comp emitp_label lbl
        | Ioddtest ->
            emitp_format out "	tbnz	%a, #0, %a\n" emitp_reg i.arg.(0) emitp_label lbl
        | Ieventest ->
            emitp_format out "	tbz	%a, #0, %a\n" emitp_reg i.arg.(0) emitp_label lbl
        end
    | Lcondbranch3(lbl0, lbl1, lbl2) ->
        emitp_format out "	cmp	%a, #1\n" emitp_reg i.arg.(0);
        begin match lbl0 with
          None -> ()
        | Some lbl -> emitp_format out "	b.lt	%a\n" emitp_label lbl
        end;
        begin match lbl1 with
          None -> ()
        | Some lbl -> emitp_format out "	b.eq	%a\n" emitp_label lbl
        end;
        begin match lbl2 with
          None -> ()
        | Some lbl -> emitp_format out "	b.gt	%a\n" emitp_label lbl
        end
    | Lswitch jumptbl ->
        let lbltbl = Cmm.new_label() in
        emitp_format out "	adr	%a, %a\n" emitp_reg reg_tmp1 emitp_label lbltbl;
        emitp_format out "	add	%a, %a, %a, lsl #2\n" emitp_reg reg_tmp1 emitp_reg reg_tmp1 emitp_reg i.arg.(0);
        emitp_format out "	br	%a\n" emitp_reg reg_tmp1;
        emitp_format out "%a:" emitp_label lbltbl;
        for j = 0 to Array.length jumptbl - 1 do
            emitp_format out "	b	%a\n" emitp_label jumptbl.(j)
        done
(* Alternative:
        let lbltbl = Cmm.new_label() in
        emitp_format out "	adr	%a, %a\n" emitp_reg reg_tmp1 emitp_label lbltbl;
        emitp_format out "	ldr	%a, [%a, %a, lsl #2]\n" emitp_wreg reg_tmp2 emitp_reg reg_tmp1 emitp_reg i.arg.(0);
        emitp_format out "	add	%a, %a, sxtb\n" emitp_reg reg_tmp1 emitp_wreg reg_tmp2;
        emitp_format out "	br	%a\n" emitp_reg reg_tmp1;
        emitp_format out "%a:\n" emitp_label lbltbl;
        for j = 0 to Array.length jumptbl - 1 do
            emitp_format out "	.word	%a - %a\n" emitp_label jumptbl.(j) emitp_label lbltbl
        done
*)
    | Lentertrap ->
        ()
    | Ladjust_stack_offset { delta_bytes } ->
        cfi_adjust_cfa_offset delta_bytes;
        stack_offset := !stack_offset + delta_bytes
    | Lpushtrap { lbl_handler; } ->
        emitp_format out "	adr	%a, %a\n" emitp_reg reg_tmp1 emitp_label lbl_handler;
        stack_offset := !stack_offset + 16;
        emitp_format out "	stp	%a, %a, [sp, -16]!\n" emitp_reg reg_trap_ptr emitp_reg reg_tmp1;
        cfi_adjust_cfa_offset 16;
        emitp_format out "	mov	%a, sp\n" emitp_reg reg_trap_ptr
    | Lpoptrap ->
        emitp_format out "	ldr	%a, [sp], 16\n" emitp_reg reg_trap_ptr;
        cfi_adjust_cfa_offset (-16);
        stack_offset := !stack_offset - 16
    | Lraise k ->
        begin match k with
        | Lambda.Raise_regular ->
          emitp_format out "	bl	%a\n" emitp_symbol "caml_raise_exn";
          emitp_format out "%a\n" record_frame Reg.Set.empty (Dbg_raise i.dbg)
        | Lambda.Raise_reraise ->
          if Config.runtime5 then
            emitp_format out "	bl	%a\n" emitp_symbol "caml_reraise_exn"
          else
            emitp_format out "	bl	%a\n" emitp_symbol "caml_raise_exn";
          emitp_format out "%a\n" record_frame Reg.Set.empty (Dbg_raise i.dbg)
        | Lambda.Raise_notrace ->
          emitp_format out "	mov	sp, %a\n" emitp_reg reg_trap_ptr;
          emitp_format out "	ldp	%a, %a, [sp], 16\n" emitp_reg reg_trap_ptr emitp_reg reg_tmp1;
          emitp_format out "	br	%a\n" emitp_reg reg_tmp1
      end
    | Lstackcheck { max_frame_size_bytes; } ->
      let overflow = Cmm.new_label () and ret = Cmm.new_label () in
      let threshold_offset =
        Domainstate.stack_ctx_words * 8 + Stack_check.stack_threshold_size
      in
      let f = max_frame_size_bytes + threshold_offset in
      let offset = Domainstate.(idx_of_field Domain_current_stack) * 8 in
      emitp_format out "	ldr	%a, [%a, #%a]\n" emitp_reg reg_tmp1 emitp_reg reg_domain_state_ptr emitp_int offset;
      emitp_addimm reg_tmp1 reg_tmp1 f;
      emitp_format out "	cmp	sp, %a\n" emitp_reg reg_tmp1;
      emitp_format out "	bcc	%a\n" emitp_label overflow;
      emitp_format out "%a:" emitp_label ret;
      stack_realloc := Some {
        sc_label = overflow;
        sc_return = ret;
        sc_max_frame_size_in_bytes = max_frame_size_bytes;
      }

let emitp_instr i =
  try emitp_instr i
  with exn -> (
    Format.eprintf "Exception whilst emitting instruction:@ %a\n"
      Printlinear.instr i;
    raise exn
  )

(* Emission of an instruction sequence *)

let rec emitp_all i =
  if i.desc = Lend then () else (emitp_instr i; emitp_all i.next)

(* Emission of a function declaration *)

let fundecl fundecl =
  let fun_end_label, fundecl =
    match Emitaux.Dwarf_helpers.record_dwarf_for_fundecl fundecl with
    | None -> None, fundecl
    | Some { fun_end_label; fundecl } -> Some fun_end_label, fundecl
  in
  function_name := fundecl.fun_name;
  fastcode_flag := fundecl.fun_fast;
  tailrec_entry_point := fundecl.fun_tailrec_entry_point_label;
  float_literals := [];
  stack_offset := 0;
  call_gc_sites := [];
  local_realloc_sites := [];
  clear_stack_realloc ();
  for i = 0 to Proc.num_stack_slot_classes - 1 do
    num_stack_slots.(i) <- fundecl.fun_num_stack_slots.(i);
  done;
  prologue_required := fundecl.fun_prologue_required;
  contains_calls := fundecl.fun_contains_calls;
  emitp_named_text_section !function_name;
  emitp_format out "	.align	3\n";
  emitp_format out "	.globl	%a\n" emitp_symbol fundecl.fun_name;
  emitp_symbol_type emitp_symbol fundecl.fun_name "function";
  emitp_format out "%a:\n" emitp_symbol fundecl.fun_name;
  emitp_debug_info fundecl.fun_dbg;
  cfi_startproc();
  let num_call_gc = num_call_gc_points fundecl.fun_body in
  let max_out_of_line_code_offset = max_out_of_line_code_offset ~num_call_gc in
  BR.relax fundecl.fun_body ~max_out_of_line_code_offset;

  emitp_all fundecl.fun_body;
  List.iter emitp_call_gc !call_gc_sites;
  List.iter emitp_local_realloc !local_realloc_sites;
  emitp_stack_realloc ();
  assert (List.length !call_gc_sites = num_call_gc);
  (match fun_end_label with
   | None -> ()
   | Some fun_end_label -> emitp_format out "%a:\n" emitp_label fun_end_label);

  cfi_endproc();
  emitp_symbol_type emitp_symbol fundecl.fun_name "function";
  emitp_symbol_size fundecl.fun_name;
  emitp_literals()

(* Emission of data *)

let emitp_item (d : Cmm.data_item) =
  match d with
  | Cdefine_symbol s ->
    if !Clflags.dlcode || s.sym_global = Cmm.Global then begin
      (* GOT relocations against non-global symbols don't seem to work
         properly: GOT entries are not created for the symbols and the
         relocations evaluate to random other GOT entries.  For the moment
         force all symbols to be global. *)
      emitp_format out "	.globl	%a\n" emitp_symbol s.sym_name;
    end;
    emitp_format out "%a:\n" emitp_symbol s.sym_name
  | Cint8 n -> emitp_format out "	.byte	%a\n" emitp_int n
  | Cint16 n -> emitp_format out "	.short	%a\n" emitp_int n
  | Cint32 n -> emitp_format out "	.long	%a\n" emitp_nativeint n
  | Cint n -> emitp_format out "	.quad	%a\n" emitp_nativeint n
  | Csingle f -> emitp_float32_directive ".long" (Int32.bits_of_float f)
  | Cdouble f -> emitp_float64_directive ".quad" (Int64.bits_of_float f)
  | Cvec128 { high; low; } ->
     emitp_float64_directive ".quad" low;
     emitp_float64_directive ".quad" high;
  | Csymbol_address s -> emitp_format out "	.quad	%a\n" emitp_symbol s.sym_name
  | Csymbol_offset (s, o) -> emitp_format out "	.quad	%a+%a\n" emitp_symbol s.sym_name emitp_int o
  | Cstring s -> emitp_string_directive "	.ascii  " s
  | Cskip n -> if n > 0 then emitp_format out "	.space	%a\n" emitp_int n
  | Calign n -> emitp_format out "	.align	%a\n" emitp_int(Misc.log2 n)

let data l =
  emitp_format out "	.data\n";
  emitp_format out "	.align  3\n";
  List.iter emitp_item l

let emitp_line str = emitp_string (str ^ "\n")

let file_emitter ~file_num ~file_name =
  emitp_line (Printf.sprintf ".file %d %S" file_num file_name)

let build_asm_directives () : (module Asm_targets.Asm_directives_intf.S) = (
  module Asm_targets.Asm_directives.Make(struct
    let emitp_line = emitp_line

    let get_file_num file_name =
      Emitaux.get_file_num ~file_emitter file_name

    let debugging_comments_in_asm_files =
      !Flambda_backend_flags.dasm_comments

    module D = struct
      type constant =
        | Int64 of Int64.t
        | Label of string
        | Add of constant * constant
        | Sub of constant * constant

      let rec string_of_constant const =
        match const with
        | Int64 n -> Int64.to_string n
        | Label s -> s
        | Add (c1, c2) ->
          Printf.sprintf "(%s + %s)"
            (string_of_constant c1) (string_of_constant c2)
        | Sub (c1, c2) ->
          Printf.sprintf "(%s - %s)"
            (string_of_constant c1) (string_of_constant c2)

      let const_int64 num = Int64 num
      let const_label str = Label str
      let const_add c1 c2 = Add (c1, c2)
      let const_sub c1 c2 = Sub (c1, c2)

      type data_type =
        | NONE
        | DWORD
        | QWORD
        | VEC128

      let file = file_emitter

      let loc ~file_num ~line ~col ?discriminator () =
        ignore discriminator;
        emitp_line (Printf.sprintf ".loc %d %d %d" file_num line col)

      let comment str =
        emitp_line (Printf.sprintf "; %s" str)

      let label ?data_type str =
        let _ = data_type in
        emitp_line (Printf.sprintf "%s:" str)

      let section ?delayed:_ name flags args =
        match name, flags, args with
        | [".data" ], _, _ -> emitp_line "\t.data"
        | [".text" ], _, _ -> emitp_line "\t.text"
        | name, flags, args ->
          emitp_string (Printf.sprintf "\t.section %s"
            (String.concat "," name));
          begin match flags with
          | None -> ()
          | Some flags -> emitp_string (Printf.sprintf ",%S" flags)
          end;
          begin match args with
          | [] -> ()
          | _ ->
            emitp_string (Printf.sprintf ",%s" (String.concat "," args))
          end;
          emitp_string "\n"

      let text () = emitp_line "\t.text"

      let new_line () = emitp_line ""

      let global sym = emitp_line (Printf.sprintf "\t.globl %s" sym)

      let protected sym =
        if not macosx then emitp_line (Printf.sprintf "\t.protected %s" sym)

      let type_ sym typ_ = emitp_line (Printf.sprintf "\t.type %s,%s" sym typ_)

      let byte const =
        emitp_line
          (Printf.sprintf "\t.byte %s" (string_of_constant const))

      let word const =
        emitp_line
          (Printf.sprintf "\t.short %s" (string_of_constant const))

      let long const =
        emitp_line
          (Printf.sprintf "\t.long %s" (string_of_constant const))

      let qword const =
        emitp_line
          (Printf.sprintf "\t.quad %s" (string_of_constant const))

      let bytes str =
        emitp_line (Printf.sprintf "\t.ascii %S" str)

      let uleb128 const =
        emitp_line
          (Printf.sprintf "\t.uleb128 %s" (string_of_constant const))

      let sleb128 const =
        emitp_line
          (Printf.sprintf "\t.sleb128 %s" (string_of_constant const))

      let direct_assignment var const =
        emitp_line
          (Printf.sprintf "\t.set %s,%s" var (string_of_constant const))
    end
  end)
)

(* Beginning / end of an assembly file *)

let begin_assembly _unix =
  reset_debug_info();
  emitp_format out "	.file	\"\"\n";  (* PR#7037 *)
  let lbl_begin = Cmm_helpers.make_symbol "data_begin" in
  emitp_format out "	.data\n";
  emitp_format out "	.globl	%a\n" emitp_symbol lbl_begin;
  emitp_format out "%a:\n" emitp_symbol lbl_begin;
  let lbl_begin = Cmm_helpers.make_symbol "code_begin" in
  emitp_named_text_section lbl_begin;
  emitp_format out "	.globl	%a\n" emitp_symbol lbl_begin;
  emitp_format out "%a:\n" emitp_symbol lbl_begin;
  (* we need to pad here to avoid collision for the unwind test between
     the code_begin symbol and the first function. (See also #4690)
     Alignment is needed to avoid linker warnings for
     shared_startup__code_{begin,end} (e.g. tests/lib-dynlink-pr4839).
   *)
  if macosx then begin
    emitp_format out "	nop\n";
    emitp_format out "	.align	3\n"
  end;
  let lbl_end = Cmm_helpers.make_symbol "code_end" in
  Emitaux.Dwarf_helpers.begin_dwarf ~build_asm_directives
    ~code_begin:lbl_begin ~code_end:lbl_end
    ~file_emitter

let end_assembly () =
  let lbl_end = Cmm_helpers.make_symbol "code_end" in
  emitp_named_text_section lbl_end;
  emitp_format out "	.globl	%a\n" emitp_symbol lbl_end;
  emitp_format out "%a:\n" emitp_symbol lbl_end;
  let lbl_end = Cmm_helpers.make_symbol "data_end" in
  emitp_format out "	.data\n";
  emitp_format out "	.quad	0\n";  (* PR#6329 *)
  emitp_format out "	.globl	%a\n" emitp_symbol lbl_end;
  emitp_format out "%a:\n" emitp_symbol lbl_end;
  emitp_format out "	.quad	0\n";
  emitp_format out "	.align	3\n";  (* #7887 *)
  let lbl = Cmm_helpers.make_symbol "frametable" in
  emitp_format out "	.globl	%a\n" emitp_symbol lbl;
  emitp_format out "%a:\n" emitp_symbol lbl;
  emitp_frames
    { efa_code_label = (fun lbl ->
                       emitp_symbol_type emitp_label lbl "function";
                       emitp_format out "	.quad	%a\n" emitp_label lbl);
      efa_data_label = (fun lbl ->
                       emitp_symbol_type emitp_label lbl "object";
                       emitp_format out "	.quad	%a\n" emitp_label lbl);
      efa_8 = (fun n -> emitp_format out "	.byte	%a\n" emitp_int n);
      efa_16 = (fun n -> emitp_format out "	.short	%a\n" emitp_int n);
      efa_32 = (fun n -> emitp_format out "	.long	%a\n" emitp_int32 n);
      efa_word = (fun n -> emitp_format out "	.quad	%a\n" emitp_int n);
      efa_align = (fun n -> emitp_format out "	.align	%a\n" emitp_int(Misc.log2 n));
      efa_label_rel = (fun lbl ofs ->
                           emitp_format out "	.long	%a - . + %a\n" emitp_label lbl emitp_int32 ofs);
      efa_def_label = (fun lbl -> emitp_format out "%a:\n" emitp_label lbl);
      efa_string = (fun s -> emitp_string_directive "	.asciz	" s) };
  emitp_symbol_type emitp_symbol lbl "object";
  emitp_symbol_size lbl;
  if not !Flambda_backend_flags.internal_assembler then
    Emitaux.Dwarf_helpers.emitp_dwarf ();
  begin match Config.system with
  | "linux" ->
      (* Mark stack as non-executable *)
      emitp_format out "	.section	.note.GNU-stack,\"\",%%progbits\n"
  | _ -> ()
  end
