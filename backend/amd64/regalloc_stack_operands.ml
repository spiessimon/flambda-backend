[@@@ocaml.warning "+a-40-41-42"]

open! Regalloc_utils
open! Int_replace_polymorphic_compare

let debug = false

let may_use_stack_operand_for_second_argument
  : type a . num_args:int -> res_is_fst:bool ->
             spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun ~num_args ~res_is_fst map instr ->
  if debug then begin
    check_lengths instr ~of_arg:num_args ~of_res:1;
    if res_is_fst then begin
      check_same "res(0)" instr.res.(0) "arg(0)" instr.arg.(0);
    end;
  end;
  begin match is_spilled map instr.arg.(1) with
  | false -> ()
  | true ->
    use_stack_operand map instr.arg 1;
  end;
  May_still_have_spilled_registers

let may_use_stack_operand_for_only_argument
  : type a . has_result:bool -> spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun ~has_result map instr ->
  if debug then check_lengths instr ~of_arg:1 ~of_res:(if has_result then 1 else 0);
  begin match is_spilled map instr.arg.(0) with
  | false -> ()
  | true ->
    use_stack_operand map instr.arg 0
  end;
  if has_result then
    May_still_have_spilled_registers
  else
    All_spilled_registers_rewritten

let may_use_stack_operand_for_only_result
  : type a . spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun map instr ->
  if debug then check_lengths instr ~of_arg:0 ~of_res:1;
  begin match is_spilled map instr.res.(0) with
  | false ->
    All_spilled_registers_rewritten
  | true ->
    use_stack_operand map instr.res 0;
    All_spilled_registers_rewritten
  end

let may_use_stack_operand_for_result
  : type a . num_args:int -> spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun ~num_args map instr ->
  if debug then check_lengths instr ~of_arg:num_args ~of_res:1;
  begin match is_spilled map instr.res.(0) with
  | false -> ()
  | true ->
    if Reg.same instr.arg.(0) instr.res.(0) then begin
      use_stack_operand map instr.arg 0;
    end;
    use_stack_operand map instr.res 0;
  end;
  May_still_have_spilled_registers

type result =
  | Result_can_be_on_stack
  | Result_cannot_be_on_stack

let is_stack_operand : Reg.t -> bool =
  fun reg ->
    match reg.loc with
    | Stack _ -> true
    | Unknown | Reg _ -> false

let binary_operation
  : type a . spilled_map -> a Cfg.instruction -> result -> stack_operands_rewrite
  = fun map instr result ->
  if debug then begin
    match result with
    | Result_can_be_on_stack ->
      check_lengths instr ~of_arg:2 ~of_res:1;
      check_same "res(0)" instr.res.(0) "arg(0)" instr.arg.(0)
    | Result_cannot_be_on_stack ->
      check_lengths instr ~of_arg:2 ~of_res:1
  end;
  let already_has_memory_operand =
    is_stack_operand instr.arg.(0)
    || is_stack_operand instr.arg.(1)
    || (match result with
      | Result_cannot_be_on_stack ->
        assert (not (is_stack_operand instr.res.(0)));
        false
      | Result_can_be_on_stack ->
        (* note: actually unreachable since instr.res.(0) and
           instr.arg.(0) are the same. *)
        is_stack_operand instr.res.(0))
  in
  if already_has_memory_operand then
    May_still_have_spilled_registers
  else begin
    match is_spilled map instr.arg.(0), is_spilled map instr.arg.(1) with
    | false, false ->
      begin match result with
      | Result_can_be_on_stack ->
        All_spilled_registers_rewritten
      | Result_cannot_be_on_stack ->
        May_still_have_spilled_registers
      end
    | false, true ->
      use_stack_operand map instr.arg 1;
      begin match result with
      | Result_can_be_on_stack | Result_cannot_be_on_stack ->
        May_still_have_spilled_registers
      end
    | true, false ->
      (* note: slightly different from the case above, because arg.(0) and res.(0) are the same. *)
      begin match result with
      | Result_can_be_on_stack ->
        use_stack_operand map instr.res 0;
        use_stack_operand map instr.arg 0;
        All_spilled_registers_rewritten
      | Result_cannot_be_on_stack ->
        use_stack_operand map instr.arg 0;
        May_still_have_spilled_registers
      end;
    | true, true ->
      if Reg.same instr.arg.(0) instr.arg.(1) then
        May_still_have_spilled_registers
      else begin
        match result with
        | Result_can_be_on_stack ->
          use_stack_operand map instr.arg 0;
          use_stack_operand map instr.res 0;
          May_still_have_spilled_registers
        | Result_cannot_be_on_stack ->
          use_stack_operand map instr.arg 0;
          May_still_have_spilled_registers
      end
  end

let unary_operation_argument_or_result_on_stack
  : type a . spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun map instr ->
  if debug then check_lengths instr ~of_arg:1 ~of_res:1;
  if is_stack_operand instr.arg.(0) || is_stack_operand instr.res.(0)
  then May_still_have_spilled_registers
  else match is_spilled map instr.arg.(0), is_spilled map instr.res.(0) with
  | false, false -> All_spilled_registers_rewritten
  | false, true ->
    use_stack_operand map instr.res 0;
    All_spilled_registers_rewritten
  | true, false ->
    use_stack_operand map instr.arg 0;
    All_spilled_registers_rewritten
  | true, true ->
    if Reg.same instr.arg.(0) instr.res.(0)
    then May_still_have_spilled_registers
    else begin
      use_stack_operand map instr.arg 0;
      May_still_have_spilled_registers
    end

let basic (map : spilled_map) (instr : Cfg.basic Cfg.instruction) =
  begin match instr.desc with
  | Op (Floatop (_, (Iaddf | Isubf | Imulf | Idivf))) ->
    may_use_stack_operand_for_second_argument map instr ~num_args:2 ~res_is_fst:true
  | Op (Specific Ipackf32) -> May_still_have_spilled_registers
  | Op (Specific (Isimd (Instruction { instr = simd; _ })))
  | Op (Specific (Isimd (Sequence { seq = { instr = simd; id =
          Sqrtss | Sqrtsd | Roundss | Roundsd |
          Pcmpestra | Pcmpestrc | Pcmpestro | Pcmpestrs | Pcmpestrz |
          Pcmpistra | Pcmpistrc | Pcmpistro | Pcmpistrs | Pcmpistrz }; _ }))) ->
    (match Array.length simd.args, simd.res with
    | 1, First_arg -> May_still_have_spilled_registers
    | 1, Res { loc = res_loc; _ } ->
      let arg_mem = Simd.loc_allows_mem simd.args.(0).loc in
      let res_mem = Simd.loc_allows_mem res_loc in
      assert (not (arg_mem && res_mem));
      if arg_mem then may_use_stack_operand_for_only_argument map instr ~has_result:true
      else if res_mem then may_use_stack_operand_for_result map instr ~num_args:1
      else May_still_have_spilled_registers
    | num_args, First_arg ->
      if Simd.loc_allows_mem simd.args.(1).loc
      then may_use_stack_operand_for_second_argument map instr ~num_args ~res_is_fst:true
      else May_still_have_spilled_registers
    | num_args, Res { loc = res_loc; _ } ->
      let arg_mem = Simd.loc_allows_mem simd.args.(1).loc in
      let res_mem = Simd.loc_allows_mem res_loc in
      assert (not (arg_mem && res_mem));
      if arg_mem then may_use_stack_operand_for_second_argument map instr ~num_args ~res_is_fst:false
      else if res_mem then may_use_stack_operand_for_result map instr ~num_args
      else May_still_have_spilled_registers)
  | Op (Specific (Isimd_mem ((SSE2 Add_f64 | SSE2 Sub_f64 | SSE2 Mul_f64 | SSE2 Div_f64 |
                              SSE Add_f32 | SSE Sub_f32 | SSE Mul_f32 | SSE Div_f32), _))) ->
    May_still_have_spilled_registers
  | Op (Reinterpret_cast (Float_of_float32 | Float32_of_float | V128_of_v128))
  | Op (Static_cast (V128_of_scalar Float64x2 | Scalar_of_v128 Float64x2))
  | Op (Static_cast (V128_of_scalar Float32x4 | Scalar_of_v128 Float32x4)) ->
    unary_operation_argument_or_result_on_stack map instr
  | Op (Reinterpret_cast (Float_of_int64 | Float32_of_int32))
  | Op (Static_cast (V128_of_scalar (Int64x2 | Int32x4 | Int16x8 | Int8x16))) ->
    may_use_stack_operand_for_only_argument map instr ~has_result:true
  | Op (Reinterpret_cast (Int64_of_float | Int32_of_float32))
  | Op (Static_cast (Scalar_of_v128 (Int64x2 | Int32x4))) ->
    may_use_stack_operand_for_result map instr ~num_args:1
  | Op (Static_cast (Scalar_of_v128 (Int16x8 | Int8x16))) ->
    (* CR mslater: (SIMD) replace once we have unboxed int16/int8 *)
    May_still_have_spilled_registers
  | Op (Static_cast (Float_of_int (Float32 | Float64) |
                     Int_of_float (Float32 | Float64) |
                     Float_of_float32 | Float32_of_float)) ->
    may_use_stack_operand_for_only_argument map instr ~has_result:true
  | Op (Const_symbol _) ->
    if !Clflags.pic_code || !Clflags.dlcode || Arch.win64 then
      May_still_have_spilled_registers
    else
      may_use_stack_operand_for_only_result map instr
  | Op (Const_int n) ->
    if (Nativeint.compare n 0x7FFFFFFFn) <= 0 && (Nativeint.compare n (-0x80000000n)) >= 0 then begin
      may_use_stack_operand_for_only_result map instr
    end else begin
      May_still_have_spilled_registers
    end
  | Op (Intop (Iadd | Isub | Iand | Ior | Ixor)) ->
    binary_operation map instr Result_can_be_on_stack
  | Op (Intop (Icomp _)) ->
    binary_operation map instr Result_cannot_be_on_stack
  | Op (Intop_imm (Icomp _, _)) ->
    may_use_stack_operand_for_only_argument map instr ~has_result:true
  | Op (Intop_imm (Iadd, _)) ->
    (* Conservatively assume it will be turned into a `lea` instruction,
       and ask for everything to be in registers. *)
    May_still_have_spilled_registers
  | Op (Intop(Ilsl | Ilsr | Iasr)) ->
    may_use_stack_operand_for_result map instr ~num_args:2
  | Op(Intop_imm((Isub | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr), _)) ->
    may_use_stack_operand_for_result map instr ~num_args:1
  | Op (Csel _) (* CR gyorsh: optimize *)
  | Op (Specific (Ilfence | Isfence | Imfence))
  | Op (Intop(Imulh _ | Imul | Idiv | Imod))
  | Op (Intop_imm ((Imulh _ | Imul | Idiv | Imod), _))
  | Op (Specific (Irdtsc | Irdpmc))
  | Op (Intop (Ipopcnt | Iclz _| Ictz _))
  | Op (Intop_atomic _)
  | Op (Move | Spill | Reload | Floatop (_, (Inegf | Iabsf | Icompf _))
       | Const_float _ | Const_float32 _  | Const_vec128 _
       | Stackoffset _ | Load _ | Store _ | Name_for_debugger _ | Probe_is_enabled _
       | Opaque | Begin_region | End_region | Dls_get | Poll | Alloc _)
  | Op (Reinterpret_cast (Int_of_value | Value_of_int))
  | Op (Specific (Isextend32 | Izextend32 | Ilea _
                 | Istore_int (_, _, _)
                 | Ioffset_loc (_, _) | Ifloatarithmem (_, _, _)
                 | Ipause
                 | Icldemote _
                 | Iprefetch _
                 | Ibswap _))
  | Reloadretaddr
  | Pushtrap _
  | Poptrap _
  | Prologue ->
    (* no rewrite *)
    May_still_have_spilled_registers
  | Op (Intop_imm ((Ipopcnt | Iclz _ | Ictz _ ), _)) | Stack_check _ ->
    (* should not happen *)
    fatal "unexpected instruction"
  end

let terminator (map : spilled_map) (term : Cfg.terminator Cfg.instruction) =
  ignore map;
  match (term : Cfg.terminator Cfg.instruction).desc with
  | Never -> fatal "unexpected terminator"
  | Int_test { lt = _; eq = _; gt =_; is_signed = _; imm = None; }
  | Int_test { lt = _; eq = _; gt =_; is_signed = _; imm = Some _; }
  | Parity_test { ifso = _; ifnot = _; }
  | Truth_test { ifso = _; ifnot = _; }
  | Float_test _ ->
    (* CR-someday xclerc for xclerc: this could be optimized, but the representation
       makes it more difficult than the cases above, because (i) multiple
       branching instructions may be emitted and (ii) the operand constraints
       depend on the exact kind of branch (because we sometimes swap the
       operands). *)
    May_still_have_spilled_registers
  | Always _
  | Return
  | Raise _
  | Switch _
  | Tailcall_self _
  | Tailcall_func _
  | Call_no_return _
  | Prim {op = External _; _ } | Call {op = Indirect | Direct _; _} ->
    (* no rewrite *)
    May_still_have_spilled_registers
  | Prim {op = Probe _; _} ->
    may_use_stack_operands_everywhere map term
