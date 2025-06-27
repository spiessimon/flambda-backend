(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Tomasz Nowak and Mark Shinwell, Jane Street Europe           *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-40-42"]

open! Dwarf_low
open! Dwarf_high
module Uid = Flambda2_identifiers.Flambda_debug_uid
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state

(* CR sspies: There must be a better way to create a string table. *)
module StringIdent = Identifiable.Make (struct
  type nonrec t = string

  let compare = Stdlib.compare

  let equal = Stdlib.( = )

  let hash = Hashtbl.hash

  let print = Format.pp_print_string

  let output _oc _t = Misc.fatal_error "unimplemented"
end)

module StringTable = StringIdent.Tbl

let debug_emit_dwarf_dies = false

let debug_die_description_table = StringIdent.Tbl.create 0

let debug_info_add ~reference info =
  if debug_emit_dwarf_dies
  then
    StringTable.add debug_die_description_table
      (Asm_targets.Asm_label.encode reference)
      info

let debug_info_add_alias ~from_ref ~to_ref =
  if debug_emit_dwarf_dies
  then
    let desc = Format.asprintf "-> %s" (Asm_targets.Asm_label.encode to_ref) in
    StringTable.add debug_die_description_table
      (Asm_targets.Asm_label.encode from_ref)
      desc

let debug_info_add_enum ~reference constructors =
  if debug_emit_dwarf_dies
  then
    let desc = Format.asprintf "= %s" (String.concat " | " constructors) in
    StringTable.add debug_die_description_table
      (Asm_targets.Asm_label.encode reference)
      desc

let debug_info_add_ptr ~reference ~inner =
  if debug_emit_dwarf_dies
  then
    let desc = Format.asprintf "%s ptr" (Asm_targets.Asm_label.encode inner) in
    StringTable.add debug_die_description_table
      (Asm_targets.Asm_label.encode reference)
      desc

let emit_debug_info ~die =
  if debug_emit_dwarf_dies
  then
    let indent = ref 0 in
    Proto_die.depth_first_fold die ~init:() ~f:(fun () d ->
        match d with
        | DIE { label; tag; has_children; attribute_values = _; _ } ->
          let indentation = String.make !indent ' ' in
          let info =
            (StringIdent.Tbl.find_opt debug_die_description_table)
              (Asm_targets.Asm_label.encode label)
          in
          Format.eprintf "%s+ %a(%s) %a\n" indentation
            Asm_targets.Asm_label.print label (Dwarf_tag.tag_name tag)
            (Format.pp_print_option Format.pp_print_string)
            info;
          if has_children = Child_determination.Yes then indent := !indent + 2
        | End_of_siblings -> indent := !indent - 2)


let base_layout_to_byte_size (sort : Jkind_types.Sort.base) =
  match sort with
  | Void -> 0
  | Float64 -> 8
  | Float32 -> 4
  | Word -> Arch.size_addr
  | Bits32 -> 4
  | Bits64 -> 8
  | Vec128 -> 16
  | Value -> Arch.size_addr

(* smaller entries in mixed blocks are expanded to be of, at least, word size *)
(* CR sspiess: This handling is incorrect for [Void] layout. Once we support
   putting [Void] data into records, we have to adjust the code below to filter
   out void fields from mixed records and mixed variants. *)
let base_layout_to_byte_size_in_mixed_block (sort : Jkind_types.Sort.base) =
  Int.max (base_layout_to_byte_size sort) Arch.size_addr

let attribute_list_with_optional_name name =
  List.map DAH.create_name (Option.to_list name)

let wrap_die_under_a_pointer ~proto_die ~reference ~parent_proto_die =
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Reference_type
    ~attribute_values:
      [DAH.create_byte_size_exn ~byte_size:8; DAH.create_type ~proto_die]
    ();
  debug_info_add_ptr ~reference ~inner:(Proto_die.reference proto_die)

let create_array_die ~reference ~parent_proto_die ~child_die ?name () =
  let array_die =
    Proto_die.create ~parent:(Some parent_proto_die) ~tag:Dwarf_tag.Array_type
      ~attribute_values:
        ([ DAH.create_type_from_reference ~proto_die_reference:child_die;
           (* We can't use DW_AT_byte_size or DW_AT_bit_size since we don't know
              how large the array might be. *)
           (* DW_AT_byte_stride probably isn't required strictly speaking, but
              let's add it for the avoidance of doubt. *)
           DAH.create_byte_stride
             ~bytes:(Numbers.Int8.of_int_exn Arch.size_addr) ]
        @ attribute_list_with_optional_name name)
      ()
  in
  Proto_die.create_ignore ~parent:(Some array_die) ~tag:Dwarf_tag.Subrange_type
    ~attribute_values:
      [ (* Thankfully, all that lldb cares about is DW_AT_count. *)
        DAH.create_count_const 0L ]
    ();
  wrap_die_under_a_pointer ~proto_die:array_die ~reference ~parent_proto_die

let create_char_die ~reference ~parent_proto_die ?name () =
  (* As a char is an immediate value, we have to ignore the first bit.
     Unfortunately lldb supports bit offsets only on members of structs, so
     instead, we create a hacky enum containing all possible char values. *)
  let enum =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:8]
        @ attribute_list_with_optional_name name)
        (* CR sspies: The name here is displayed as ["enum " ^ name] in gdb, but
           correctly as [name] in lldb. *)
      ()
  in
  debug_info_add ~reference "char enum";
  List.iter
    (fun i ->
      Proto_die.create_ignore ~parent:(Some enum) ~tag:Dwarf_tag.Enumerator
        ~attribute_values:
          [ DAH.create_const_value ~value:(Int64.of_int ((2 * i) + 1));
            DAH.create_name (Printf.sprintf "%C" (Char.chr i)) ]
        ())
    (List.init 256 (fun i -> i))

let create_unboxed_base_layout_die ~reference ~parent_proto_die ?name ~byte_size
    encoding =
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Base_type
    ~attribute_values:
      ([DAH.create_byte_size_exn ~byte_size; DAH.create_encoding ~encoding]
      @ attribute_list_with_optional_name name)
    ()

let create_typedef_die ~reference ~parent_proto_die ?name child_die =
  debug_info_add_alias ~from_ref:reference ~to_ref:child_die;
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Typedef
    ~attribute_values:
      ([DAH.create_type_from_reference ~proto_die_reference:child_die]
      @ attribute_list_with_optional_name name)
    ()

let create_record_die ~reference ~parent_proto_die ?name fields =
  let total_size =
    List.fold_left (fun acc (_, field_size, _) -> acc + field_size) 0 fields
  in
  let structure =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:total_size]
        @ attribute_list_with_optional_name name)
      ()
  in
  let offset = ref 0 in
  List.iter
    (fun (field_name, field_size, field_die) ->
      let field =
        Proto_die.create ~parent:(Some structure) ~tag:Dwarf_tag.Member
          ~attribute_values:
            [ DAH.create_name field_name;
              DAH.create_type_from_reference ~proto_die_reference:field_die;
              DAH.create_data_member_location_offset
                ~byte_offset:(Int64.of_int !offset) ]
          ()
      in
      debug_info_add ~reference:(Proto_die.reference field) ("." ^ field_name);
      offset := !offset + field_size)
    fields;
  wrap_die_under_a_pointer ~proto_die:structure ~reference ~parent_proto_die

(* Contrary to what one might assume from the name, the following function only
   creates one kind of unboxed record. The record for [[@@unboxed]]. The records
   of the form [#{ ... }] are destructed into their component parts by
   unarization. *)
let create_unboxed_record_die ~reference ~parent_proto_die ?name ~field_name
    ~field_size field_die =
  let structure =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:field_size]
        @ attribute_list_with_optional_name name)
      ()
  in
  Proto_die.create_ignore ~parent:(Some structure) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_name field_name;
        DAH.create_type_from_reference ~proto_die_reference:field_die;
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0) ]
    ()

let create_simple_variant_die ~reference ~parent_proto_die ?name
    simple_constructors =
  debug_info_add_enum ~reference simple_constructors;
  let enum =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:8]
        @ attribute_list_with_optional_name name)
      ()
  in
  List.iteri
    (fun i constructor ->
      Proto_die.create_ignore ~parent:(Some enum) ~tag:Dwarf_tag.Enumerator
        ~attribute_values:
          [ DAH.create_const_value ~value:(Int64.of_int ((2 * i) + 1));
            DAH.create_name constructor ]
        ())
    simple_constructors

let reorder_record_fields_for_mixed_record
    ~(mixed_block_shapes : Shape.base_layout array) fields =
  (* We go into arrays and back, because it makes the reordering of the fields
     via accesses O(n) instead of O(n^2) *)
  let fields = Array.of_list fields in
  let mixed_block_shapes =
    Array.map
      (function
        | Jkind_types.Sort.Value -> Types.Value
        | Jkind_types.Sort.Float64 ->
          Types.Float64
          (* This is a case, where we potentially have mapped [Float_boxed] to
             [Float64], but that is fine, because they are reordered like other
             mixed fields. *)
          (* CR sspies: Is this really true? *)
        | Jkind_types.Sort.Float32 -> Types.Float32
        | Jkind_types.Sort.Bits32 -> Types.Bits32
        | Jkind_types.Sort.Bits64 -> Types.Bits64
        | Jkind_types.Sort.Vec128 -> Types.Vec128
        | Jkind_types.Sort.Word -> Types.Word
        | Jkind_types.Sort.Void -> Misc.fatal_error "Not implemented")
      mixed_block_shapes
  in
  let reordering =
    Mixed_block_shape.of_mixed_block_elements
      (Lambda.transl_mixed_product_shape
         ~get_value_kind:(fun _ -> Lambda.generic_value)
         (* We don't care about the value kind of values, because it is dropped
            again immediately afterwards. We only care about the layout
            remapping. We only need the reordering to get the fields right
            below. *)
         mixed_block_shapes)
  in
  let fields =
    Array.init (Array.length fields) (fun i ->
        Array.get fields (Mixed_block_shape.new_index_to_old_index reordering i))
  in
  Array.to_list fields

let variant_constructor_reorder_fields fields kind =
  match kind with
  | Shape.Constructor_uniform_value -> fields
  | Shape.Constructor_mixed mixed_block_shapes ->
    reorder_record_fields_for_mixed_record ~mixed_block_shapes fields

(* CR sspies: This is a very hacky way of doing an unboxed variant with just a
   single constructor. DWARF variants expect to have a discriminator. So what we
   do is pick the first bit of the contents of the unboxed variant as the
   discriminator and then we simply ouput the same DWARF information for both
   cases. *)
let create_unboxed_variant_die ~reference ~parent_proto_die ?name ~constr_name
    ~arg_name ~arg_layout arg_die =
  let base_layout =
    match arg_layout with
    | Jkind_types.Sort.Const.Base base_layout -> base_layout
    | Jkind_types.Sort.Const.Product _ -> Misc.fatal_error "Not a base layout"
  in
  let width = base_layout_to_byte_size base_layout in
  let structure_ref = reference in
  let variant_part_ref = Proto_die.create_reference () in
  let variant_member_ref = Proto_die.create_reference () in
  let enum_die =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:[DAH.create_byte_size_exn ~byte_size:width]
      ()
  in
  let structure_die =
    Proto_die.create ~reference:structure_ref ~parent:(Some parent_proto_die)
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:width]
        @ attribute_list_with_optional_name name)
      ~tag:Dwarf_tag.Structure_type ()
  in
  let variant_part_die =
    Proto_die.create ~reference:variant_part_ref ~parent:(Some structure_die)
      ~attribute_values:
        [DAH.create_discr ~proto_die_reference:variant_member_ref]
      ~tag:Dwarf_tag.Variant_part ()
  in
  Proto_die.create_ignore ~reference:variant_member_ref
    ~parent:(Some variant_part_die) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference
          ~proto_die_reference:(Proto_die.reference enum_die);
        DAH.create_bit_size (Numbers.Int8.of_int_exn 1);
        DAH.create_data_bit_offset ~bit_offset:(Numbers.Int8.of_int_exn 0);
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0) ]
    ();
  for i = 0 to 1 do
    (* We create two identical discriminants. First, we create an enum case for
       both with the constructor name. *)
    Proto_die.create_ignore ~parent:(Some enum_die) ~tag:Dwarf_tag.Enumerator
      ~attribute_values:
        [ DAH.create_const_value ~value:(Int64.of_int i);
          DAH.create_name constr_name ]
      ();
    (* Then we create variant entries of the variant parts with the same
       discriminant. *)
    let constructor_variant =
      Proto_die.create ~parent:(Some variant_part_die) ~tag:Dwarf_tag.Variant
        ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int i)]
        ()
    in
    (* Lastly, we add the constructor argument as a member to the variant. *)
    Proto_die.create_ignore ~parent:(Some constructor_variant)
      ~tag:Dwarf_tag.Member
      ~attribute_values:
        ([ DAH.create_type_from_reference ~proto_die_reference:arg_die;
           DAH.create_byte_size_exn ~byte_size:width;
           DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0)
         ]
        @ attribute_list_with_optional_name arg_name)
      ()
  done

let create_complex_variant_die ~reference ~parent_proto_die ?name
    simple_constructors
    (complex_constructors :
      (Proto_die.reference * Jkind_types.Sort.base) Shape.complex_constructor
      list) =
  let complex_constructors_names =
    List.map
      (fun { Shape.name; kind = _; args = _ } -> name)
      complex_constructors
  in
  debug_info_add_enum ~reference
    (simple_constructors @ complex_constructors_names);
  let value_size = Arch.size_addr in
  let variant_part_immediate_or_pointer =
    let int_or_ptr_structure =
      Proto_die.create ~reference ~parent:(Some parent_proto_die)
        ~attribute_values:
          ([DAH.create_byte_size_exn ~byte_size:value_size]
          @ attribute_list_with_optional_name name)
        ~tag:Dwarf_tag.Structure_type ()
    in
    Proto_die.create ~parent:(Some int_or_ptr_structure) ~attribute_values:[]
      ~tag:Dwarf_tag.Variant_part ()
  in
  let enum_immediate_or_pointer =
    let enum_die =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Enumeration_type
        ~attribute_values:[DAH.create_byte_size_exn ~byte_size:value_size]
        ()
    in
    debug_info_add_enum
      ~reference:(Proto_die.reference enum_die)
      ["Immediate"; "Pointer"];
    List.iteri
      (fun i name ->
        Proto_die.create_ignore ~parent:(Some enum_die)
          ~tag:Dwarf_tag.Enumerator
          ~attribute_values:
            [ DAH.create_name name;
              DAH.create_const_value ~value:(Int64.of_int i) ]
          ())
      ["Pointer"; "Immediate"];
    enum_die
  in
  let _discriminant_immediate_or_pointer =
    let member_die =
      Proto_die.create ~parent:(Some variant_part_immediate_or_pointer)
        ~attribute_values:
          [ DAH.create_type ~proto_die:enum_immediate_or_pointer;
            DAH.create_bit_size (Numbers.Int8.of_int_exn 1);
            DAH.create_data_bit_offset ~bit_offset:(Numbers.Int8.of_int_exn 0);
            DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0);
            (* Making a member artificial will mark the struct as artificial,
               which will not print the enum name when the struct is a
               variant. *)
            DAH.create_artificial () ]
        ~tag:Dwarf_tag.Member ()
    in
    debug_info_add
      ~reference:(Proto_die.reference member_die)
      ("discriminant "
      ^ Asm_targets.Asm_label.encode
          (Proto_die.reference enum_immediate_or_pointer));
    Proto_die.add_or_replace_attribute_value variant_part_immediate_or_pointer
      (DAH.create_discr ~proto_die_reference:(Proto_die.reference member_die))
  in
  let _enum_simple_constructor =
    let enum_die =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Enumeration_type
        ~attribute_values:[DAH.create_byte_size_exn ~byte_size:value_size]
        ()
    in
    debug_info_add_enum
      ~reference:(Proto_die.reference enum_die)
      simple_constructors;
    List.iteri
      (fun i name ->
        Proto_die.create_ignore ~parent:(Some enum_die)
          ~tag:Dwarf_tag.Enumerator
          ~attribute_values:
            [ DAH.create_const_value ~value:(Int64.of_int i);
              DAH.create_name name ]
          ())
      simple_constructors;
    let variant_immediate_case =
      Proto_die.create ~parent:(Some variant_part_immediate_or_pointer)
        ~tag:Dwarf_tag.Variant
        ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int 1)]
        ()
    in
    let discr =
      Proto_die.create ~parent:(Some variant_immediate_case)
        ~tag:Dwarf_tag.Member
        ~attribute_values:
          [ DAH.create_type ~proto_die:enum_die;
            DAH.create_bit_size (Numbers.Int8.of_int_exn ((value_size * 8) - 1));
            DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0);
            DAH.create_data_bit_offset ~bit_offset:(Numbers.Int8.of_int_exn 1)
          ]
        ()
    in
    debug_info_add_alias
      ~from_ref:(Proto_die.reference discr)
      ~to_ref:(Proto_die.reference enum_die)
  in
  let _variant_complex_constructors =
    let ptr_case_structure =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Structure_type
        ~attribute_values:
          [ DAH.create_byte_size_exn ~byte_size:value_size;
            DAH.create_ocaml_offset_record_from_pointer
              ~value:(Int64.of_int (-value_size)) ]
        ()
    in
    let _attached_structure_to_pointer_variant =
      let ptr_case_pointer_to_structure =
        Proto_die.create ~parent:(Some parent_proto_die)
          ~tag:Dwarf_tag.Reference_type
          ~attribute_values:
            [ DAH.create_byte_size_exn ~byte_size:value_size;
              DAH.create_type ~proto_die:ptr_case_structure ]
          ()
      in
      debug_info_add_ptr
        ~reference:(Proto_die.reference ptr_case_pointer_to_structure)
        ~inner:(Proto_die.reference ptr_case_structure);
      let variant_pointer =
        Proto_die.create ~parent:(Some variant_part_immediate_or_pointer)
          ~tag:Dwarf_tag.Variant
          ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int 0)]
          ()
      in
      let ptr_mem =
        Proto_die.create ~parent:(Some variant_pointer) ~tag:Dwarf_tag.Member
          ~attribute_values:
            [ DAH.create_type ~proto_die:ptr_case_pointer_to_structure;
              DAH.create_data_member_location_offset
                ~byte_offset:(Int64.of_int 0) ]
          ()
      in
      debug_info_add_alias
        ~from_ref:(Proto_die.reference ptr_mem)
        ~to_ref:(Proto_die.reference ptr_case_pointer_to_structure)
    in
    let variant_part_pointer =
      Proto_die.create ~parent:(Some ptr_case_structure) ~attribute_values:[]
        ~tag:Dwarf_tag.Variant_part ()
    in
    let _enum_complex_constructor =
      let enum_die =
        Proto_die.create ~parent:(Some parent_proto_die)
          ~tag:Dwarf_tag.Enumeration_type
          ~attribute_values:[DAH.create_byte_size_exn ~byte_size:1]
          ()
      in
      List.iteri
        (fun i { Shape.name; kind = _; args = _ } ->
          Proto_die.create_ignore ~parent:(Some enum_die)
            ~tag:Dwarf_tag.Enumerator
            ~attribute_values:
              [ DAH.create_const_value ~value:(Int64.of_int i);
                DAH.create_name name ]
            ())
        complex_constructors;
      let discriminant =
        Proto_die.create ~parent:(Some variant_part_pointer)
          ~attribute_values:
            [ DAH.create_type ~proto_die:enum_die;
              DAH.create_data_member_location_offset
                ~byte_offset:(Int64.of_int 0) ]
          ~tag:Dwarf_tag.Member ()
      in
      Proto_die.add_or_replace_attribute_value variant_part_pointer
        (DAH.create_discr
           ~proto_die_reference:(Proto_die.reference discriminant))
    in
    List.iteri
      (fun i { Shape.name; kind = constructor_kind; args } ->
        let subvariant =
          Proto_die.create ~parent:(Some variant_part_pointer)
            ~tag:Dwarf_tag.Variant
            ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int i)]
            ()
        in
        debug_info_add ~reference:(Proto_die.reference subvariant) name;
        let args = variant_constructor_reorder_fields args constructor_kind in
        let offset = ref 0 in
        List.iter
          (fun { Shape.field_name; field_value = field_type, ly } ->
            let member_size = base_layout_to_byte_size_in_mixed_block ly in
            let member_die =
              Proto_die.create ~parent:(Some subvariant) ~tag:Dwarf_tag.Member
                ~attribute_values:
                  [ DAH.create_data_member_location_offset
                      ~byte_offset:(Int64.of_int (!offset + value_size));
                    (* members start after the block header, hence we add
                       [value_size] *)
                    DAH.create_byte_size_exn ~byte_size:member_size;
                    DAH.create_type_from_reference
                      ~proto_die_reference:field_type ]
                ()
            in
            debug_info_add_alias
              ~from_ref:(Proto_die.reference member_die)
              ~to_ref:field_type;
            offset := !offset + member_size;
            match field_name with
            | Some name ->
              Proto_die.add_or_replace_attribute_value member_die
                (DAH.create_name name)
            | None -> ())
          args)
      complex_constructors
  in
  ()

type immediate_or_pointer =
  | Immediate
  | Pointer

let tag_bit = function Immediate -> 1 | Pointer -> 0

let create_immediate_or_block ~reference ~parent_proto_die ?name ~immediate_type
    ~pointer_type () =
  let value_size = Arch.size_addr in
  let int_or_ptr_structure =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:value_size]
        @ attribute_list_with_optional_name name)
      ~tag:Dwarf_tag.Structure_type ()
  in
  (* We create the reference early to use it already for the variant, before we
     allocate the child die for the discriminant below. *)
  let discriminant_reference = Proto_die.create_reference () in
  let variant_part_immediate_or_pointer =
    Proto_die.create ~parent:(Some int_or_ptr_structure)
      ~attribute_values:
        [DAH.create_discr ~proto_die_reference:discriminant_reference]
      ~tag:Dwarf_tag.Variant_part ()
  in
  let enum_die =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:[DAH.create_byte_size_exn ~byte_size:value_size]
      ()
  in
  debug_info_add_enum
    ~reference:(Proto_die.reference enum_die)
    ["Immediate"; "Pointer"];
  List.iter
    (fun elem ->
      Proto_die.create_ignore ~parent:(Some enum_die) ~tag:Dwarf_tag.Enumerator
        ~attribute_values:
          [DAH.create_const_value ~value:(Int64.of_int (tag_bit elem))]
        ())
    [Immediate; Pointer];
  let _discriminant_die =
    Proto_die.create ~reference:discriminant_reference
      ~parent:(Some variant_part_immediate_or_pointer)
      ~attribute_values:
        [ DAH.create_type ~proto_die:enum_die;
          DAH.create_bit_size (Numbers.Int8.of_int_exn 1);
          DAH.create_data_bit_offset ~bit_offset:(Numbers.Int8.of_int_exn 0);
          DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0);
          (* Making a member artificial will mark the struct as artificial,
             which will not print the enum name when the struct is a variant. *)
          DAH.create_artificial () ]
      ~tag:Dwarf_tag.Member ()
  in
  let variant_immediate_case =
    Proto_die.create ~parent:(Some variant_part_immediate_or_pointer)
      ~tag:Dwarf_tag.Variant
      ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int 1)]
      ()
  in
  (* Unlike in the code above, we include the tag bit in this representation. *)
  Proto_die.create_ignore ~parent:(Some variant_immediate_case)
    ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type ~proto_die:immediate_type;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0) ]
    ();
  let variant_pointer =
    Proto_die.create ~parent:(Some variant_part_immediate_or_pointer)
      ~tag:Dwarf_tag.Variant
      ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int 0)]
      ()
  in
  Proto_die.create_ignore ~parent:(Some variant_pointer) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type ~proto_die:pointer_type;
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0) ]
    ()

(*= The runtime representation of polymorphic variants is different from that
    of regular blocks. At runtime, the variant type

      [type t = [`Foo | `Bar of int | `Baz of int * string]]

    is represented as follows:

    For the constant constructors (i.e., here [`Foo]), the representation is
    the hash value of the constructor name tagged as an immediate. Specifically,
    it is [(Btype.hash_variant name) * 2 + 1], where [name] does not include the
    backtick.

    For the variable constructors (i.e., here [`Bar] and [`Baz]), the
    representation is a pointer to a block with the following layout:

      ---------------------------------------------------------
      | tagged constructor hash | arg 1 | arg 2 | ... | arg n |
      ---------------------------------------------------------

    In other words, the first field (offset 0) is the hash of the constructor
    name (tagged as in the constant constructor case) and the subsequent fields
    store the arguments of the constructor.
*)

let create_poly_variant_die ~reference ~parent_proto_die ?name constructors =
  let enum_constructor_for_poly_variant ~parent name =
    let hash = Btype.hash_variant name in
    let tagged_constructor_hash =
      Int64.add (Int64.mul (Int64.of_int hash) 2L) 1L
    in
    Proto_die.create_ignore ~parent:(Some parent) ~tag:Dwarf_tag.Enumerator
      ~attribute_values:
        [ DAH.create_const_value ~value:tagged_constructor_hash;
          DAH.create_name ("`" ^ name) ]
      ();
    tagged_constructor_hash
  in
  let simple_constructors, complex_constructors =
    List.partition_map
      (fun ({ pv_constr_name; pv_constr_args } :
             _ Shape.poly_variant_constructor) ->
        match pv_constr_args with
        | [] -> Left pv_constr_name
        | _ :: _ -> Right (pv_constr_name, pv_constr_args))
      constructors
  in
  (* For the simple constructors, it is enough to create an enum with the right
     numbers for the constructor labels. *)
  let simple_constructor_enum_die =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:[DAH.create_byte_size_exn ~byte_size:Arch.size_addr]
      ()
  in
  List.iter
    (fun constr_name ->
      ignore
        (enum_constructor_for_poly_variant ~parent:simple_constructor_enum_die
           constr_name))
    simple_constructors;
  (* For the complex constructors, we create a reference to a structure. The
     structure uses the first field in the block to discriminate between the
     different constructor cases. *)
  let complex_constructor_enum_die =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:[DAH.create_byte_size_exn ~byte_size:Arch.size_addr]
      ()
  in
  let complex_constructors_struct =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:[DAH.create_byte_size_exn ~byte_size:Arch.size_addr]
      (* CR sspies: This is not really the width of the structure type, but the
         code seems to work fine. The true width of the block depends on how
         many arguments the constructor has. *)
      ()
  in
  let constructor_discriminant_ref = Proto_die.create_reference () in
  let variant_part_constructor =
    Proto_die.create ~parent:(Some complex_constructors_struct)
      ~attribute_values:
        [DAH.create_discr ~proto_die_reference:constructor_discriminant_ref]
      ~tag:Dwarf_tag.Variant_part ()
  in
  Proto_die.create_ignore ~reference:constructor_discriminant_ref
    ~parent:(Some complex_constructors_struct) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type ~proto_die:complex_constructor_enum_die;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0) ]
    ();
  List.iter
    (fun (name, args) ->
      let tag_value =
        enum_constructor_for_poly_variant ~parent:complex_constructor_enum_die
          name
      in
      let constructor_variant =
        Proto_die.create ~parent:(Some variant_part_constructor)
          ~tag:Dwarf_tag.Variant
          ~attribute_values:[DAH.create_discr_value ~value:tag_value]
          ()
      in
      List.iteri
        (fun i arg ->
          Proto_die.create_ignore ~parent:(Some constructor_variant)
            ~tag:Dwarf_tag.Member
            ~attribute_values:
              [ DAH.create_type_from_reference ~proto_die_reference:arg;
                DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
                (* We add an offset of [Arch.size_addr], because the first field
                   in the block stores the hash of the constructor name. *)
                DAH.create_data_member_location_offset
                  ~byte_offset:(Int64.of_int ((i + 1) * Arch.size_addr)) ]
            ())
        args)
    complex_constructors;
  let ptr_case_pointer_to_structure =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Reference_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
          DAH.create_type ~proto_die:complex_constructors_struct ]
      ()
  in
  create_immediate_or_block ~reference ?name ~parent_proto_die
    ~immediate_type:simple_constructor_enum_die
    ~pointer_type:ptr_case_pointer_to_structure ()

let create_exception_die ~reference ~fallback_value_die ~parent_proto_die ?name
    () =
  let exn_structure =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:Arch.size_addr]
        @ attribute_list_with_optional_name name)
      ()
  in
  let constructor_ref = Proto_die.create_reference () in
  Proto_die.create_ignore ~parent:(Some exn_structure) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference ~proto_die_reference:constructor_ref;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:0L;
        DAH.create_name "exn" ]
    ();
  Proto_die.create_ignore ~parent:(Some exn_structure) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference ~proto_die_reference:fallback_value_die;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:0L;
        DAH.create_name "raw" ]
    ();
  (* CR sspies: Instead of printing the raw exception, it would be nice if we
     could encode this as a variant. Unfortunately, the DWARF LLDB support is
     not expressive enough to support a variant, whose discriminant is not
     directly a member of the surrounding struct. Moreover, for the number of
     arguments, we would need support for some form of arrays without a pointer
     indirection. *)
  let structure_type =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
          DAH.create_ocaml_offset_record_from_pointer
            ~value:(Int64.of_int (-Arch.size_addr)) ]
      ()
  in
  Proto_die.create_ignore ~reference:constructor_ref
    ~parent:(Some parent_proto_die) ~tag:Dwarf_tag.Reference_type
    ~attribute_values:
      [ DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_type ~proto_die:structure_type ]
    ();
  let tag_type =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:1;
          DAH.create_encoding ~encoding:Encoding_attribute.signed ]
      ~tag:Dwarf_tag.Base_type ()
  in
  let exception_tag_discriminant_ref = Proto_die.create_reference () in
  Proto_die.create_ignore ~parent:(Some structure_type)
    ~reference:exception_tag_discriminant_ref ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type ~proto_die:tag_type;
        DAH.create_byte_size_exn ~byte_size:1;
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0);
        DAH.create_artificial () ]
    ();
  let variant_part_exception =
    Proto_die.create ~parent:(Some structure_type)
      ~attribute_values:
        [DAH.create_discr ~proto_die_reference:exception_tag_discriminant_ref]
      ~tag:Dwarf_tag.Variant_part ()
  in
  let exception_without_arguments_variant =
    Proto_die.create ~parent:(Some variant_part_exception)
      ~tag:Dwarf_tag.Variant
      ~attribute_values:[DAH.create_discr_value ~value:248L]
      ()
  in
  Proto_die.create_ignore ~parent:(Some exception_without_arguments_variant)
    ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference ~proto_die_reference:fallback_value_die;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:8L;
        DAH.create_name "name" ]
    ();
  Proto_die.create_ignore ~parent:(Some exception_without_arguments_variant)
    ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference ~proto_die_reference:fallback_value_die;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:16L;
        DAH.create_name "id" ]
    ();
  let exception_with_arguments_variant =
    Proto_die.create ~parent:(Some variant_part_exception)
      ~tag:Dwarf_tag.Variant
      ~attribute_values:[DAH.create_discr_value ~value:0L]
      ()
  in
  let inner_exn_block =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:(3 * Arch.size_addr);
          DAH.create_ocaml_offset_record_from_pointer ~value:(-8L) ]
      ()
  in
  Proto_die.create_ignore ~parent:(Some inner_exn_block) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference ~proto_die_reference:fallback_value_die;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:8L;
        DAH.create_name "name" ]
    ();
  Proto_die.create_ignore ~parent:(Some inner_exn_block) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference ~proto_die_reference:fallback_value_die;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:16L;
        DAH.create_name "id" ]
    ();
  let outer_reference =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Reference_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
          DAH.create_type_from_reference
            ~proto_die_reference:(Proto_die.reference inner_exn_block) ]
      ()
  in
  Proto_die.create_ignore ~parent:(Some exception_with_arguments_variant)
    ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference
          ~proto_die_reference:(Proto_die.reference outer_reference);
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:8L ]
    ()

let create_tuple_die ~reference ~parent_proto_die ?name fields =
  let structure_type =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:(List.length fields * 8)]
        @ attribute_list_with_optional_name name)
      ()
  in
  List.iteri
    (fun i field_die ->
      let member_attributes =
        [ DAH.create_type_from_reference ~proto_die_reference:field_die;
          DAH.create_data_member_location_offset
            ~byte_offset:(Int64.of_int (8 * i)) ]
      in
      let member =
        Proto_die.create ~parent:(Some structure_type) ~tag:Dwarf_tag.Member
          ~attribute_values:member_attributes ()
      in
      debug_info_add_alias
        ~from_ref:(Proto_die.reference member)
        ~to_ref:field_die)
    fields;
  wrap_die_under_a_pointer ~proto_die:structure_type ~reference
    ~parent_proto_die

type vec128_splits =
  | Int8x16
  | Int16x8
  | Int32x4
  | Int64x2
  | Float32x4
  | Float64x2

let unboxed_base_type_to_vec128_split (x : Shape.Predef.unboxed) =
  match x with
  | Shape.Predef.Unboxed_int8x16 -> Some Int8x16
  | Unboxed_int16x8 -> Some Int16x8
  | Unboxed_int32x4 -> Some Int32x4
  | Unboxed_int64x2 -> Some Int64x2
  | Unboxed_float32x4 -> Some Float32x4
  | Unboxed_float64x2 -> Some Float64x2
  | Unboxed_float | Unboxed_float32 | Unboxed_nativeint | Unboxed_int64
  | Unboxed_int32 ->
    None

let predef_type_to_vec128_split (m : Shape.Predef.t) =
  match m with
  | Int8x16 -> Some Int8x16
  | Int16x8 -> Some Int16x8
  | Int32x4 -> Some Int32x4
  | Int64x2 -> Some Int64x2
  | Float32x4 -> Some Float32x4
  | Float64x2 -> Some Float64x2
  | Array | Char | Unboxed _ | Extension_constructor | Float | Float32 | String
  | Lazy_t | Bytes | Floatarray | Exception | Int | Int32 | Int64 | Nativeint ->
    None

let create_vec128_base_layout_die ~reference ~parent_proto_die ?name ~split () =
  match split with
  | None ->
    Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Base_type
      ~attribute_values:
        ([ DAH.create_encoding ~encoding:Encoding_attribute.unsigned;
           DAH.create_byte_size_exn ~byte_size:16 ]
        @ attribute_list_with_optional_name name)
      ()
  | Some vec_split ->
    let structure =
      Proto_die.create ~reference ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Structure_type
        ~attribute_values:
          ([DAH.create_byte_size_exn ~byte_size:16]
          @ attribute_list_with_optional_name name)
        ()
    in
    let encoding, count, size =
      match vec_split with
      | Int8x16 -> Encoding_attribute.signed, 16, 1
      | Int16x8 -> Encoding_attribute.signed, 8, 2
      | Int32x4 -> Encoding_attribute.signed, 4, 4
      | Int64x2 -> Encoding_attribute.signed, 2, 8
      | Float32x4 -> Encoding_attribute.float, 4, 4
      | Float64x2 -> Encoding_attribute.float, 2, 8
    in
    let base_type =
      Proto_die.create ~parent:(Some parent_proto_die) ~tag:Dwarf_tag.Base_type
        ~attribute_values:
          [ DAH.create_encoding ~encoding;
            DAH.create_byte_size_exn ~byte_size:size ]
        ()
    in
    for i = 0 to count - 1 do
      Proto_die.create_ignore ~parent:(Some structure) ~tag:Dwarf_tag.Member
        ~attribute_values:
          [ DAH.create_type_from_reference
              ~proto_die_reference:(Proto_die.reference base_type);
            DAH.create_data_member_location_offset
              ~byte_offset:(Int64.of_int (i * size)) ]
        ()
    done

let create_base_layout_type ?(vec128_split = None) ~reference
    (sort : Jkind_types.Sort.base) ?name ~parent_proto_die ~fallback_value_die
    () =
  let byte_size = base_layout_to_byte_size sort in
  match sort with
  | Value ->
    create_typedef_die ~reference ~parent_proto_die ?name fallback_value_die
  | Void ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ?name ~byte_size
      Encoding_attribute.signed
  | Float64 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ?name ~byte_size
      Encoding_attribute.float
  | Float32 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ?name ~byte_size
      Encoding_attribute.float
  | Word ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ?name ~byte_size
      Encoding_attribute.signed
  | Bits32 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ?name ~byte_size
      Encoding_attribute.signed
  | Bits64 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ?name ~byte_size
      Encoding_attribute.signed
  | Vec128 ->
    create_vec128_base_layout_die ~reference ~parent_proto_die ?name
      ~split:vec128_split ()

module Cache = Shape.Uid.Tbl

type base_layout = Jkind_types.Sort.base

module Layout = Jkind_types.Sort.Const

let cache = Cache.create 16

let rec type_shape_layout_to_die ?type_name (type_shape : Layout.t Shape.ts)
    ~parent_proto_die ~fallback_value_die =
  (* match Cache.find_opt cache type_shape with | Some reference -> reference |
     None -> *)
  let reference = Proto_die.create_reference () in
  let layout_name =
    Format.asprintf "%a" Jkind_types.Sort.Const.format
      (Shape.shape_layout type_shape)
  in
  let name =
    Option.map (fun type_name -> type_name ^ " @ " ^ layout_name) type_name
  in
  (match type_shape with
  | Ts_other type_layout | Ts_var (_, type_layout) -> (
    match type_layout with
    | Base b ->
      create_base_layout_type ~reference b ?name ~parent_proto_die
        ~fallback_value_die ()
    | Product _ ->
      Misc.fatal_errorf
        "only base layouts supported, but found unboxed product layout %s"
        layout_name)
  | Ts_unboxed_tuple _ ->
    Misc.fatal_errorf "unboxed tuples cannot have base layout %s" layout_name
  | Ts_tuple fields ->
    type_shape_layout_tuple_die ~reference ~parent_proto_die ~fallback_value_die
      ?name fields
  | Ts_predef (predef, args) ->
    type_shape_layout_predef_die ~reference ?name ~parent_proto_die
      ~fallback_value_die predef args
  | Ts_constr ((shape, type_layout), shapes) -> (
    match type_layout with
    | Base b ->
      type_shape_layout_constructor_die ~reference ?name ~parent_proto_die
        ~fallback_value_die shape b shapes
    | Product _ ->
      Misc.fatal_errorf
        "only base layouts supported, but found product layout %s" layout_name)
  | Ts_variant (fields, _) ->
    poly_variant_die ~reference ?name ~parent_proto_die ~fallback_value_die
      fields
  | Ts_arrow (arg, ret) ->
    type_shape_layout_arrow_die ~reference ?name ~parent_proto_die
      ~fallback_value_die arg ret);
  reference

and type_shape_layout_tuple_die ?name ~reference ~parent_proto_die
    ~fallback_value_die fields =
  let fields =
    List.map
      (fun shape ->
        type_shape_layout_to_die ~parent_proto_die ~fallback_value_die shape)
      fields
  in
  create_tuple_die ~reference ~parent_proto_die ?name fields

and type_shape_layout_predef_die ?name ~reference ~parent_proto_die
    ~fallback_value_die (predef : Shape.Predef.t) args =
  match predef, args with
  | Array, [element_type_shape] ->
    let element_type_shape =
      Shape.shape_with_layout ~layout:(Base Value) element_type_shape
    in
    (* CR sspies: Check whether the elements of an array are always values and,
       if not, where that information is maintained. *)
    let child_die =
      type_shape_layout_to_die ~parent_proto_die ~fallback_value_die
        element_type_shape
    in
    create_array_die ~reference ~parent_proto_die ~child_die ?name ()
  | Array, _ ->
    Misc.fatal_error "Array applied to zero or more than one type."
    (* CR sspies: What should we do in this case. The old code supported it,
       simply yielding the [fallback_value_die], but that seems strange. *)
  | Char, _ -> create_char_die ~reference ~parent_proto_die ?name ()
  | Unboxed b, _ ->
    let type_layout = Shape.Predef.unboxed_type_to_base_layout b in
    create_base_layout_type
      ~vec128_split:(unboxed_base_type_to_vec128_split b)
      ~reference type_layout ?name ~parent_proto_die ~fallback_value_die ()
    (* CR sspies: Take [b] into account here, perhaps as an optional argument,
       to support int8x16 vs float64x2. *)
  | ((Int8x16 | Int16x8 | Int32x4 | Int64x2 | Float32x4 | Float64x2) as b), _ ->
    (* We represent these vectors as pointers of the form [struct {...} *],
       because their runtime representation are abstract blocks. *)
    let base_ref = Proto_die.create_reference () in
    let vec128_split = predef_type_to_vec128_split b in
    create_vec128_base_layout_die ~split:vec128_split ~reference:base_ref
      ~parent_proto_die ();
    Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Reference_type
      ~attribute_values:
        ([ DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
           DAH.create_type_from_reference ~proto_die_reference:base_ref ]
        @ attribute_list_with_optional_name name)
      ()
  | Exception, _ ->
    create_exception_die ~reference ~fallback_value_die ~parent_proto_die ?name
      ()
  | ( ( Bytes | Extension_constructor | Float | Float32 | Floatarray | Int
      | Int32 | Int64 | Lazy_t | Nativeint | String ),
      _ ) ->
    create_base_layout_type ~reference Value ?name ~parent_proto_die
      ~fallback_value_die ()

and type_shape_layout_constructor_die ~reference ?name ~parent_proto_die
    ~fallback_value_die (shape : Shape.t) (type_layout : base_layout) shapes =
  let decl =
    match shape.desc with
    | Shape.Type_decl tds -> `Declaration tds
    | Shape.Leaf -> (
      match Option.bind shape.uid (Cache.find_opt cache) with
      | Some cached_ref -> `Reference cached_ref
      | None -> `Missing)
    | Shape.Var _ | Shape.Abs _ | Shape.App _ | Shape.Struct _ | Shape.Alias _
    | Shape.Proj _ | Shape.Comp_unit _ | Shape.Error _ ->
      `Missing
  in
  match
    (* CR sspies: Somewhat subtly, this case currently also handles [unit],
       [bool], [option], and [list], because they are not treated as predefined
       types and do have declarations. *)
    decl
  with
  | `Missing ->
    create_base_layout_type ~reference type_layout ?name ~parent_proto_die
      ~fallback_value_die ()
  | `Reference cached_ref ->
    create_typedef_die ~reference ~parent_proto_die ?name cached_ref
  | `Declaration type_decl_shape -> (
    let type_expr_ref = reference in
    let reference = Proto_die.create_reference () in
    create_typedef_die ~reference:type_expr_ref ~parent_proto_die ?name
      reference;
    (* We add the combination of shape and layout early in case of recursive
       types, which can then look up their reference, before it is fully
       defined. That way [type myintlist = MyNil | MyCons of int * myintlist]
       will work correctly (as opposed to diverging). *)
    Option.iter (fun uid -> Cache.add cache uid reference) shape.uid;
    let type_decl_shape =
      Type_shape.Type_decl_shape.replace_tvar type_decl_shape shapes
    in
    match type_decl_shape.definition with
    | Tds_other ->
      create_base_layout_type ~reference type_layout ?name ~parent_proto_die
        ~fallback_value_die ()
    | Tds_alias alias_shape ->
      let alias_shape =
        Shape.shape_with_layout ~layout:(Base type_layout) alias_shape
      in
      let alias_die =
        type_shape_layout_to_die alias_shape ~parent_proto_die
          ~fallback_value_die
      in
      create_typedef_die ~reference ~parent_proto_die ?name alias_die
    | Tds_record { fields; kind = Record_boxed | Record_floats } ->
      let fields =
        List.map
          (fun (name, type_shape, type_layout) ->
            let type_shape' =
              Shape.shape_with_layout ~layout:type_layout type_shape
            in
            ( name,
              Arch.size_addr,
              (* field size for values*)
              type_shape_layout_to_die ~parent_proto_die ~fallback_value_die
                type_shape' ))
          fields
      in
      create_record_die ~reference ~parent_proto_die ?name fields
    | Tds_record { fields = _; kind = Record_unboxed_product } ->
      Misc.fatal_error
        "Unboxed records should not reach this stage. They are deconstructed \
         by unarization in earlier stages of the compiler."
    | Tds_record
        { fields = [(field_name, sh, Base base_layout)]; kind = Record_unboxed }
      ->
      let field_shape = Shape.shape_with_layout ~layout:(Base base_layout) sh in
      let field_die =
        type_shape_layout_to_die ~parent_proto_die ~fallback_value_die
          field_shape
      in
      let field_size = base_layout_to_byte_size base_layout in
      create_unboxed_record_die ~reference ~parent_proto_die ?name ~field_name
        ~field_size field_die
      (* The two cases below are filtered out by the flattening of shapes in
         [flatten_type_shape]. *)
    | Tds_record { fields = [] | _ :: _ :: _; kind = Record_unboxed } ->
      assert false
    | Tds_record { fields = [(_, _, Product _)]; kind = Record_unboxed } ->
      assert false
    | Tds_record { fields; kind = Record_mixed mixed_block_shapes } ->
      let fields =
        List.map
          (fun (name, type_shape, type_layout) ->
            let type_shape' =
              Shape.shape_with_layout ~layout:type_layout type_shape
            in
            match (type_layout : Layout.t) with
            | Base base_layout ->
              ( name,
                base_layout_to_byte_size_in_mixed_block base_layout,
                type_shape_layout_to_die ~parent_proto_die ~fallback_value_die
                  type_shape' )
            | Product _ ->
              Misc.fatal_error "mixed products must contain base layouts")
          fields
      in
      let fields =
        reorder_record_fields_for_mixed_record ~mixed_block_shapes fields
      in
      create_record_die ~reference ~parent_proto_die ?name fields
    | Tds_variant { simple_constructors; complex_constructors } -> (
      match complex_constructors with
      | [] ->
        create_simple_variant_die ~reference ~parent_proto_die ?name
          simple_constructors
      | _ :: _ ->
        let complex_constructors =
          List.map
            (Shape.complex_constructor_map (fun (sh, layout) ->
                 match layout with
                 | Jkind_types.Sort.Const.Base ly ->
                   let sh = Shape.shape_with_layout ~layout sh in
                   ( type_shape_layout_to_die ~parent_proto_die
                       ~fallback_value_die sh,
                     ly )
                 | Jkind_types.Sort.Const.Product _ ->
                   Misc.fatal_error
                     "unboxed product in complex constructor is not allowed"))
            complex_constructors
        in
        create_complex_variant_die ~reference ~parent_proto_die ?name
          simple_constructors complex_constructors)
    | Tds_variant_unboxed
        { name = constr_name; arg_name; arg_shape; arg_layout } ->
      let arg_shape = Shape.shape_with_layout ~layout:arg_layout arg_shape in
      let arg_die =
        type_shape_layout_to_die ~parent_proto_die ~fallback_value_die arg_shape
      in
      create_unboxed_variant_die ~reference ~parent_proto_die ?name ~constr_name
        ~arg_name ~arg_layout arg_die)

and type_shape_layout_arrow_die ~reference ?name ~parent_proto_die
    ~fallback_value_die _arg _ret =
  (* There is no need to inspect the argument and return value. *)
  create_typedef_die ~reference ~parent_proto_die ?name fallback_value_die

and poly_variant_die ~reference ~parent_proto_die ~fallback_value_die ?name
    constructors =
  let constructors_with_references =
    List.map
      (fun ({ pv_constr_name; pv_constr_args } :
             _ Shape.poly_variant_constructor) :
           _ Shape.poly_variant_constructor ->
        { pv_constr_name;
          pv_constr_args =
            List.map
              (fun arg ->
                type_shape_layout_to_die ~parent_proto_die ~fallback_value_die
                  arg)
              pv_constr_args
        })
      constructors
  in
  create_poly_variant_die ~reference ~parent_proto_die ?name
    constructors_with_references

let rec flatten_to_base_sorts (sort : Jkind_types.Sort.Const.t) :
    Jkind_types.Sort.base list =
  match sort with
  | Base b -> [b]
  | Product sorts -> List.concat_map flatten_to_base_sorts sorts

(* This function performs the counterpart of unarization in the rest of the
   compiler. We flatten the type into a sequence that corresponds to the fields
   after unarization. In some cases, the type cannot be broken up (e.g., for
   type variables). In these cases, we produce the corresponding number of
   entries of the form [`Unknown base_layout] for the fields. Otherwise, when
   the type is known, we produce [`Known type_shape] for the fields. *)
let rec flatten_type_shape (type_shape : Jkind_types.Sort.Const.t Shape.ts) =
  let unknown_base_layouts layout =
    let base_sorts = flatten_to_base_sorts layout in
    List.map (fun base_sort -> `Unknown base_sort) base_sorts
  in
  match type_shape with
  | Ts_var (_, Base _) -> [`Known type_shape]
  | Ts_var (_, (Product _ as type_layout)) -> unknown_base_layouts type_layout
  | Ts_tuple _ ->
    [`Known type_shape] (* tuples are only a single base layout wide *)
  | Ts_unboxed_tuple shapes -> List.concat_map flatten_type_shape shapes
  | Ts_predef _ -> [`Known type_shape]
  | Ts_arrow _ -> [`Known type_shape]
  | Ts_variant _ -> [`Known type_shape]
  | Ts_other layout ->
    let base_layouts = flatten_to_base_sorts layout in
    List.map (fun layout -> `Unknown layout) base_layouts
  | Ts_constr ((shape, layout), shapes) -> (
    let decl =
      match shape.desc with
      | Shape.Type_decl tds -> Some tds
      | Shape.Var _ | Shape.Abs _ | Shape.App _ | Shape.Leaf | Shape.Struct _
      | Shape.Alias _ | Shape.Proj _ | Shape.Comp_unit _ | Shape.Error _ ->
        None
    in
    match[@warning "-4"] decl with
    | None -> unknown_base_layouts layout
    | Some { definition = Tds_other; _ } -> unknown_base_layouts layout
    | Some type_decl_shape -> (
      let type_decl_shape =
        Type_shape.Type_decl_shape.replace_tvar type_decl_shape shapes
      in
      match type_decl_shape.definition with
      | Tds_other ->
        unknown_base_layouts layout (* Cannot break up unknown type. *)
      | Tds_alias alias_shape ->
        let alias_shape = Shape.shape_with_layout ~layout alias_shape in
        flatten_type_shape alias_shape
        (* At first glance, this recursion could potentially diverge, for direct
           cycles between type aliases and the defintion of the type. However,
           it seems the compiler disallows direct cycles such as [type t = t]
           and the like. If this ever causes trouble or the behvior of the
           compiler changes with respect to recursive types, we can add a bound
           on the maximal recursion depth. *)
      | Tds_record
          { fields = _; kind = Record_boxed | Record_mixed _ | Record_floats }
        -> (
        match layout with
        | Base Value -> [`Known type_shape]
        | _ -> Misc.fatal_error "record must have value layout")
      | Tds_record { fields = [(_, sh, ly)]; kind = Record_unboxed }
        when Layout.equal ly layout -> (
        match layout with
        | Product _ -> flatten_type_shape (Shape.shape_with_layout ~layout sh)
        (* for unboxed products of the form [{ field: ty } [@@unboxed]] where
           [ty] is of product sort, we simply look through the unboxed product.
           Otherwise, we will create an additional DWARF entry for it. *)
        | Base _ -> [`Known type_shape])
      | Tds_record { fields = [_]; kind = Record_unboxed } ->
        Misc.fatal_error "unboxed record at different layout than its field"
      | Tds_record
          { fields = ([] | _ :: _ :: _) as fields; kind = Record_unboxed } ->
        Misc.fatal_errorf "unboxed record must have exactly one field, found %a"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
             Format.pp_print_string)
          (List.map (fun (name, _, _) -> name) fields)
      | Tds_record { fields; kind = Record_unboxed_product } -> (
        match layout with
        | Product prod_shapes when List.length prod_shapes = List.length fields
          ->
          let shapes =
            List.map
              (fun (_, sh, ly) -> Shape.shape_with_layout ~layout:ly sh)
              fields
          in
          List.concat_map flatten_type_shape shapes
        | Product _ -> Misc.fatal_error "unboxed record field mismatch"
        | Base _ -> Misc.fatal_error "unboxed record must have product layout")
      | Tds_variant _ -> (
        match layout with
        | Base Value -> [`Known type_shape]
        | _ -> Misc.fatal_error "variant must have value layout")
      | Tds_variant_unboxed
          { name = _; arg_name = _; arg_layout; arg_shape = _ } ->
        if Layout.equal arg_layout layout
        then [`Known type_shape]
        else
          Misc.fatal_error
            "unboxed variant must have same layout as its contents"))

let rec used_uids_shape (sh : Shape.t) =
  match sh.desc with
  | Comp_unit _ -> Shape.Uid.Set.empty
  | Var _ -> Shape.Uid.Set.empty
  | Leaf -> (
    (* After reduction, the only possible occurrences of uids are the leafs. *)
    match sh.uid with
    | None -> Shape.Uid.Set.empty
    | Some uid -> Shape.Uid.Set.singleton uid)
  | Type_decl tds -> used_uids_tds tds
  | Abs (_, e) -> used_uids_shape e
  | App (f, s) -> Shape.Uid.Set.union (used_uids_shape f) (used_uids_shape s)
  | Struct items ->
    Shape.Item.Map.fold
      (fun _ sh acc -> Shape.Uid.Set.union (used_uids_shape sh) acc)
      items Shape.Uid.Set.empty
  | Alias sh -> used_uids_shape sh
  | Proj (str, _) -> used_uids_shape str
  | Error _ -> Shape.Uid.Set.empty

and used_uids_tds (tds : Shape.tds) =
  match tds.definition with
  | Tds_other -> Shape.Uid.Set.empty
  | Tds_alias sh -> used_uids_ts sh
  | Tds_record { fields; _ } ->
    List.fold_left
      (fun acc (_, sh, _) -> Shape.Uid.Set.union (used_uids_ts sh) acc)
      Shape.Uid.Set.empty fields
  | Tds_variant { complex_constructors; _ } ->
    List.fold_left
      (fun acc { Shape.args; _ } ->
        Shape.Uid.Set.union
          (List.fold_left
             (fun acc { Shape.field_value = ts, _; _ } ->
               Shape.Uid.Set.union (used_uids_ts ts) acc)
             Shape.Uid.Set.empty args)
          acc)
      Shape.Uid.Set.empty complex_constructors
  | Tds_variant_unboxed { arg_shape; _ } -> used_uids_ts arg_shape

and used_uids_ts (ts : 'a Shape.ts) =
  let used_uids_type_shapes tss =
    List.fold_left
      (fun acc sh -> Shape.Uid.Set.union (used_uids_ts sh) acc)
      Shape.Uid.Set.empty tss
  in
  match ts with
  | Ts_constr ((sh, _), args) ->
    Shape.Uid.Set.union (used_uids_shape sh) (used_uids_type_shapes args)
  | Ts_tuple ts -> used_uids_type_shapes ts
  | Ts_unboxed_tuple ts -> used_uids_type_shapes ts
  | Ts_var _ -> Shape.Uid.Set.empty
  | Ts_predef _ -> Shape.Uid.Set.empty
  | Ts_arrow (arg, ret) -> used_uids_type_shapes [arg; ret]
  | Ts_variant (fields, _) ->
    List.fold_left
      (fun acc { Shape.pv_constr_args; _ } ->
        Shape.Uid.Set.union (used_uids_type_shapes pv_constr_args) acc)
      Shape.Uid.Set.empty fields
  | Ts_other _ -> Shape.Uid.Set.empty

let rec compress_shape (used_uids : Shape.Uid.Set.t) (sh : Shape.t) =
  let uid_used = function
    | None -> false
    | Some uid -> Shape.Uid.Set.mem uid used_uids
  in
  let compressed =
    match[@warning "-4"] sh.desc with
    | Comp_unit _ | Var _ | Proj _ | Leaf | Abs _ | App _ | Struct _ | Error _
      ->
      sh
    | Alias sh -> compress_shape used_uids sh
    | Type_decl { definition = Tds_alias (Ts_constr ((inner_sh, _), [])); _ }
      when not (uid_used sh.uid) ->
      compress_shape used_uids inner_sh
    | Type_decl tds -> Shape.type_decl sh.uid (compress_tds used_uids tds)
  in
  compressed

and compress_tds (used_uids : Shape.Uid.Set.t) (tds : Shape.tds) =
  let desc =
    match tds.definition with
    | Tds_other -> Shape.Tds_other
    | Tds_alias sh -> Shape.Tds_alias (compress_ts used_uids sh)
    | Tds_record { fields; kind } ->
      Shape.Tds_record
        { fields =
            List.map
              (fun (name, sh, layout) -> name, compress_ts used_uids sh, layout)
              fields;
          kind
        }
    | Tds_variant { simple_constructors; complex_constructors } ->
      Shape.Tds_variant
        { simple_constructors;
          complex_constructors =
            List.map
              (Shape.complex_constructor_map (fun (sh, ly) ->
                   compress_ts used_uids sh, ly))
              complex_constructors
        }
    | Tds_variant_unboxed { name; arg_name; arg_shape; arg_layout; _ } ->
      Shape.Tds_variant_unboxed
        { name;
          arg_name;
          arg_shape = compress_ts used_uids arg_shape;
          arg_layout
        }
  in
  let params = List.map (fun sh -> compress_ts used_uids sh) tds.type_params in
  { Shape.definition = desc; type_params = params }

and compress_ts (used_uids : Shape.Uid.Set.t) (ts : 'a Shape.ts) =
  let compress_ts_list tss =
    List.map (fun ts -> compress_ts used_uids ts) tss
  in
  match ts with
  | Ts_constr ((sh, ly), args) ->
    Shape.Ts_constr ((compress_shape used_uids sh, ly), compress_ts_list args)
  | Ts_tuple ts -> Shape.Ts_tuple (compress_ts_list ts)
  | Ts_unboxed_tuple ts -> Shape.Ts_unboxed_tuple (compress_ts_list ts)
  | Ts_var _ -> ts
  | Ts_predef _ -> ts
  | Ts_arrow (arg, ret) ->
    Shape.Ts_arrow (compress_ts used_uids arg, compress_ts used_uids ret)
  | Ts_variant (fields, kind) ->
    Shape.Ts_variant
      ( List.map
          (fun { Shape.pv_constr_args; pv_constr_name } ->
            { Shape.pv_constr_args = compress_ts_list pv_constr_args;
              pv_constr_name
            })
          fields,
        kind )
  | Ts_other _ -> ts

module With_cms_reduce = Shape_reduce.Make (struct
  let fuel = 10

  let read_unit_shape ~unit_name =
    let filename = String.uncapitalize_ascii unit_name in
    match Load_path.find_normalized (filename ^ ".cms") with
    | exception Not_found -> None
    | fn ->
      (* CR tnowak: exception? *)
      let cms_infos = Cms_format.read fn in
      Shape.Uid.Tbl.iter
        (fun uid decl -> Shape.Uid.Tbl.add Type_shape.all_type_decls uid decl)
        cms_infos.cms_decl_table;
      (* CR sspies: Hacky way of dumping the shapes into .cms files and then
         retrieving them again. *)
      cms_infos.cms_impl_shape
end)

let variable_to_die state (var_uid : Uid.t) ~parent_proto_die =
  let fallback_value_die =
    Proto_die.reference (DS.value_type_proto_die state)
  in
  (* Once we reach the backend, layouts such as Product [Product [Bits64;
     Bits64]; Float64] have de facto been flattened into a sequence of base
     layouts [Bits64; Bits64; Float64]. Below, we compute the index into the
     flattened list (and later compute the flattened list itself). *)
  let uid_to_lookup, unboxed_projection =
    match var_uid with
    | Uid var_uid -> var_uid, None
    | Proj (var_uid, field) -> var_uid, Some field
  in
  match Shape.Uid.Tbl.find_opt Type_shape.all_type_shapes uid_to_lookup with
  | None ->
    Format.eprintf "variable_to_die: no type shape for %a@." Shape.Uid.print
      uid_to_lookup;
    fallback_value_die
  (* CR sspies: This is somewhat dangerous, since this is a variable for which
     we seem to have no declaration, and we also do not know the layout. Perhaps
     we should simply not emit any DWARF information for this variable
     instead. *)
  | Some (type_shape, type_name) -> (
    let shape_reduce =
      With_cms_reduce.reduce_ts Env.empty ~uid_lookup:(fun uid ->
          Option.map
            (Shape.type_decl (Some uid))
            (Type_shape.find_in_type_decls uid))
    in
    Format.eprintf "pre reduce shape = %a@." Shape.print_ts type_shape;
    let layout = Shape.shape_layout type_shape in
    let type_shape = shape_reduce (Shape.forget_layout type_shape) in
    (* Format.eprintf "post reduce shape = %a@." Shape.print_ts type_shape; *)
    let bound = used_uids_ts type_shape in
    (* Format.eprintf "used uids = %a@." Shape.Uid.Set.print bound; *)
    let type_shape = compress_ts bound type_shape in
    (* Format.eprintf "post compress shape = %a@." Shape.print_ts type_shape; *)
    let type_shape = Shape.shape_with_layout ~layout type_shape in
    Format.eprintf "final shape = %a@." Shape.print_ts type_shape;
    let type_shape =
      match unboxed_projection with
      | None -> `Known type_shape
      | Some i ->
        let flattened = flatten_type_shape type_shape in
        if i < 0 || i >= List.length flattened
        then Misc.fatal_error "unboxed projection index out of bounds";
        List.nth flattened i
    in
    match type_shape with
    | `Known type_shape ->
      let reference =
        type_shape_layout_to_die ~type_name type_shape ~parent_proto_die
          ~fallback_value_die
      in
      if debug_emit_dwarf_dies
      then (
        Format.eprintf "var %a has become %a@." Uid.print var_uid
          Asm_targets.Asm_label.print reference;
        emit_debug_info ~die:parent_proto_die);
      reference
    | `Unknown base_layout ->
      let reference = Proto_die.create_reference () in
      create_base_layout_type ~reference ~parent_proto_die
        ~name:(type_name ^ " @ " ^ Jkind_types.Sort.to_string_base base_layout)
        ~fallback_value_die base_layout ();
      reference)
