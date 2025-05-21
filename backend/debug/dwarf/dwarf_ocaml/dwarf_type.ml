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

let wrap_die_under_a_pointer ~proto_die ~reference ~parent_proto_die =
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Reference_type
    ~attribute_values:
      [DAH.create_byte_size_exn ~byte_size:8; DAH.create_type ~proto_die]
    ()

let create_array_die ~reference ~parent_proto_die ~child_die ~name =
  let array_die =
    Proto_die.create ~parent:(Some parent_proto_die) ~tag:Dwarf_tag.Array_type
      ~attribute_values:
        [ DAH.create_name name;
          DAH.create_type_from_reference ~proto_die_reference:child_die;
          (* We can't use DW_AT_byte_size or DW_AT_bit_size since we don't know
             how large the array might be. *)
          (* DW_AT_byte_stride probably isn't required strictly speaking, but
             let's add it for the avoidance of doubt. *)
          DAH.create_byte_stride ~bytes:(Numbers.Int8.of_int_exn Arch.size_addr)
        ]
      ()
  in
  Proto_die.create_ignore ~parent:(Some array_die) ~tag:Dwarf_tag.Subrange_type
    ~attribute_values:
      [ (* Thankfully, all that lldb cares about is DW_AT_count. *)
        DAH.create_count_const 0L ]
    ();
  wrap_die_under_a_pointer ~proto_die:array_die ~reference ~parent_proto_die

let create_char_die ~reference ~parent_proto_die ~name =
  (* As a char is an immediate value, we have to ignore the first bit.
     Unfortunately lldb supports bit offsets only on members of structs, so
     instead, we create a hacky enum containing all possible char values. *)
  let enum =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:
        [DAH.create_name name; DAH.create_byte_size_exn ~byte_size:8]
        (* CR sspies: The name here is displayed as ["enum " ^ name] in gdb, but
           correctly as [name] in lldb. *)
      ()
  in
  List.iter
    (fun i ->
      Proto_die.create_ignore ~parent:(Some enum) ~tag:Dwarf_tag.Enumerator
        ~attribute_values:
          [ DAH.create_const_value ~value:(Int64.of_int ((2 * i) + 1));
            DAH.create_name (Printf.sprintf "%C" (Char.chr i)) ]
        ())
    (List.init 256 (fun i -> i))

let create_unboxed_base_layout_die ~reference ~parent_proto_die ~name ~byte_size
    ~encoding =
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Base_type
    ~attribute_values:
      [ DAH.create_name name;
        DAH.create_byte_size_exn ~byte_size;
        DAH.create_encoding ~encoding ]
    ()

let create_typedef_die ~reference ~parent_proto_die ~child_die ~name =
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Typedef
    ~attribute_values:
      [ DAH.create_name name;
        DAH.create_type_from_reference ~proto_die_reference:child_die ]
    ()

let create_record_die ~reference ~parent_proto_die ~name ~fields =
  let total_size =
    List.fold_left (fun acc (_, field_size, _) -> acc + field_size) 0 fields
  in
  let structure =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        [DAH.create_byte_size_exn ~byte_size:total_size; DAH.create_name name]
      ()
  in
  let offset = ref 0 in
  List.iter
    (fun (field_name, field_size, field_die) ->
      Proto_die.create_ignore ~parent:(Some structure) ~tag:Dwarf_tag.Member
        ~attribute_values:
          [ DAH.create_name field_name;
            DAH.create_type_from_reference ~proto_die_reference:field_die;
            DAH.create_data_member_location_offset
              ~byte_offset:(Int64.of_int !offset) ]
        ();
      offset := !offset + field_size)
    fields;
  wrap_die_under_a_pointer ~proto_die:structure ~reference ~parent_proto_die

(* Contrary to what one might assume from the name, the following function only
   creates one kind of unboxed record. The record for [[@@unboxed]]. The records
   of the form [#{ ... }] are destructed into their component parts by
   unarization. *)
let create_unboxed_record_die ~reference ~parent_proto_die ~name ~field_die
    ~field_name ~field_size =
  let structure =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        [DAH.create_byte_size_exn ~byte_size:field_size; DAH.create_name name]
      ()
  in
  Proto_die.create_ignore ~parent:(Some structure) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_name field_name;
        DAH.create_type_from_reference ~proto_die_reference:field_die;
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0) ]
    ()

let create_simple_variant_die ~reference ~parent_proto_die ~name
    ~simple_constructors =
  let enum =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:
        [DAH.create_byte_size_exn ~byte_size:8; DAH.create_name name]
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

let reorder_record_fields_for_mixed_record ~mixed_block_shapes fields =
  (* We go into arrays and back, because it makes the reordering of the fields
     via accesses O(n) instead of O(n^2) *)
  let fields = Array.of_list fields in
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
  | Types.Constructor_uniform_value -> fields
  | Types.Constructor_mixed mixed_block_shapes ->
    reorder_record_fields_for_mixed_record ~mixed_block_shapes fields

(* CR sspies: This is a very hacky way of doing an unboxed variant with just a
   single constructor. DWARF variants expect to have a discriminator. So what we
   do is pick the first bit of the contents of the unboxed variant as the
   discriminator and then we simply ouput the same DWARF information for both
   cases. *)
let create_unboxed_variant_die ~reference ~parent_proto_die ~name ~constr_name
    ~arg_name ~arg_layout ~arg_die =
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
      ~attribute_values:
        [DAH.create_byte_size_exn ~byte_size:width; DAH.create_name name]
      ()
  in
  let structure_die =
    Proto_die.create ~reference:structure_ref ~parent:(Some parent_proto_die)
      ~attribute_values:
        [DAH.create_byte_size_exn ~byte_size:width; DAH.create_name name]
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
    let member_name =
      match arg_name with Some name -> [DAH.create_name name] | None -> []
    in
    Proto_die.create_ignore ~parent:(Some constructor_variant)
      ~tag:Dwarf_tag.Member
      ~attribute_values:
        ([ DAH.create_type_from_reference ~proto_die_reference:arg_die;
           DAH.create_byte_size_exn ~byte_size:width;
           DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0)
         ]
        @ member_name)
      ()
  done

let create_complex_variant_die ~reference ~parent_proto_die ~name
    ~simple_constructors
    ~(complex_constructors :
       (Proto_die.reference * Jkind_types.Sort.base)
       Type_shape.Type_decl_shape.complex_constructor
       list) =
  let complex_constructors_names =
    List.map
      (fun { Type_shape.Type_decl_shape.name; kind = _; args = _ } -> name)
      complex_constructors
  in
  let value_size = Arch.size_addr in
  let variant_part_immediate_or_pointer =
    let int_or_ptr_structure =
      Proto_die.create ~reference ~parent:(Some parent_proto_die)
        ~attribute_values:
          [DAH.create_byte_size_exn ~byte_size:value_size; DAH.create_name name]
        ~tag:Dwarf_tag.Structure_type ()
    in
    Proto_die.create ~parent:(Some int_or_ptr_structure) ~attribute_values:[]
      ~tag:Dwarf_tag.Variant_part ()
  in
  let enum_immediate_or_pointer =
    let enum_die =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Enumeration_type
        ~attribute_values:
          [ DAH.create_byte_size_exn ~byte_size:value_size;
            DAH.create_name ("Enum ptr/immediate case " ^ name) ]
        ()
    in
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
    Proto_die.add_or_replace_attribute_value variant_part_immediate_or_pointer
      (DAH.create_discr ~proto_die_reference:(Proto_die.reference member_die))
  in
  let _enum_simple_constructor =
    let enum_die =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Enumeration_type
        ~attribute_values:
          [ DAH.create_byte_size_exn ~byte_size:value_size;
            DAH.create_name
              (name ^ " simple constructor enum "
              ^ String.concat "," simple_constructors) ]
        ()
    in
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
    Proto_die.create_ignore ~parent:(Some variant_immediate_case)
      ~tag:Dwarf_tag.Member
      ~attribute_values:
        [ DAH.create_type ~proto_die:enum_die;
          DAH.create_bit_size (Numbers.Int8.of_int_exn ((value_size * 8) - 1));
          DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0);
          DAH.create_data_bit_offset ~bit_offset:(Numbers.Int8.of_int_exn 1) ]
      ()
  in
  let _variant_complex_constructors =
    let ptr_case_structure =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Structure_type
        ~attribute_values:
          [ DAH.create_byte_size_exn ~byte_size:value_size;
            DAH.create_ocaml_offset_record_from_pointer
              ~value:(Int64.of_int (-value_size));
            DAH.create_name
              ("variant_part " ^ name ^ " "
              ^ String.concat "," complex_constructors_names) ]
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
      let variant_pointer =
        Proto_die.create ~parent:(Some variant_part_immediate_or_pointer)
          ~tag:Dwarf_tag.Variant
          ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int 0)]
          ()
      in
      Proto_die.create_ignore ~parent:(Some variant_pointer)
        ~tag:Dwarf_tag.Member
        ~attribute_values:
          [ DAH.create_type ~proto_die:ptr_case_pointer_to_structure;
            DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0)
          ]
        ()
    in
    let variant_part_pointer =
      Proto_die.create ~parent:(Some ptr_case_structure) ~attribute_values:[]
        ~tag:Dwarf_tag.Variant_part ()
    in
    let _enum_complex_constructor =
      let enum_die =
        Proto_die.create ~parent:(Some parent_proto_die)
          ~tag:Dwarf_tag.Enumeration_type
          ~attribute_values:
            [ DAH.create_byte_size_exn ~byte_size:1;
              DAH.create_name
                (name ^ " " ^ String.concat "," complex_constructors_names) ]
          ()
      in
      List.iteri
        (fun i { Type_shape.Type_decl_shape.name; kind = _; args = _ } ->
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
      (fun i
           { Type_shape.Type_decl_shape.name = _;
             kind = constructor_kind;
             args
           } ->
        let subvariant =
          Proto_die.create ~parent:(Some variant_part_pointer)
            ~tag:Dwarf_tag.Variant
            ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int i)]
            ()
        in
        let args = variant_constructor_reorder_fields args constructor_kind in
        let offset = ref 0 in
        List.iter
          (fun { Type_shape.Type_decl_shape.field_name;
                 field_value = field_type, ly
               } ->
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

let create_tuple_die ~reference ~parent_proto_die ~name ~fields =
  let structure_type =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:(List.length fields * 8);
          DAH.create_name name ]
      ()
  in
  List.iteri
    (fun i field_die ->
      let member_attributes =
        [ DAH.create_type_from_reference ~proto_die_reference:field_die;
          DAH.create_data_member_location_offset
            ~byte_offset:(Int64.of_int (8 * i)) ]
      in
      Proto_die.create_ignore ~parent:(Some structure_type)
        ~tag:Dwarf_tag.Member ~attribute_values:member_attributes ())
    fields;
  wrap_die_under_a_pointer ~proto_die:structure_type ~reference
    ~parent_proto_die

let create_base_layout_type ~reference (sort : Jkind_types.Sort.base) ~name
    ~parent_proto_die ~fallback_value_die =
  let byte_size = base_layout_to_byte_size sort in
  match sort with
  | Value ->
    create_typedef_die ~reference ~parent_proto_die ~name
      ~child_die:fallback_value_die
  | Void ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name ~byte_size
      ~encoding:Encoding_attribute.signed
  | Float64 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name ~byte_size
      ~encoding:Encoding_attribute.float
  | Float32 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name ~byte_size
      ~encoding:Encoding_attribute.float
  | Word ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name ~byte_size
      ~encoding:Encoding_attribute.signed
  | Bits32 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name ~byte_size
      ~encoding:Encoding_attribute.signed
  | Bits64 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name ~byte_size
      ~encoding:Encoding_attribute.signed
  | Vec128 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name ~byte_size
      ~encoding:Encoding_attribute.unsigned

module Cache = Type_shape.Type_shape.With_layout.Tbl

type base_layout = Jkind_types.Sort.base

module Layout = Jkind_types.Sort.Const

let cache = Cache.create 16

let rec type_shape_layout_to_die (type_shape : Layout.t Type_shape.Type_shape.t)
    ~parent_proto_die ~fallback_value_die =
  match Cache.find_opt cache type_shape with
  | Some reference -> reference
  | None ->
    let reference = Proto_die.create_reference () in
    (* We add the combination of shape and layout early in case of recursive
       types, which can then look up their reference, before it is fully
       defined. That way [type myintlist = MyNil | MyCons of int * myintlist]
       will work correctly (as opposed to diverging). *)
    Cache.add cache type_shape reference;
    let type_name = Type_shape.type_name type_shape ~load_decls_from_cms in
    let layout_name =
      Format.asprintf "%a" Jkind_types.Sort.Const.format
        (Type_shape.Type_shape.shape_layout type_shape)
    in
    let name = type_name ^ " @ " ^ layout_name in
    (match type_shape with
    | Ts_other type_layout | Ts_var (_, type_layout) -> (
      match type_layout with
      | Base b ->
        create_base_layout_type ~reference b ~name ~parent_proto_die
          ~fallback_value_die
      | Product _ ->
        Misc.fatal_errorf
          "only base layouts supported, but found unboxed product layout %s"
          layout_name)
    | Ts_unboxed_tuple _ ->
      Misc.fatal_errorf "unboxed tuples cannot have base layout %s" layout_name
    | Ts_tuple fields ->
      type_shape_layout_tuple_die ~reference ~parent_proto_die
        ~fallback_value_die ~name fields
    | Ts_predef (predef, args) ->
      type_shape_layout_predef_die ~reference ~name ~parent_proto_die
        ~fallback_value_die predef args
    | Ts_constr ((type_uid, type_path, type_layout), shapes) -> (
      match type_layout with
      | Base b ->
        type_shape_layout_constructor_die ~reference ~name ~parent_proto_die
          ~fallback_value_die ~type_uid type_path b shapes
      | Product _ ->
        Misc.fatal_errorf
          "only base layouts supported, but found product layout %s" layout_name
      )
    | Ts_arrow (arg, ret) ->
      type_shape_layout_arrow_die ~reference ~name ~parent_proto_die
        ~fallback_value_die arg ret);
    reference

and type_shape_layout_tuple_die ~name ~reference ~parent_proto_die
    ~fallback_value_die fields =
  let fields =
    List.map
      (fun shape ->
        type_shape_layout_to_die ~parent_proto_die ~fallback_value_die shape)
      fields
  in
  create_tuple_die ~reference ~parent_proto_die ~name ~fields

and type_shape_layout_predef_die ~name ~reference ~parent_proto_die
    ~fallback_value_die (predef : Type_shape.Type_shape.Predef.t) args =
  match predef, args with
  | Array, [element_type_shape] ->
    let element_type_shape =
      Type_shape.Type_shape.shape_with_layout ~layout:(Base Value)
        element_type_shape
    in
    (* CR sspies: Check whether the elements of an array are always values and,
       if not, where that information is maintained. *)
    let child_die =
      type_shape_layout_to_die ~parent_proto_die ~fallback_value_die
        element_type_shape
    in
    create_array_die ~reference ~parent_proto_die ~child_die ~name
  | Array, _ ->
    Misc.fatal_error "Array applied to zero or more than one type."
    (* CR sspies: What should we do in this case. The old code supported it,
       simply yielding the [fallback_value_die], but that seems strange. *)
  | Char, _ -> create_char_die ~reference ~parent_proto_die ~name
  | Unboxed b, _ ->
    let type_layout = Type_shape.Type_shape.Predef.unboxed_type_to_layout b in
    create_base_layout_type ~reference type_layout ~name ~parent_proto_die
      ~fallback_value_die
    (* CR sspies: Take [b] into account here, perhaps as an optional argument,
       to support int8x16 vs float64x2. *)
  | ( ( Bytes | Extension_constructor | Float | Floatarray | Int | Int32 | Int64
      | Lazy_t | Nativeint | String ),
      _ ) ->
    create_base_layout_type ~reference Value ~name ~parent_proto_die
      ~fallback_value_die

and type_shape_layout_constructor_die ~reference ~name ~parent_proto_die
    ~fallback_value_die ~type_uid type_path (type_layout : base_layout) shapes =
  match
    (* CR sspies: Somewhat subtly, this case currently also handles [unit],
       [bool], [option], and [list], because they are not treated as predefined
       types and do have declarations. *)
    Type_shape.find_in_type_decls type_uid type_path ~load_decls_from_cms
  with
  | None ->
    create_base_layout_type ~reference type_layout ~name ~parent_proto_die
      ~fallback_value_die
  | Some type_decl_shape -> (
    let type_decl_shape =
      Type_shape.Type_decl_shape.replace_tvar type_decl_shape shapes
    in
    match type_decl_shape.definition with
    | Tds_other ->
      create_base_layout_type ~reference type_layout ~name ~parent_proto_die
        ~fallback_value_die
    | Tds_alias alias_shape ->
      let alias_shape =
        Type_shape.Type_shape.shape_with_layout ~layout:(Base type_layout)
          alias_shape
      in
      let alias_die =
        type_shape_layout_to_die alias_shape ~parent_proto_die
          ~fallback_value_die
      in
      create_typedef_die ~reference ~parent_proto_die ~child_die:alias_die ~name
    | Tds_record { fields; kind = Record_boxed | Record_floats } ->
      let fields =
        List.map
          (fun (name, type_shape, type_layout) ->
            let type_shape' =
              Type_shape.Type_shape.shape_with_layout ~layout:type_layout
                type_shape
            in
            ( name,
              Arch.size_addr,
              (* field size for values*)
              type_shape_layout_to_die ~parent_proto_die ~fallback_value_die
                type_shape' ))
          fields
      in
      create_record_die ~reference ~parent_proto_die ~name ~fields
    | Tds_record { fields = _; kind = Record_unboxed_product } ->
      Misc.fatal_error
        "Unboxed records should not reach this stage. They are deconstructed \
         by unarization in earlier stages of the compiler."
    | Tds_record
        { fields = [(field_name, sh, Base base_layout)]; kind = Record_unboxed }
      ->
      let field_shape =
        Type_shape.Type_shape.shape_with_layout ~layout:(Base base_layout) sh
      in
      let field_die =
        type_shape_layout_to_die ~parent_proto_die ~fallback_value_die
          field_shape
      in
      let field_size = base_layout_to_byte_size base_layout in
      create_unboxed_record_die ~reference ~parent_proto_die ~name ~field_die
        ~field_name ~field_size
      (* The two cases below are filtered out by the flattening of shapes in
         [flatten_shape]. *)
    | Tds_record { fields = [] | _ :: _ :: _; kind = Record_unboxed } ->
      assert false
    | Tds_record { fields = [(_, _, Product _)]; kind = Record_unboxed } ->
      assert false
    | Tds_record { fields; kind = Record_mixed mixed_block_shapes } ->
      let fields =
        List.map
          (fun (name, type_shape, type_layout) ->
            let type_shape' =
              Type_shape.Type_shape.shape_with_layout ~layout:type_layout
                type_shape
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
      create_record_die ~reference ~parent_proto_die ~name ~fields
    | Tds_variant { simple_constructors; complex_constructors } -> (
      match complex_constructors with
      | [] ->
        create_simple_variant_die ~reference ~parent_proto_die ~name
          ~simple_constructors
      | _ :: _ ->
        let complex_constructors =
          List.map
            (Type_shape.Type_decl_shape.complex_constructor_map
               (fun (sh, layout) ->
                 match layout with
                 | Jkind_types.Sort.Const.Base ly ->
                   let sh =
                     Type_shape.Type_shape.shape_with_layout ~layout sh
                   in
                   ( type_shape_layout_to_die ~parent_proto_die
                       ~fallback_value_die sh,
                     ly )
                 | Jkind_types.Sort.Const.Product _ ->
                   Misc.fatal_error
                     "unboxed product in complex constructor is not allowed"))
            complex_constructors
        in
        create_complex_variant_die ~reference ~parent_proto_die ~name
          ~simple_constructors ~complex_constructors)
    | Tds_variant_unboxed
        { name = constr_name; arg_name; arg_shape; arg_layout } ->
      let arg_shape =
        Type_shape.Type_shape.shape_with_layout ~layout:arg_layout arg_shape
      in
      let arg_die =
        type_shape_layout_to_die ~parent_proto_die ~fallback_value_die arg_shape
      in
      create_unboxed_variant_die ~reference ~parent_proto_die ~name ~constr_name
        ~arg_name ~arg_layout ~arg_die)

and type_shape_layout_arrow_die ~reference ~name ~parent_proto_die
    ~fallback_value_die _arg _ret =
  (* There is no need to inspect the argument and return value. *)
  create_typedef_die ~reference ~parent_proto_die ~name
    ~child_die:fallback_value_die

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
let rec flatten_shape
    (type_shape : Jkind_types.Sort.Const.t Type_shape.Type_shape.t) =
  let unknown_base_layouts layout =
    let base_sorts = flatten_to_base_sorts layout in
    List.map (fun base_sort -> `Unknown base_sort) base_sorts
  in
  match type_shape with
  | Ts_var (_, Base _) -> [`Known type_shape]
  | Ts_var (_, (Product _ as type_layout)) -> unknown_base_layouts type_layout
  | Ts_tuple _ ->
    [`Known type_shape] (* tuples are only a single base layout wide *)
  | Ts_unboxed_tuple shapes -> List.concat_map flatten_shape shapes
  | Ts_predef _ -> [`Known type_shape]
  | Ts_arrow _ -> [`Known type_shape]
  | Ts_other layout ->
    let base_layouts = flatten_to_base_sorts layout in
    List.map (fun layout -> `Unknown layout) base_layouts
  | Ts_constr ((type_uid, type_path, layout), shapes) -> (
    match[@warning "-4"]
      Type_shape.find_in_type_decls type_uid type_path ~load_decls_from_cms
    with
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
        let alias_shape =
          Type_shape.Type_shape.shape_with_layout ~layout alias_shape
        in
        flatten_shape alias_shape
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
        | Product _ ->
          flatten_shape (Type_shape.Type_shape.shape_with_layout ~layout sh)
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
              (fun (_, sh, ly) ->
                Type_shape.Type_shape.shape_with_layout ~layout:ly sh)
              fields
          in
          List.concat_map flatten_shape shapes
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
  | Some type_shape -> (
    let type_shape =
      match unboxed_projection with
      | None -> `Known type_shape
      | Some i ->
        let flattened = flatten_shape type_shape in
        if i < 0 || i >= List.length flattened
        then Misc.fatal_error "unboxed projection index out of bounds";
        List.nth flattened i
    in
    match type_shape with
    | `Known type_shape ->
      type_shape_layout_to_die type_shape ~parent_proto_die ~fallback_value_die
    | `Unknown base_layout ->
      let reference = Proto_die.create_reference () in
      create_base_layout_type ~reference ~parent_proto_die
        ~name:("unknown @ " ^ Jkind_types.Sort.to_string_base base_layout)
        ~fallback_value_die base_layout;
      reference)
