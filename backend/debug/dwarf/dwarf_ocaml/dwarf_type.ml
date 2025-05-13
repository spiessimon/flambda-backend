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

let cache = Type_shape.Type_shape.Tbl.create 16

let load_decls_from_cms _path =
  (* let cms_infos = Cms_format.read path in cms_infos.cms_shapes_for_dwarf *)
  (* CR sspies: We return an empty table here for now, because we have not yet
     agumented the [.cms] format to store the relevant shape information. *)
  Shape.Uid.Tbl.create 0

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

let create_char_die ~reference ~parent_proto_die =
  (* As a char is an immediate value, we have to ignore the first bit.
     Unfortunately lldb supports bit offsets only on members of structs, so
     instead, we create a hacky enum containing all possible char values. *)
  let enum =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:
        [DAH.create_name "char"; DAH.create_byte_size_exn ~byte_size:8]
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
  let structure =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:(8 * List.length fields);
          DAH.create_name name ]
      ()
  in
  List.iteri
    (fun i (field_name, field_die) ->
      Proto_die.create_ignore ~parent:(Some structure) ~tag:Dwarf_tag.Member
        ~attribute_values:
          [ DAH.create_name field_name;
            DAH.create_type_from_reference ~proto_die_reference:field_die;
            DAH.create_data_member_location_offset
              ~byte_offset:(Int64.of_int (8 * i)) ]
        ())
    fields;
  wrap_die_under_a_pointer ~proto_die:structure ~reference ~parent_proto_die

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

let create_complex_variant_die ~reference ~parent_proto_die ~name
    ~simple_constructors
    ~(complex_constructors :
       Proto_die.reference Type_shape.Type_decl_shape.complex_constructor list)
    =
  let complex_constructors_names =
    List.map
      (fun { Type_shape.Type_decl_shape.name; args = _ } -> name)
      complex_constructors
  in
  let variant_part_immediate_or_pointer =
    let int_or_ptr_structure =
      Proto_die.create ~reference ~parent:(Some parent_proto_die)
        ~attribute_values:
          [DAH.create_byte_size_exn ~byte_size:8; DAH.create_name name]
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
          [ DAH.create_byte_size_exn ~byte_size:8;
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
          [ DAH.create_byte_size_exn ~byte_size:8;
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
          DAH.create_bit_size (Numbers.Int8.of_int_exn 63);
          DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0);
          DAH.create_data_bit_offset ~bit_offset:(Numbers.Int8.of_int_exn 1) ]
      ()
  in
  let _variant_complex_constructors =
    let ptr_case_structure =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Structure_type
        ~attribute_values:
          [ DAH.create_byte_size_exn ~byte_size:8;
            DAH.create_ocaml_offset_record_from_pointer
              ~value:(Int64.of_int (-8));
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
            [ DAH.create_byte_size_exn ~byte_size:8;
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
        (fun i { Type_shape.Type_decl_shape.name; args = _ } ->
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
      (fun i { Type_shape.Type_decl_shape.name = _; args } ->
        let subvariant =
          Proto_die.create ~parent:(Some variant_part_pointer)
            ~tag:Dwarf_tag.Variant
            ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int i)]
            ()
        in
        List.iteri
          (fun i
               { Type_shape.Type_decl_shape.field_name;
                 field_value = field_type
               } ->
            let member_die =
              Proto_die.create ~parent:(Some subvariant) ~tag:Dwarf_tag.Member
                ~attribute_values:
                  [ DAH.create_data_member_location_offset
                      ~byte_offset:(Int64.of_int (8 * (1 + i)));
                    DAH.create_byte_size_exn ~byte_size:8;
                    DAH.create_type_from_reference
                      ~proto_die_reference:field_type ]
                ()
            in
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

let rec value_type_shape_to_die (type_shape : Type_shape.Type_shape.t)
    ~parent_proto_die ~fallback_die =
  match Type_shape.Type_shape.Tbl.find_opt cache type_shape with
  | Some reference -> reference
  | None ->
    let reference = Proto_die.create_reference () in
    Type_shape.Type_shape.Tbl.add cache type_shape reference;
    let name = Type_shape.type_name type_shape ~load_decls_from_cms in
    let successfully_created =
      match type_shape with
      | Ts_other | Ts_var _ -> false
      | Ts_unboxed_tuple _ ->
        Misc.fatal_error "Unboxed tuple is not a value shape."
      | Ts_predef (Array, [element_type_shape]) ->
        let child_die =
          value_type_shape_to_die element_type_shape ~parent_proto_die
            ~fallback_die
        in
        create_array_die ~reference ~parent_proto_die ~child_die ~name;
        true
      | Ts_predef (Array, _) -> false
      | Ts_predef (Char, _) ->
        create_char_die ~reference ~parent_proto_die;
        true
      | Ts_predef ((Unboxed _ as predef), _) ->
        Misc.fatal_errorf "Unboxed type %s does not have OCaml value layout."
          (Type_shape.Type_shape.Predef.to_string predef)
      | Ts_predef
          ( ( Bytes | Extension_constructor | Float | Floatarray | Int | Int32
            | Int64 | Lazy_t | Nativeint | String ),
            _ ) ->
        create_typedef_die ~reference ~parent_proto_die ~name
          ~child_die:fallback_die;
        true
      | Ts_constr ((type_uid, type_path), shapes) -> (
        match(* CR sspies: Somewhat subtly, this case currently also handles
                [unit], [bool], [option], and [list], because they are not
                treated as predefined types, but they do have declarations. *)
             [@warning "-4"]
          Type_shape.find_in_type_decls type_uid type_path ~load_decls_from_cms
        with
        | None | Some { definition = Tds_other; _ } -> false
        | Some type_decl_shape -> (
          let type_decl_shape =
            Type_shape.Type_decl_shape.replace_tvar type_decl_shape shapes
          in
          match type_decl_shape.definition with
          | Tds_other -> false
          | Tds_alias alias_shape ->
            let alias_die =
              value_type_shape_to_die alias_shape ~parent_proto_die
                ~fallback_die
            in
            create_typedef_die ~reference ~parent_proto_die ~child_die:alias_die
              ~name;
            true
          | Tds_record fields ->
            let fields =
              List.map
                (fun (name, type_shape) ->
                  ( name,
                    value_type_shape_to_die ~parent_proto_die ~fallback_die
                      type_shape ))
                fields
            in
            create_record_die ~reference ~parent_proto_die ~name ~fields;
            true
          | Tds_variant { simple_constructors; complex_constructors } -> (
            match complex_constructors with
            | [] ->
              create_simple_variant_die ~reference ~parent_proto_die ~name
                ~simple_constructors;
              true
            | _ :: _ ->
              let complex_constructors =
                List.map
                  (Type_shape.Type_decl_shape.complex_constructor_map
                     (value_type_shape_to_die ~parent_proto_die ~fallback_die))
                  complex_constructors
              in
              create_complex_variant_die ~reference ~parent_proto_die ~name
                ~simple_constructors ~complex_constructors;
              true)))
      | Ts_tuple fields ->
        let fields =
          List.map
            (value_type_shape_to_die ~parent_proto_die ~fallback_die)
            fields
        in
        create_tuple_die ~reference ~parent_proto_die ~name ~fields;
        true
    in
    let reference = if successfully_created then reference else fallback_die in
    Type_shape.Type_shape.Tbl.add (* replace *) cache type_shape reference;
    reference

let create_unboxed_base_type_to_die (sort : Jkind_types.Sort.base) type_shape
    ~parent_proto_die =
  let reference = Proto_die.create_reference () in
  let type_name =
    match type_shape with
    | Some type_shape -> Type_shape.type_name type_shape ~load_decls_from_cms
    | None -> "unknown"
    (* CR sspies: Extend the type_shapes to handle more builtin types. *)
  in
  (match sort with
  | Value -> Misc.fatal_error "ocaml values should not be treated as unboxed"
  | Void ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die
      ~name:(type_name ^ " @ void") ~byte_size:0
      ~encoding:Encoding_attribute.signed
  | Float64 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die
      ~name:(type_name ^ " @ float64") ~byte_size:8
      ~encoding:Encoding_attribute.float
  | Float32 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die
      ~name:(type_name ^ " @ float32") ~byte_size:4
      ~encoding:Encoding_attribute.float
  | Word ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die
      ~name:(type_name ^ " @ word") ~byte_size:Arch.size_addr
      ~encoding:Encoding_attribute.signed
  | Bits32 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die
      ~name:(type_name ^ " @ bits32") ~byte_size:4
      ~encoding:Encoding_attribute.signed
  | Bits64 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die
      ~name:(type_name ^ " @ bits64") ~byte_size:8
      ~encoding:Encoding_attribute.signed
  | Vec128 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die
      ~name:(type_name ^ " @ vec128") ~byte_size:16
      ~encoding:Encoding_attribute.signed);
  reference

let rec flatten_sort_aux (sort : Jkind_types.Sort.Const.t) :
    Jkind_types.Sort.Const.t list =
  match sort with
  | Base _ -> [sort]
  | Product sorts -> List.concat_map flatten_sort_aux sorts

let flatten_sort (sort : Jkind_types.Sort.Const.t) : Jkind_types.Sort.Const.t =
  match sort with
  | Base _ -> sort
  | Product sorts -> Product (List.concat_map flatten_sort_aux sorts)

let type_shape_to_die (type_shape : Type_shape.Type_shape.t option)
    (sort : Jkind_types.Sort.Const.t) ~parent_proto_die ~fallback_die =
  match sort with
  | Base Value -> (
    match type_shape with
    | Some type_shape ->
      value_type_shape_to_die type_shape ~parent_proto_die ~fallback_die
    | None -> fallback_die)
  | Base ((Void | Word | Float64 | Float32 | Bits32 | Bits64 | Vec128) as base)
    ->
    (* CR sspies: Fix that this creates a new type for every occurrence. *)
    create_unboxed_base_type_to_die base type_shape ~parent_proto_die
  | Product _ ->
    Misc.fatal_error
      "products should have been destructed by the time we reach assembly code."

let rec extract_unboxed_record_field_type_shape
    (type_shape : Type_shape.Type_shape.t) (length : int) (field : int) =
  match[@warning "-4"] type_shape with
  | Ts_constr ((type_uid, type_path), shapes) -> (
    match[@warning "-4"]
      Type_shape.find_in_type_decls type_uid type_path ~load_decls_from_cms
    with
    (* CR sspies: Instead of duplicating the logic for looking up type
       declarations, we should have one mechanism that produces the types in the
       representation that is relevant for the debugger. *)
    | None -> None
    | Some { definition = Tds_other; _ } -> None
    | Some type_decl_shape -> (
      let type_decl_shape =
        Type_shape.Type_decl_shape.replace_tvar type_decl_shape shapes
      in
      match type_decl_shape.definition with
      | Tds_other -> None
      | Tds_alias alias_shape ->
        extract_unboxed_record_field_type_shape alias_shape length field
        (* CR sspies: We still need to project out the field, so we recurse. We
           might want to add a bound here to avoid issues with type t = t *)
      | Tds_record _ ->
        (* This case should never happen, because we are looking at an unboxed
           tuple. *)
        None
      | Tds_variant _ ->
        (* This case should never happen, because we are looking at an unboxed
           tuple. *)
        None))
  | Ts_unboxed_tuple shapes when List.length shapes = length ->
    Some (List.nth shapes field)
  | _ ->
    (* If this is not an unboxed tuple of the expected length or a type
       definition, then we do not have enough information to produce the field
       shape (e.g., it could be a type variable). In this case, we drop the
       shape information and only use the layout for emitting the dwarf
       information. *)
    None

let variable_to_die state (var_uid : Uid.t) ~parent_proto_die =
  let fallback_die = Proto_die.reference (DS.value_type_proto_die state) in
  let uid_to_lookup, field =
    match var_uid with
    | Uid var_uid -> var_uid, None
    | Proj (var_uid, field) -> var_uid, Some field
  in
  match Shape.Uid.Tbl.find_opt Type_shape.all_type_shapes uid_to_lookup with
  | None ->
    Format.eprintf "variable_to_die: no type shape for %a@." Shape.Uid.print
      uid_to_lookup;
    fallback_die
  (* CR sspies: This is somewhat dangerous, since this is a variable for which
     we seem to have no declaration, and we also do not know the layout. Perhaps
     we should simply not emit any DWARF information for this variable
     instead. *)
  | Some { type_shape; type_sort } -> (
    let type_sort = flatten_sort type_sort in
    match type_sort, field with
    | Base _, None ->
      type_shape_to_die (Some type_shape) type_sort ~parent_proto_die
        ~fallback_die
    | Base _, Some _ -> Misc.fatal_error "cannot take field of base layout"
    | Product _, None ->
      Misc.fatal_error
        "products should have been destructed by the time we reach assembly \
         code"
    | Product sorts, Some field ->
      let num_of_sorts = List.length sorts in
      if field < 0 || field >= num_of_sorts
      then
        Misc.fatal_errorf
          "index out of bounds: attempting to access field %d via debug UID %a \
           in product sort %a"
          field Uid.print var_uid Jkind_types.Sort.Const.format type_sort;
      let sort = List.nth sorts field in
      let shape =
        extract_unboxed_record_field_type_shape type_shape num_of_sorts field
      in
      type_shape_to_die shape sort ~parent_proto_die ~fallback_die)
