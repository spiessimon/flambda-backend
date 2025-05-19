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

let create_char_die ~reference ~parent_proto_die ~name =
  (* As a char is an immediate value, we have to ignore the first bit.
     Unfortunately lldb supports bit offsets only on members of structs, so
     instead, we create a hacky enum containing all possible char values. *)
  let enum =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:
        [DAH.create_name name; DAH.create_byte_size_exn ~byte_size:8]
        (* CR sspies: Potentially add a type def here to avoid the enum having
           name [enum char] or the like. *)
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

let create_base_layout_type ~reference (sort : Jkind_types.Sort.base) ~name
    ~parent_proto_die ~fallback_value_die =
  match sort with
  | Value ->
    create_typedef_die ~reference ~parent_proto_die ~name
      ~child_die:fallback_value_die
  | Void ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name
      ~byte_size:0 ~encoding:Encoding_attribute.signed
  | Float64 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name
      ~byte_size:8 ~encoding:Encoding_attribute.float
  | Float32 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name
      ~byte_size:4 ~encoding:Encoding_attribute.float
  | Word ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name
      ~byte_size:Arch.size_addr ~encoding:Encoding_attribute.signed
  | Bits32 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name
      ~byte_size:4 ~encoding:Encoding_attribute.signed
  | Bits64 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name
      ~byte_size:8 ~encoding:Encoding_attribute.signed
  | Vec128 ->
    (*= CR sspies: 128-bit vectors can have very different layouts. If we do
      have the type available, we should turn this into a struct with the
      right fields. For example:
        int8x16   -> struct {int8#; ...; int8#}
        float64x2 -> struct {float64#; float64#}
    *)
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name
      ~byte_size:16 ~encoding:Encoding_attribute.unsigned

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
    | Tds_record fields ->
      let fields =
        List.map
          (fun (name, type_shape, type_layout) ->
            let type_shape' =
              Type_shape.Type_shape.shape_with_layout ~layout:type_layout
                type_shape
            in
            ( name,
              type_shape_layout_to_die ~parent_proto_die ~fallback_value_die
                type_shape' ))
          fields
        (* CR sspies: Ideally, these should all simply have their layout
           annotated if it is not value. *)
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
                 let sh = Type_shape.Type_shape.shape_with_layout ~layout sh in
                 type_shape_layout_to_die ~parent_proto_die ~fallback_value_die
                   sh))
            (* CR sspies: Can we be sure that these are always values? *)
            complex_constructors
        in
        create_complex_variant_die ~reference ~parent_proto_die ~name
          ~simple_constructors ~complex_constructors))

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
        (* CR sspies: We still need to project out the field, so we recurse. We
           might want to add a bound here to avoid issues with type t = t *)
      | Tds_record _ -> (
        match layout with
        | Base Value -> [`Known type_shape]
        | _ -> Misc.fatal_error "record must have value layout")
      | Tds_variant _ -> (
        match layout with
        | Base Value -> [`Known type_shape]
        | _ -> Misc.fatal_error "variant must have value layout")))

let variable_to_die state (var_uid : Uid.t) ~parent_proto_die =
  let fallback_die = Proto_die.reference (DS.value_type_proto_die state) in
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
    fallback_die
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
      type_shape_layout_to_die type_shape ~parent_proto_die
        ~fallback_value_die:fallback_die
    | `Unknown base_layout ->
      let reference = Proto_die.create_reference () in
      create_base_layout_type ~reference ~parent_proto_die
        ~name:("unknown @ " ^ Jkind_types.Sort.to_string_base base_layout)
        ~fallback_value_die:fallback_die base_layout;
      reference)
