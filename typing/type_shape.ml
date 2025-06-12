[@@@ocaml.warning "+a-40-42"]

module Uid = Shape.Uid

module Type_shape = struct

  (* Similarly to [value_kind], we track a set of visited types to avoid cycles
     in the lookup and we, additionally, carry a maximal depth for the recursion.
     We allow a deeper bound than [value_kind]. *)
  (* CR sspies: Consider additionally adding a max size for the set of visited types.
     Also consider reverting to the original value kind depth limit (although 2
     seems low). *)
  let rec of_type_expr_go ~visited ~depth (expr : Types.type_expr)

    shape_of_path
    =
    let open[@warning "-44"] Shape in
    let[@inline] cannot_proceed () =
      Numbers.Int.Set.mem (Types.get_id expr) visited || depth >= 10
    in
    if cannot_proceed ()
    then Ts_other Layout_to_be_determined
    else
      let visited = Numbers.Int.Set.add (Types.get_id expr) visited in
      let depth = depth + 1 in
      let desc = Types.get_desc expr in
      let map_expr_list (exprs : Types.type_expr list) =
        List.map (fun expr ->
            of_type_expr_go ~depth ~visited expr shape_of_path
          ) exprs
      in
      match desc with
      | Tconstr (path, constrs, _abbrev_memo) -> (
        match Predef.of_string (Path.name path) with
        | Some predef -> Ts_predef (predef, map_expr_list constrs)
        | None -> (
          match shape_of_path path with
          | Some shape ->
            Ts_constr
              ((shape, Layout_to_be_determined), map_expr_list constrs)
          | None -> Ts_other Layout_to_be_determined))
      | Ttuple exprs -> Ts_tuple (map_expr_list (List.map snd exprs))
      | Tvar { name; _ } -> Ts_var (name, Layout_to_be_determined)
      | Tpoly (type_expr, _type_vars) ->
        (* CR sspies: At the moment, we simply ignore the polymorphic variables.
           This code used to only work for [_type_vars = []]. *)
        of_type_expr_go ~depth ~visited type_expr shape_of_path
      | Tunboxed_tuple exprs ->
        Ts_unboxed_tuple (map_expr_list (List.map snd exprs))
      | Tobject _ | Tnil | Tfield _ ->
        Ts_other Layout_to_be_determined
        (* Objects are currently not supported in the debugger. *)
      | Tlink _ | Tsubst _ ->
        Misc.fatal_error "linking and substitution should not reach this stage."
      | Tvariant rd ->
        let row_fields = Types.row_fields rd in
        let row_kind = if Types.row_closed rd then Closed else Open in
        let row_fields =
          List.concat_map
            (fun (name, desc) ->
              match Types.row_field_repr desc with
              | Types.Rpresent (Some ty) ->
                [ { pv_constr_name = name;
                    pv_constr_args =
                      [of_type_expr_go ~depth ~visited ty shape_of_path]
                  } ]
              | Types.Rpresent None ->
                [{ pv_constr_name = name; pv_constr_args = [] }]
              | Types.Rabsent -> [] (* we filter out absent constructors *)
              | Types.Reither (_, args, _) ->
                [{ pv_constr_name = name; pv_constr_args = map_expr_list args }])
            row_fields
        in
        Ts_variant (row_fields, row_kind)
      | Tarrow (_, arg, ret, _) ->
        Ts_arrow
          ( of_type_expr_go ~depth ~visited arg shape_of_path,
            of_type_expr_go ~depth ~visited ret shape_of_path)
      | Tunivar { name; _ } -> Ts_var (name, Layout_to_be_determined)
      | Tpackage _ ->
        Ts_other
          Layout_to_be_determined (* CR sspies: Support first-class modules. *)

  let of_type_expr (expr : Types.type_expr) shape_of_path =
    of_type_expr_go ~visited:Numbers.Int.Set.empty ~depth:(-1) expr shape_of_path

  (* CR sspies: This function looks very different from a regular substitution.
     It seems to replace an entire type description, and it uses polymorphic
     equality to determine which one. *)
  let rec replace_tvar t ~(pairs : (Shape.without_layout Shape.ts * Shape.without_layout Shape.ts) list) =
    let open Shape in
    match
      List.filter_map
        (fun (from, to_) -> if t = from then Some to_ else None)
        pairs
    with
    | new_type :: _ -> new_type
    | [] -> (
      match t with
      | Ts_constr (uid, shape_list) ->
        Ts_constr (uid, List.map (replace_tvar ~pairs) shape_list)
      | Ts_tuple shape_list ->
        Ts_tuple (List.map (replace_tvar ~pairs) shape_list)
      | Ts_unboxed_tuple shape_list ->
        Ts_unboxed_tuple (List.map (replace_tvar ~pairs) shape_list)
      | Ts_var (name, ly) -> Ts_var (name, ly)
      | Ts_predef (predef, shape_list) -> Ts_predef (predef, shape_list)
      | Ts_arrow (arg, ret) ->
        Ts_arrow (replace_tvar ~pairs arg, replace_tvar ~pairs ret)
      | Ts_variant (fields, kind) ->
        Ts_variant
          ( List.map
              (fun field ->
                { field with
                  pv_constr_args =
                    List.map (replace_tvar ~pairs) field.pv_constr_args
                })
              fields,
            kind )
      | Ts_other ly -> Ts_other ly)

  module With_layout = struct
    include Identifiable.Make (struct
      type nonrec t = Shape.Layout.t Shape.ts

      let compare = Stdlib.compare

      let print = Shape.print_ts

      let hash = Hashtbl.hash

      let equal (x : t) y = x = y

      let output _oc _t = Misc.fatal_error "unimplemented"
    end)
  end
end

module Type_decl_shape = struct

  let mixed_block_shape_to_base_layout = function
    | Types.Value -> Jkind_types.Sort.Value
    | Types.Float_boxed ->
      Jkind_types.Sort.Float64
      (* [Float_boxed] records are unboxed inside the variant at runtime,
         contrary to the name.*)
    | Types.Float64 -> Jkind_types.Sort.Float64
    | Types.Float32 -> Jkind_types.Sort.Float32
    | Types.Bits32 -> Jkind_types.Sort.Bits32
    | Types.Bits64 -> Jkind_types.Sort.Bits64
    | Types.Vec128 -> Jkind_types.Sort.Vec128
    | Types.Word -> Jkind_types.Sort.Word

  let of_variant_constructor_with_args name
      (cstr_args : Types.constructor_declaration)
      ((constructor_repr, _) : Types.constructor_representation * _) shape_of_path
      =
    let open Shape in
    let args =
      match cstr_args.cd_args with
      | Cstr_tuple list ->
        List.map
          (fun ({ ca_type = type_expr; ca_sort = type_layout; _ } :
                 Types.constructor_argument) ->
            { field_name = None;
              field_value =
                Type_shape.of_type_expr type_expr shape_of_path, type_layout
            })
          list
      | Cstr_record list ->
        List.map
          (fun (lbl : Types.label_declaration) ->
            { field_name = Some (Ident.name lbl.ld_id);
              field_value =
                Type_shape.of_type_expr lbl.ld_type shape_of_path, lbl.ld_sort
            })
          list
    in
    let constructor_repr = match constructor_repr with
    | Constructor_mixed shapes ->
      let shapes_and_fields = List.combine (Array.to_list shapes) args in
      List.iter
        (fun (mix_shape, { field_name = _; field_value = _, ly }) ->
          let ly2 = Layout.Base (mixed_block_shape_to_base_layout mix_shape) in
          if not (Layout.equal ly ly2)
          then
            Misc.fatal_errorf
              "Type_shape: variant constructor with mismatched layout, has %a \
               but expected %a"
              Layout.format ly Layout.format ly2)
        shapes_and_fields;
      Constructor_mixed (Array.map mixed_block_shape_to_base_layout shapes)

    | Constructor_uniform_value ->
      List.iter
        (fun { field_name = _; field_value = _, ly } ->
          if not (Layout.equal ly (Layout.Base Value))
          then
            Misc.fatal_errorf
              "Type_shape: variant constructor with mismatched layout, has %a \
               but expected value"
              Layout.format ly)
        args;
      Constructor_uniform_value
    in
    { name; kind = constructor_repr; args }

  let is_empty_constructor_list (cstr_args : Types.constructor_declaration) =
    let length =
      match cstr_args.cd_args with
      | Cstr_tuple list -> List.length list
      | Cstr_record list -> List.length list
    in
    length = 0

  let record_of_labels ~shape_of_path kind labels =
    Shape.Tds_record
      { fields =
          List.map
            (fun (lbl : Types.label_declaration) ->
              ( Ident.name lbl.ld_id,
                Type_shape.of_type_expr lbl.ld_type shape_of_path,
                lbl.ld_sort ))
            labels;
        kind
      }

  let of_type_declaration (type_declaration : Types.type_declaration)
     shape_of_path =
    let module TypesPredef = Predef in
    let open Shape in
    let definition =
      match type_declaration.type_manifest with
      | Some type_expr ->
        Tds_alias (Type_shape.of_type_expr type_expr shape_of_path)
      | None -> (
        match type_declaration.type_kind with
        | Type_variant (cstr_list, Variant_boxed layouts, _unsafe_mode_crossing)
          ->
          let cstrs_with_layouts =
            List.combine cstr_list (Array.to_list layouts)
          in
          let simple_constructors, complex_constructors =
            List.partition_map
              (fun ((cstr, arg_layouts) : Types.constructor_declaration * _) ->
                let name = Ident.name cstr.cd_id in
                match is_empty_constructor_list cstr with
                | true -> Left name
                | false ->
                  Right
                    (of_variant_constructor_with_args name cstr arg_layouts
                      shape_of_path))
              cstrs_with_layouts
          in
          Tds_variant { simple_constructors; complex_constructors }
        | Type_variant ([cstr], Variant_unboxed, _unsafe_mode_crossing)
          when not (is_empty_constructor_list cstr) ->
          let name = Ident.name cstr.cd_id in
          let field_name, type_expr, layout =
            match cstr.cd_args with
            | Cstr_tuple [ca] -> None, ca.ca_type, ca.ca_sort
            | Cstr_record [ld] ->
              Some (Ident.name ld.ld_id), ld.ld_type, ld.ld_sort
            | Cstr_tuple _ | Cstr_record _ ->
              Misc.fatal_error "Unboxed variant must have exactly one argument."
          in
          Tds_variant_unboxed
            { name;
              arg_name = field_name;
              arg_layout = layout;
              arg_shape = Type_shape.of_type_expr type_expr shape_of_path
            }
        | Type_variant ([_], Variant_unboxed, _unsafe_mode_crossing) ->
          Misc.fatal_error "Unboxed variant must have constructor arguments."
        | Type_variant (([] | _ :: _ :: _), Variant_unboxed, _) ->
          Misc.fatal_error "Unboxed variant must have exactly one constructor."
        | Type_variant
            (_, (Variant_extensible | Variant_with_null), _unsafe_mode_crossing)
          ->
          Tds_other (* CR sspies: These variants are not yet supported. *)
        | Type_record (lbl_list, record_repr, _unsafe_mode_crossing) -> (
          match record_repr with
          (* CR sspies: Why is there another copy of the layouts of the fields
             here? Which one should we use? Shouldn't they both be just values? *)
          | Record_boxed _ ->
            record_of_labels ~shape_of_path Record_boxed lbl_list
          | Record_mixed fields ->
            record_of_labels ~shape_of_path
              (Record_mixed (Array.map mixed_block_shape_to_base_layout fields))
              lbl_list
          | Record_unboxed ->
            record_of_labels ~shape_of_path Record_unboxed lbl_list
          | Record_float | Record_ufloat ->
            let lbl_list =
              List.map
                (fun (lbl : Types.label_declaration) ->
                  { lbl with
                    ld_sort = Base Float64;
                    ld_type = TypesPredef.type_unboxed_float
                  })
                  (* CR sspies: We are changing the type and the layout here. Consider
                     adding a name for the types of the fields instead of replacing
                     it with [float#]. *)
                lbl_list
            in
            record_of_labels ~shape_of_path Record_floats lbl_list
          | Record_inlined _ ->
            Misc.fatal_error "inlined records not allowed here"
            (* Inline records of this form should not occur as part of type delcarations.
               They do not exist for top-level declarations, but they do exist tempoarily
               such as inside of a match (e.g., [t] is an inline record in
               [match e with Foo t -> ...]). *))
        | Type_abstract _ -> Tds_other
        | Type_open -> Tds_other
        | Type_record_unboxed_product (lbl_list, _, _) ->
          record_of_labels ~shape_of_path Record_unboxed_product lbl_list)
    in
    let type_params =
      List.map
        (fun type_expr -> Type_shape.of_type_expr type_expr shape_of_path)
        type_declaration.type_params
    in
    { definition; type_params }

  (* CR sspies: This function instantiates the polymorphic variables in the type
     declarations. The corresponding functionality in the type checker is [Ctype.apply]
     for [Type_shape.replace_tvar]. The problem we are facing here is that we do this
     after conversion to shapes. Consider doing this pre conversion to shapes. *)
  let replace_tvar (t : Shape.tds)
      (shapes : Shape.without_layout Shape.ts list) =
    let open Shape in
    let debug = false in
    if debug
    then
      Format.eprintf "replacing tvar %a; %a; %a\n%!" Shape.print_tds t
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Shape.print_ts)
        shapes
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Shape.print_ts)
        t.type_params;
    match List.length t.type_params = List.length shapes with
    | true ->
      let subst = List.combine t.type_params shapes in
      let replace_tvar (sh, ly) = Type_shape.replace_tvar ~pairs:subst sh, ly in
      let ret =
        { type_params = [];
          definition =
            (match t.definition with
            | Tds_variant { simple_constructors; complex_constructors } ->
              Tds_variant
                { simple_constructors;
                  complex_constructors =
                    List.map
                      (complex_constructor_map replace_tvar)
                      complex_constructors
                }
            | Tds_variant_unboxed { name; arg_name; arg_shape; arg_layout } ->
              Tds_variant_unboxed
                { name;
                  arg_name;
                  arg_shape = Type_shape.replace_tvar ~pairs:subst arg_shape;
                  arg_layout
                }
            | Tds_record { fields; kind } ->
              Tds_record
                { fields =
                    List.map
                      (fun (name, sh, ly) ->
                        name, Type_shape.replace_tvar ~pairs:subst sh, ly)
                      fields;
                  kind
                }
            | Tds_alias type_shape ->
              Tds_alias (Type_shape.replace_tvar ~pairs:subst type_shape)
            | Tds_other -> Tds_other)
        }
      in
      ret
    | false ->
      (* CR tnowak: investigate *)
      { type_params = []; definition = Tds_other }
end

let (all_type_decls : Shape.tds Uid.Tbl.t) = Uid.Tbl.create 16

let (all_type_shapes : (Shape.Layout.t Shape.ts * string) Uid.Tbl.t) = Uid.Tbl.create 16

let add_to_type_decls (type_decl : Types.type_declaration) shape_of_path =
  let type_decl_shape =
    Type_decl_shape.of_type_declaration type_decl shape_of_path
  in
  Uid.Tbl.add all_type_decls type_decl.type_uid type_decl_shape

let add_to_type_shapes var_uid type_expr sort ~name shape_of_path =
  let type_shape = Type_shape.of_type_expr type_expr shape_of_path in
  let type_shape = Shape.shape_with_layout ~layout:sort type_shape in
  Uid.Tbl.add all_type_shapes var_uid (type_shape, name)

(* CR sspies: The original code of [compilation_unit_from_path], namely
   [split_type_path_at_compilation_unit], split the path here into compilation
   unit and the path. However, this code created fresh identifiers (leading to
   unexpected changes in variable names) and always discarded the path. Hence,
   this new version only extracts the compilation unit. *)
let rec compilation_unit_from_path (path : Path.t) =
  match path with
  | Pident _ | Papply _ -> None
  | Pdot (Pident i, _) ->
    if Ident.is_global i then Some (Ident.name i) else None
  | Pdot (path, _) ->
    let comp_unit = compilation_unit_from_path path in
    comp_unit
  | Pextra_ty (path, _) ->
    let comp_unit = compilation_unit_from_path path in
    comp_unit

let debug_type_search = false

(* CR sspies: This seems to not perform caching. I think [load_decls_from_cms]
   always goes to the file and loads it the original code. *)
let find_in_type_decls (type_uid : Uid.t) (type_path : Path.t option)
    ~(load_decls_from_cms : string -> Shape.tds Shape.Uid.Tbl.t) =
  if debug_type_search
  then Format.eprintf "trying to find type_uid = %a\n" Uid.print type_uid;
  (*= if debug_type_search
  then Format.eprintf "obtaining compilation unit of %a\n" Path.print type_path; *)
  let compilation_unit_type_decls =
    match Option.bind type_path compilation_unit_from_path with
    | Some compilation_unit -> (
      if debug_type_search
      then
        Format.eprintf "got compilation unit %a\n" Format.pp_print_string
          compilation_unit;
      (* CR tnowak: change the [String.uncapitalize_ascii] to a proper function. *)
      let filename = compilation_unit |> String.uncapitalize_ascii in
      match Load_path.find_normalized (filename ^ ".cms") with
      | exception Not_found ->
        if debug_type_search
        then
          Format.eprintf "not found filename %a" Format.pp_print_string filename;
        None
      | fn ->
        let type_decls = load_decls_from_cms fn in
        Some type_decls)
    | None ->
      if debug_type_search then Format.eprintf "same unit\n";
      Some all_type_decls
  in
  Option.bind compilation_unit_type_decls (fun tbl ->
      Uid.Tbl.find_opt tbl type_uid)

(*= let rec attach_head (path : Path.t) (new_head : Ident.t) =
  match path with
  | Pident ident -> Path.Pdot (Pident new_head, Ident.name ident)
  | Pdot (l, r) -> Path.Pdot (attach_head l new_head, r)
  | Papply (l, r) -> Path.Papply (attach_head l new_head, attach_head r new_head)
  | Pextra_ty (l, extra) -> Path.Pextra_ty (attach_head l new_head, extra) *)

(*= let attach_compilation_unit_to_path (path : Path.t)
    (compilation_unit : Compilation_unit.t) =
  match compilation_unit_from_path path with
  | None ->
    attach_head path
      (Compilation_unit.to_global_ident_for_bytecode compilation_unit)
  | Some _ -> path *)

(* CR sspies: I don't know what this function does. *)
let[@warning "-27"] attach_compilation_unit_to_paths (type_decl : Shape.tds)
    ~(compilation_unit : Compilation_unit.t) =
  Misc.fatal_error "Unimplemented"
  (*= let[@warning "-4"] attach_to_shape = function
    | Shape.Ts_constr ((uid, path, ly), ts) ->
      Shape.Ts_constr
        ((uid, attach_compilation_unit_to_path path compilation_unit, ly), ts)
    | _ as x -> x
  in
  { Type_decl_shape.path =
      attach_compilation_unit_to_path type_decl.path compilation_unit;
    type_params = List.map attach_to_shape type_decl.type_params;
    definition =
      (match type_decl.definition with
      | Tds_variant { simple_constructors; complex_constructors } ->
        Tds_variant
          { simple_constructors;
            complex_constructors =
              List.map
                (Type_decl_shape.complex_constructor_map (fun (sh, ly) ->
                     attach_to_shape sh, ly))
                complex_constructors
          }
      | Tds_variant_unboxed { name; arg_name; arg_shape; arg_layout } ->
        Tds_variant_unboxed
          { name; arg_name; arg_shape = attach_to_shape arg_shape; arg_layout }
      | Tds_record { fields; kind } ->
        Tds_record
          { fields =
              List.map
                (fun (name, sh, ly) -> name, attach_to_shape sh, ly)
                fields;
            kind
          }
      | Tds_alias shape -> Tds_alias (attach_to_shape shape)
      | Tds_other -> Tds_other)
  } *)

let print_table ppf (columns : (string * string list) list) =
  if List.length columns = 0 then Misc.fatal_errorf "print_table: empty table";
  let column_widths =
    List.map
      (fun (name, entries) ->
        List.fold_left max (String.length name) (List.map String.length entries))
      columns
  in
  let table_depth = List.hd columns |> snd |> List.length in
  let table_width =
    List.fold_left ( + ) 0 column_widths
    + 4 (* boundary characters *)
    + ((List.length column_widths - 1) * 3 (* inter column boundaries *))
  in
  let columns = List.combine column_widths columns in
  let columns =
    List.map
      (fun (w, (name, entries)) -> w, name, Array.of_list entries)
      columns
  in
  Format.fprintf ppf "%s\n" (String.make table_width '-');
  let headers =
    List.map
      (fun (w, name, _) ->
        Format.asprintf "%s%s" name (String.make (w - String.length name) ' '))
      columns
  in
  Format.fprintf ppf "| %s |\n" (String.concat " | " headers);
  Format.fprintf ppf "%s\n" (String.make table_width '-');
  let print_row ppf i =
    let row_strings =
      List.map
        (fun (w, _, entries) ->
          Format.asprintf "%s%s" entries.(i)
            (String.make (w - String.length entries.(i)) ' '))
        columns
    in
    Format.fprintf ppf "| %s |\n" (String.concat " | " row_strings)
  in
  for i = 0 to table_depth - 1 do
    print_row ppf i
  done;
  Format.fprintf ppf "%s\n" (String.make table_width '-')

let print_table_all_type_decls ppf =
  let entries = Uid.Tbl.to_list all_type_decls in
  let entries = List.sort (fun (a, _) (b, _) -> Uid.compare a b) entries in
  let entries =
    List.map
      (fun (k, v) ->
        ( Format.asprintf "%a" Uid.print k,
          Format.asprintf "%a" Shape.print_tds v ))
      entries
  in
  let uids, decls = List.split entries in
  print_table ppf ["UID", uids; "Type Declaration", decls]

let print_table_all_type_shapes ppf =
  let entries = Uid.Tbl.to_list all_type_shapes in
  let entries = List.sort (fun (a, _) (b, _) -> Uid.compare a b) entries in
  let entries =
    List.map
      (fun (k, (type_shape, name)) ->
        ( Format.asprintf "%a" Uid.print k,
          ( Format.asprintf "%a" Shape.print_ts type_shape,
            (name,
            Format.asprintf "%a" Jkind_types.Sort.Const.format
              (Shape.shape_layout type_shape) )) ))
      entries
  in
  let uids, rest = List.split entries in
  let type_shapes, rest = List.split rest in
  let names, sorts = List.split rest in
  print_table ppf ["UID", uids; "Type", names; "Sort", sorts; "Shape", type_shapes]
