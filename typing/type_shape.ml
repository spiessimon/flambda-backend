[@@@ocaml.warning "+a-40-42"]

module Uid = Shape.Uid

module Type_shape = struct
  module Predef = struct
    type unboxed =
      | Unboxed_float
      | Unboxed_float32
      | Unboxed_nativeint
      | Unboxed_int64
      | Unboxed_int32

    type t =
      | Array
      | Bytes
      | Char
      | Extension_constructor
      | Float
      | Floatarray
      | Int
      | Int32
      | Int64
      | Lazy_t
      | Nativeint
      | String
      (* Unboxed types *)
      | Unboxed of unboxed

    (* name of the type without the hash *)
    let unboxed_to_string (u : unboxed) =
      match u with
      | Unboxed_float -> "float"
      | Unboxed_float32 -> "float32"
      | Unboxed_nativeint -> "nativeint"
      | Unboxed_int64 -> "int64"
      | Unboxed_int32 -> "int32"

    let to_string = function
      | Array -> "array"
      | Bytes -> "bytes"
      | Char -> "char"
      | Extension_constructor -> "extension_constructor"
      | Float -> "float"
      | Floatarray -> "floatarray"
      | Int -> "int"
      | Int32 -> "int32"
      | Int64 -> "int64"
      | Lazy_t -> "lazy_t"
      | Nativeint -> "nativeint"
      | String -> "string"
      | Unboxed u -> unboxed_to_string u ^ "#"

    let unboxed_of_string = function
      | "float#" -> Some Unboxed_float
      | "float32#" -> Some Unboxed_float32
      | "nativeint#" -> Some Unboxed_nativeint
      | "int64#" -> Some Unboxed_int64
      | "int32#" -> Some Unboxed_int32
      | _ -> None

    let of_string = function
      | "array" -> Some Array
      | "bytes" -> Some Bytes
      | "char" -> Some Char
      | "extension_constructor" -> Some Extension_constructor
      | "float" -> Some Float
      | "floatarray" -> Some Floatarray
      | "int" -> Some Int
      | "int32" -> Some Int32
      | "int64" -> Some Int64
      | "lazy_t" -> Some Lazy_t
      | "nativeint" -> Some Nativeint
      | "string" -> Some String
      | s -> (
        match unboxed_of_string s with
        | Some u -> Some (Unboxed u)
        | None -> None)
  end

  type t =
    | Ts_constr of (Uid.t * Path.t) * t list
    | Ts_tuple of t list
    | Ts_unboxed_tuple of t list
    | Ts_var of string option
    | Ts_predef of Predef.t * t list
    | Ts_other

  (* Similarly to [value_kind], we track a set of visited types to avoid cycles
     in the lookup and we, additionally, carry a maximal depth for the recursion.
     We allow a deeper bound than [value_kind]. *)
  (* CR sspies: Consider additionally adding a max size for the set of visited types.
     Also consider reverting to the original value kind depth limit (although 2
     seems low). *)
  let rec of_type_expr_go ~visited ~depth (expr : Types.type_expr) uid_of_path =
    let[@inline] cannot_proceed () =
      Numbers.Int.Set.mem (Types.get_id expr) visited || depth >= 10
    in
    if cannot_proceed ()
    then Ts_other
    else
      let visited = Numbers.Int.Set.add (Types.get_id expr) visited in
      let depth = depth + 1 in
      let desc = Types.get_desc expr in
      let map_expr_list (exprs : Types.type_expr list) =
        List.map
          (fun expr -> of_type_expr_go ~depth ~visited expr uid_of_path)
          exprs
      in
      match[@warning "-4"] desc with
      (* CR sspies: Extend this match to handle more type constructor cases. *)
      | Tconstr (path, constrs, _abbrev_memo) -> (
        match Predef.of_string (Path.name path) with
        | Some predef -> Ts_predef (predef, map_expr_list constrs)
        | None -> (
          match uid_of_path path with
          | Some uid -> Ts_constr ((uid, path), map_expr_list constrs)
          | None -> Ts_other))
      | Ttuple exprs -> Ts_tuple (map_expr_list (List.map snd exprs))
      | Tvar { name; _ } -> Ts_var name
      | Tpoly (type_expr, _type_vars) ->
        (* CR sspies: At the moment, we simply ignore the polymorphic variables.
           This code used to only work for [_type_vars = []]. *)
        of_type_expr_go ~depth ~visited type_expr uid_of_path
      | Tunboxed_tuple exprs ->
        Ts_unboxed_tuple (map_expr_list (List.map snd exprs))
      | Tobject _ | Tnil | Tfield _ ->
        Ts_other (* Objects are currently not supported in the debugger. *)
      | Tlink _ | Tsubst _ ->
        Misc.fatal_error "linking and substitution should not reach this stage."
      | Tvariant _ -> Ts_other (* CR sspies: Support polymorphic variants. *)
      | Tarrow _ -> Ts_other (* CR sspies: Support function types. *)
      | Tunivar { name; _ } -> Ts_var name
      | Tpackage _ -> Ts_other (* CR sspies: Support first-class modules. *)

  let of_type_expr (expr : Types.type_expr) uid_of_path =
    of_type_expr_go ~visited:Numbers.Int.Set.empty ~depth:(-1) expr uid_of_path

  let rec print ppf = function
    | Ts_predef (predef, shapes) ->
      Format.fprintf ppf "Ts_predef %s (%a)" (Predef.to_string predef)
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_constr ((uid, path), shapes) ->
      Format.fprintf ppf "Ts_constr uid=%a path=%a (%a)" Uid.print uid
        Path.print path
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_tuple shapes ->
      Format.fprintf ppf "Ts_tuple (%a)"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_unboxed_tuple shapes ->
      Format.fprintf ppf "Ts_unboxed_tuple (%a)"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_var name ->
      Format.fprintf ppf "Ts_var (%a)"
        (fun ppf opt -> Format.pp_print_option Format.pp_print_string ppf opt)
        name
    | Ts_other -> Format.fprintf ppf "Ts_other"

  (* CR sspies: This function looks very different from a regular substitution.
     It seems to replace an entire type description, and it uses polymorphic
     equality to determine which one. *)
  let rec replace_tvar t ~(pairs : (t * t) list) =
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
      | Ts_var name -> Ts_var name
      | Ts_predef (predef, shape_list) -> Ts_predef (predef, shape_list)
      | Ts_other -> Ts_other)

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = Stdlib.compare

    let print = print

    let hash = Hashtbl.hash

    let equal (x : t) y = x = y

    let output _oc _t = Misc.fatal_error "unimplemented"
  end)
end

module Type_decl_shape = struct
  type 'a complex_constructor =
    { name : string;
      args : 'a complex_constructor_arguments list
    }

  and 'a complex_constructor_arguments =
    { field_name : string option;
      field_value : 'a
    }

  type tds =
    | Tds_variant of
        { simple_constructors : string list;
          complex_constructors : Type_shape.t complex_constructor list
        }
    | Tds_record of (string * Type_shape.t) list
    | Tds_alias of Type_shape.t
    | Tds_other

  type t =
    { path : Path.t;
      definition : tds;
      type_params : Type_shape.t list
    }

  let complex_constructor_map f { name; args } =
    let args =
      List.map
        (fun { field_name; field_value } ->
          { field_name; field_value = f field_value })
        args
    in
    { name; args }

  let get_variant_constructors (cstr_args : Types.constructor_declaration)
      uid_of_path =
    match cstr_args.cd_args with
    | Cstr_tuple list ->
      List.map
        (fun ({ ca_type = type_expr; _ } : Types.constructor_argument) ->
          { field_name = None;
            field_value = Type_shape.of_type_expr type_expr uid_of_path
          })
        list
    | Cstr_record list ->
      List.map
        (fun (lbl : Types.label_declaration) ->
          { field_name = Some (Ident.name lbl.ld_id);
            field_value = Type_shape.of_type_expr lbl.ld_type uid_of_path
          })
        list

  let is_empty_constructor_list (cstr_args : Types.constructor_declaration) =
    let length =
      match cstr_args.cd_args with
      | Cstr_tuple list -> List.length list
      | Cstr_record list -> List.length list
    in
    length = 0

  let of_type_declaration path (type_declaration : Types.type_declaration)
      uid_of_path =
    let definition =
      match type_declaration.type_manifest with
      | Some type_expr ->
        Tds_alias (Type_shape.of_type_expr type_expr uid_of_path)
      | None -> (
        match type_declaration.type_kind with
        | Type_variant (cstr_list, _variant_repr, _unsafe_mode_crossing) ->
          let simple_constructors, complex_constructors =
            List.partition_map
              (fun (cstr : Types.constructor_declaration) ->
                let name = Ident.name cstr.cd_id in
                match is_empty_constructor_list cstr with
                | true -> Left name
                | false ->
                  Right
                    { name; args = get_variant_constructors cstr uid_of_path })
              cstr_list
          in
          Tds_variant { simple_constructors; complex_constructors }
        | Type_record (lbl_list, record_repr, _unsafe_mode_crossing) -> (
          match record_repr with
          | Record_boxed _ ->
            Tds_record
              (List.map
                 (fun (lbl : Types.label_declaration) ->
                   ( Ident.name lbl.ld_id,
                     Type_shape.of_type_expr lbl.ld_type uid_of_path ))
                 lbl_list)
          | Record_float ->
            Tds_record
              (List.map
                 (fun (lbl : Types.label_declaration) ->
                   ( Ident.name lbl.ld_id,
                     Type_shape.Ts_predef (Unboxed Unboxed_float, []) ))
                 lbl_list)
            (* CR sspies: This seems dangerous. We probably want something to mark
               this better. *)
          | Record_mixed _ ->
            (* CR sspies: Mixed records are currently not supported. *)
            Tds_other
          | Record_inlined _ | Record_unboxed | Record_ufloat -> Tds_other)
        | Type_abstract _ -> Tds_other
        | Type_open -> Tds_other
        | Type_record_unboxed_product _ ->
          (* CR sspies: Unboxed products are currently not supported.*)
          Tds_other)
    in
    let type_params =
      List.map
        (fun type_expr -> Type_shape.of_type_expr type_expr uid_of_path)
        type_declaration.type_params
    in
    { path; definition; type_params }

  (* We use custom strings as separators instead of pp_print_space, because the
     latter introduces line breaks that can mess up the tables with all shapes.*)
  let print_sep_string str ppf () = Format.pp_print_string ppf str

  let print_one_entry print_value ppf { field_name; field_value } =
    match field_name with
    | Some name ->
      Format.fprintf ppf "%a=%a" Format.pp_print_string name print_value
        field_value
    | None -> Format.fprintf ppf "%a" print_value field_value

  let print_complex_constructor print_value ppf { name; args } =
    Format.fprintf ppf "(%a of %a)" Format.pp_print_string name
      (Format.pp_print_list ~pp_sep:(print_sep_string " * ")
         (print_one_entry print_value))
      args

  let print_field ppf (name, shape) =
    Format.fprintf ppf "%a: %a" Format.pp_print_string name Type_shape.print
      shape

  let print_tds ppf = function
    | Tds_variant { simple_constructors; complex_constructors } ->
      Format.fprintf ppf
        "Tds_variant simple_constructors=%a complex_constructors=%a"
        (Format.pp_print_list ~pp_sep:(print_sep_string " | ")
           Format.pp_print_string)
        simple_constructors
        (Format.pp_print_list ~pp_sep:(print_sep_string " | ")
           (print_complex_constructor Type_shape.print))
        complex_constructors
    | Tds_record field_list ->
      Format.fprintf ppf "Tds_record { %a }"
        (Format.pp_print_list ~pp_sep:(print_sep_string "; ") print_field)
        field_list
    | Tds_alias type_shape ->
      Format.fprintf ppf "Tds_alias %a" Type_shape.print type_shape
    | Tds_other -> Format.fprintf ppf "Tds_other"

  let print ppf t =
    Format.fprintf ppf "path=%a, definition=(%a)" Path.print t.path print_tds
      t.definition

  let map_snd f list = List.map (fun (fst, snd) -> fst, f snd) list

  (* CR sspies: This function is probably meant to instantiate the polymorphic
     variables in the type declarations. There must be functionality in the
     type checker for this. *)
  let replace_tvar (t : t) (shapes : Type_shape.t list) =
    let debug = false in
    if debug
    then
      Format.eprintf "replacing tvar %a; %a; %a\n%!" print t
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Type_shape.print)
        shapes
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Type_shape.print)
        t.type_params;
    match List.length t.type_params == List.length shapes with
    | true ->
      let replace_tvar =
        Type_shape.replace_tvar ~pairs:(List.combine t.type_params shapes)
      in
      let ret =
        { type_params = [];
          path = t.path;
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
            | Tds_record field_list ->
              Tds_record (map_snd replace_tvar field_list)
            | Tds_alias type_shape -> Tds_alias (replace_tvar type_shape)
            | Tds_other -> Tds_other)
        }
      in
      ret
    | false ->
      (* CR tnowak: investigate *)
      { type_params = []; path = t.path; definition = Tds_other }
end

type binder_shape =
  { type_shape : Type_shape.t;
    type_sort : Jkind_types.Sort.Const.t
  }

let (all_type_decls : Type_decl_shape.t Uid.Tbl.t) = Uid.Tbl.create 16

let (all_type_shapes : binder_shape Uid.Tbl.t) = Uid.Tbl.create 16

let add_to_type_decls path (type_decl : Types.type_declaration) uid_of_path =
  let type_decl_shape =
    Type_decl_shape.of_type_declaration path type_decl uid_of_path
  in
  Uid.Tbl.add all_type_decls type_decl.type_uid type_decl_shape

let add_to_type_shapes var_uid type_expr sort uid_of_path =
  let type_shape = Type_shape.of_type_expr type_expr uid_of_path in
  Uid.Tbl.add all_type_shapes var_uid { type_shape; type_sort = sort }

let tuple_to_string (strings : string list) =
  match strings with
  | [] -> ""
  | hd :: [] -> hd
  | _ :: _ :: _ -> "(" ^ String.concat " * " strings ^ ")"

let unboxed_tuple_to_string (strings : string list) =
  match strings with
  | [] -> ""
  | hd :: [] -> hd
  | _ :: _ :: _ -> "(" ^ String.concat " & " strings ^ ")"

let shapes_to_string (strings : string list) =
  match strings with
  | [] -> ""
  | hd :: [] -> hd ^ " "
  | _ :: _ :: _ -> "(" ^ String.concat ", " strings ^ ") "

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
let find_in_type_decls (type_uid : Uid.t) (type_path : Path.t)
    ~(load_decls_from_cms : string -> Type_decl_shape.t Shape.Uid.Tbl.t) =
  if debug_type_search
  then Format.eprintf "trying to find type_uid = %a\n" Uid.print type_uid;
  if debug_type_search
  then Format.eprintf "obtaining compilation unit of %a\n" Path.print type_path;
  let compilation_unit_type_decls =
    match compilation_unit_from_path type_path with
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

(* CR sspies: This seems to not perform caching. I think [load_decls_from_cms]
   always goes to the file and loads it the original code. *)
let rec type_name (type_shape : Type_shape.t)
    ~(load_decls_from_cms : string -> Type_decl_shape.t Shape.Uid.Tbl.t) =
  match type_shape with
  | Ts_predef (predef, shapes) ->
    shapes_to_string (List.map (type_name ~load_decls_from_cms) shapes)
    ^ Type_shape.Predef.to_string predef
  | Ts_other ->
    if debug_type_search then Format.eprintf "unknown type (Tds_other)\n";
    "unknown"
  | Ts_tuple shapes ->
    tuple_to_string (List.map (type_name ~load_decls_from_cms) shapes)
  | Ts_unboxed_tuple shapes ->
    unboxed_tuple_to_string (List.map (type_name ~load_decls_from_cms) shapes)
  | Ts_var name -> "'" ^ Option.value name ~default:"?"
  | Ts_constr ((type_uid, type_path), shapes) -> (
    match[@warning "-4"]
      find_in_type_decls type_uid type_path ~load_decls_from_cms
    with
    | None ->
      if debug_type_search
      then Format.eprintf "unknown type (declaration not found)\n";
      (* CR sspies: This used to be unknown, perhaps we just want the Path.name? *)
      Format.asprintf "unknown(%s)" (Path.name type_path)
    | Some { definition = Tds_other; _ } ->
      if debug_type_search then Format.eprintf "type has shape Tds_other\n";
      (* CR sspies: This used to be unknown, perhaps we just want the Path.name? *)
      Format.asprintf "unknown(%s)" (Path.name type_path)
    | Some type_decl_shape ->
      (* We have found type instantiation shapes [shapes] and a typing
         declaration shape [type_decl_shape]. *)
      let type_decl_shape =
        Type_decl_shape.replace_tvar type_decl_shape shapes
      in
      let args =
        shapes_to_string (List.map (type_name ~load_decls_from_cms) shapes)
      in
      let name = Path.name type_decl_shape.path in
      args ^ name)

let rec attach_head (path : Path.t) (new_head : Ident.t) =
  match path with
  | Pident ident -> Path.Pdot (Pident new_head, Ident.name ident)
  | Pdot (l, r) -> Path.Pdot (attach_head l new_head, r)
  | Papply (l, r) -> Path.Papply (attach_head l new_head, attach_head r new_head)
  | Pextra_ty (l, extra) -> Path.Pextra_ty (attach_head l new_head, extra)

let attach_compilation_unit_to_path (path : Path.t)
    (compilation_unit : Compilation_unit.t) =
  match compilation_unit_from_path path with
  | None ->
    attach_head path
      (Compilation_unit.to_global_ident_for_bytecode compilation_unit)
  | Some _ -> path

let map_snd f list = List.map (fun (a, b) -> a, f b) list

(* CR sspies: I don't know what this function does. *)
let attach_compilation_unit_to_paths (type_decl : Type_decl_shape.t)
    ~(compilation_unit : Compilation_unit.t) =
  let[@warning "-4"] attach_to_shape = function
    | Type_shape.Ts_constr ((uid, path), ts) ->
      Type_shape.Ts_constr
        ((uid, attach_compilation_unit_to_path path compilation_unit), ts)
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
                (Type_decl_shape.complex_constructor_map attach_to_shape)
                complex_constructors
          }
      | Tds_record list -> Tds_record (map_snd attach_to_shape list)
      | Tds_alias shape -> Tds_alias (attach_to_shape shape)
      | Tds_other -> Tds_other)
  }

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
          Format.asprintf "%a" Type_decl_shape.print v ))
      entries
  in
  let uids, decls = List.split entries in
  print_table ppf ["UID", uids; "Type Declaration", decls]

let print_table_all_type_shapes ppf =
  let entries = Uid.Tbl.to_list all_type_shapes in
  let entries = List.sort (fun (a, _) (b, _) -> Uid.compare a b) entries in
  let entries =
    List.map
      (fun (k, { type_shape; type_sort }) ->
        ( Format.asprintf "%a" Uid.print k,
          ( Format.asprintf "%a" Type_shape.print type_shape,
            Format.asprintf "%a" Jkind_types.Sort.Const.format type_sort ) ))
      entries
  in
  let uids, rest = List.split entries in
  let types, sorts = List.split rest in
  print_table ppf ["UID", uids; "Type", types; "Sort", sorts]
