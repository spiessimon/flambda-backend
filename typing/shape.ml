(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Ulysse Gérard, Thomas Refis, Tarides                    *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Layout = Jkind_types.Sort.Const
type base_layout = Jkind_types.Sort.base

module Uid = struct
  type t =
    | Compilation_unit of string
    | Item of { comp_unit: string; id: int; from: Unit_info.intf_or_impl }
    | Internal
    | Predef of string
    | Unboxed_version of t

  include Identifiable.Make(struct
    type nonrec t = t

    let rec compare (x : t) y =
      match x, y with
      | Compilation_unit s1, Compilation_unit s2 -> String.compare s1 s2
      | Item c1, Item c2 ->
        let c = Int.compare c1.id c2.id in
        let c =
          if c <> 0 then c else String.compare c1.comp_unit c2.comp_unit
        in
        if c <> 0 then c else Stdlib.compare c1.from c2.from
      | Internal, Internal -> 0
      | Predef s1, Predef s2 -> String.compare s1 s2
      | Unboxed_version t1, Unboxed_version t2 -> compare t1 t2
      | Compilation_unit _,
        (Item _ | Internal | Predef _ | Unboxed_version _) ->
        -1
      | Item _, (Internal | Predef _| Unboxed_version _) -> -1
      | Internal, (Predef _ | Unboxed_version _) -> -1
      | Predef _, Unboxed_version _ -> -1
      | (Item _ | Internal | Predef _ | Unboxed_version _),
        Compilation_unit _ ->
        1
      | (Internal | Predef _ | Unboxed_version _), Item _ -> 1
      | (Predef _ | Unboxed_version _), Internal -> 1
      | Unboxed_version _, Predef _ -> 1

    let equal x y = compare x y = 0

    let hash (x : t) = Hashtbl.hash x

    let pp_intf_or_impl fmt = function
      | Unit_info.Intf -> Format.pp_print_string fmt "[intf]"
      | Unit_info.Impl -> ()

    let rec print fmt = function
      | Internal -> Format.pp_print_string fmt "<internal>"
      | Predef name -> Format.fprintf fmt "<predef:%s>" name
      | Compilation_unit s -> Format.pp_print_string fmt s
      | Item { comp_unit; id; from } ->
          Format.fprintf fmt "%a%s.%d" pp_intf_or_impl from comp_unit id
      | Unboxed_version t -> Format.fprintf fmt "%a#" print t

    let output oc t =
      let fmt = Format.formatter_of_out_channel oc in
      print fmt t
  end)

  let id = ref (-1)

  let reinit () = id := (-1)

  let mk  ~current_unit =
      let comp_unit, from =
        let open Unit_info in
        match current_unit with
        | None -> "", Impl
        | Some ui ->
          Compilation_unit.full_path_as_string (modname ui), kind ui
      in
      incr id;
      Item { comp_unit; id = !id; from }

  let of_compilation_unit_id id =
    Compilation_unit (id |> Compilation_unit.full_path_as_string)

  let of_compilation_unit_name name =
    Compilation_unit (name |> Compilation_unit.Name.to_string)

  let of_predef_id id =
    if not (Ident.is_predef id) then
      Misc.fatal_errorf "Types.Uid.of_predef_id %S" (Ident.name id);
    Predef (Ident.name id)

  let internal_not_actually_unique = Internal

  let unboxed_version t =
    match t with
    | Unboxed_version _ ->
      Misc.fatal_error "Shape.unboxed_version"
    | _ -> Unboxed_version t

  let for_actual_declaration = function
    | Item _ -> true
    | _ -> false
end

module Sig_component_kind = struct
  type t =
    | Value
    | Type
    | Constructor
    | Label
    | Unboxed_label
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  let to_string = function
    | Value -> "value"
    | Type -> "type"
    | Constructor -> "constructor"
    | Label -> "label"
    | Unboxed_label -> "unboxed label"
    | Module -> "module"
    | Module_type -> "module type"
    | Extension_constructor -> "extension constructor"
    | Class -> "class"
    | Class_type -> "class type"

  let can_appear_in_types = function
    | Value
    | Extension_constructor ->
        false
    | Type
    | Constructor
    | Label
    | Unboxed_label
    | Module
    | Module_type
    | Class
    | Class_type ->
        true

  let rank = function
    | Value -> 0
    | Type -> 1
    | Module -> 2
    | Module_type -> 3
    | Extension_constructor -> 4
    | Class -> 5
    | Class_type -> 6
    | Constructor -> 7
    | Label -> 8
    | Unboxed_label -> 9

  let compare a b =
    let a = rank a in
    let b = rank b in
    Int.compare a b
end

module Item = struct
  module T = struct
    type t = string * Sig_component_kind.t

    let compare (sa, ka) (sb, kb) =
      let c = String.compare sa sb in
      if c <> 0 then c
      else (Sig_component_kind.compare ka kb)

    let name (name, _) = name
    let kind (_, kind) = kind

    let make str ns = str, ns

    let value id = Ident.name id, Sig_component_kind.Value
    let type_ id = Ident.name id, Sig_component_kind.Type
    let constr id = Ident.name id, Sig_component_kind.Constructor
    let label id = Ident.name id, Sig_component_kind.Label
    let unboxed_label id = Ident.name id, Sig_component_kind.Unboxed_label
    let module_ id = Ident.name id, Sig_component_kind.Module
    let module_type id = Ident.name id, Sig_component_kind.Module_type
    let extension_constructor id =
      Ident.name id, Sig_component_kind.Extension_constructor
    let class_ id =
      Ident.name id, Sig_component_kind.Class
    let class_type id =
      Ident.name id, Sig_component_kind.Class_type

    let print fmt (name, ns) =
      Format.fprintf fmt "%S[%s]"
        name
        (Sig_component_kind.to_string ns)

    let hash x = Hashtbl.hash x
  end

  include T

  module Map = Map.Make(T)
end

module Predef = struct
  type simd_vec_split =
    (* 128 bit *)
    | Int8x16
    | Int16x8
    | Int32x4
    | Int64x2
    | Float32x4
    | Float64x2
    (* 256 bit *)
    | Int8x32
    | Int16x16
    | Int32x8
    | Int64x4
    | Float32x8
    | Float64x4
    (* 512 bit *)
    | Int8x64
    | Int16x32
    | Int32x16
    | Int64x8
    | Float32x16
    | Float64x8

  type unboxed =
    | Unboxed_float
    | Unboxed_float32
    | Unboxed_nativeint
    | Unboxed_int64
    | Unboxed_int32
    | Unboxed_simd of simd_vec_split

  type t =
    | Array
    | Bytes
    | Char
    | Extension_constructor
    | Float
    | Float32
    | Floatarray
    | Int
    | Int32
    | Int64
    | Lazy_t
    | Nativeint
    | String
    | Simd of simd_vec_split
    | Exception
    | Unboxed of unboxed

  let simd_vec_split_to_string : simd_vec_split -> string = function
    | Int8x16 -> "int8x16"
    | Int16x8 -> "int16x8"
    | Int32x4 -> "int32x4"
    | Int64x2 -> "int64x2"
    | Float32x4 -> "float32x4"
    | Float64x2 -> "float64x2"
    | Int8x32 -> "int8x32"
    | Int16x16 -> "int16x16"
    | Int32x8 -> "int32x8"
    | Int64x4 -> "int64x4"
    | Float32x8 -> "float32x8"
    | Float64x4 -> "float64x4"
    | Int8x64 -> "int8x64"
    | Int16x32 -> "int16x32"
    | Int32x16 -> "int32x16"
    | Int64x8 -> "int64x8"
    | Float32x16 -> "float32x16"
    | Float64x8 -> "float64x8"

  (* name of the type without the hash *)
  let unboxed_to_string (u : unboxed) =
    match u with
    | Unboxed_float -> "float"
    | Unboxed_float32 -> "float32"
    | Unboxed_nativeint -> "nativeint"
    | Unboxed_int64 -> "int64"
    | Unboxed_int32 -> "int32"
    | Unboxed_simd s -> simd_vec_split_to_string s

  let to_string : t -> string = function
    | Array -> "array"
    | Bytes -> "bytes"
    | Char -> "char"
    | Extension_constructor -> "extension_constructor"
    | Float -> "float"
    | Float32 -> "float32"
    | Floatarray -> "floatarray"
    | Int -> "int"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Lazy_t -> "lazy_t"
    | Nativeint -> "nativeint"
    | String -> "string"
    | Simd s -> simd_vec_split_to_string s
    | Exception -> "exn"
    | Unboxed u -> unboxed_to_string u ^ "#"

  let simd_vec_split_of_string : string -> simd_vec_split option = function
    | "int8x16#" -> Some Int8x16
    | "int16x8#" -> Some Int16x8
    | "int32x4#" -> Some Int32x4
    | "int64x2#" -> Some Int64x2
    | "float32x4#" -> Some Float32x4
    | "float64x2#" -> Some Float64x2
    | "int8x32#" -> Some Int8x32
    | "int16x16#" -> Some Int16x16
    | "int32x8#" -> Some Int32x8
    | "int64x4#" -> Some Int64x4
    | "float32x8#" -> Some Float32x8
    | "float64x4#" -> Some Float64x4
    | "int8x64#" -> Some Int8x64
    | "int16x32#" -> Some Int16x32
    | "int32x16#" -> Some Int32x16
    | "int64x8#" -> Some Int64x8
    | "float32x16#" -> Some Float32x16
    | "float64x8#" -> Some Float64x8
    | _ -> None

  let unboxed_of_string = function
    | "float#" -> Some Unboxed_float
    | "float32#" -> Some Unboxed_float32
    | "nativeint#" -> Some Unboxed_nativeint
    | "int64#" -> Some Unboxed_int64
    | "int32#" -> Some Unboxed_int32
    | s -> Option.map (fun s -> Unboxed_simd s) (simd_vec_split_of_string s)

  let simd_base_type_of_string = function
    | "int8x16" -> Some Int8x16
    | "int16x8" -> Some Int16x8
    | "int32x4" -> Some Int32x4
    | "int64x2" -> Some Int64x2
    | "float32x4" -> Some Float32x4
    | "float64x2" -> Some Float64x2
    | "int8x32" -> Some Int8x32
    | "int16x16" -> Some Int16x16
    | "int32x8" -> Some Int32x8
    | "int64x4" -> Some Int64x4
    | "float32x8" -> Some Float32x8
    | "float64x4" -> Some Float64x4
    | "int8x64" -> Some Int8x64
    | "int16x32" -> Some Int16x32
    | "int32x16" -> Some Int32x16
    | "int64x8" -> Some Int64x8
    | "float32x16" -> Some Float32x16
    | "float64x8" -> Some Float64x8
    | _ -> None

  let of_string : string -> t option = function
    | "array" -> Some Array
    | "bytes" -> Some Bytes
    | "char" -> Some Char
    | "extension_constructor" -> Some Extension_constructor
    | "float" -> Some Float
    | "float32" -> Some Float32
    | "floatarray" -> Some Floatarray
    | "int" -> Some Int
    | "int32" -> Some Int32
    | "int64" -> Some Int64
    | "lazy_t" -> Some Lazy_t
    | "nativeint" -> Some Nativeint
    | "string" -> Some String
    | "exn" -> Some Exception
    | s -> (
      match simd_base_type_of_string s with
      | Some b -> Some (Simd b)
      | None -> (
        match unboxed_of_string s with
        | Some u -> Some (Unboxed u)
        | None -> None))

  let simd_vec_split_to_layout (b : simd_vec_split) : Jkind_types.Sort.base =
    match b with
    | Int8x16 -> Vec128
    | Int16x8 -> Vec128
    | Int32x4 -> Vec128
    | Int64x2 -> Vec128
    | Float32x4 -> Vec128
    | Float64x2 -> Vec128
    | Int8x32 -> Vec256
    | Int16x16 -> Vec256
    | Int32x8 -> Vec256
    | Int64x4 -> Vec256
    | Float32x8 -> Vec256
    | Float64x4 -> Vec256
    | Int8x64 -> Vec512
    | Int16x32 -> Vec512
    | Int32x16 -> Vec512
    | Int64x8 -> Vec512
    | Float32x16 -> Vec512
    | Float64x8 -> Vec512

  let unboxed_type_to_base_layout (b : unboxed) : base_layout =
    match b with
    | Unboxed_float -> Float64
    | Unboxed_float32 -> Float32
    | Unboxed_nativeint -> Word
    | Unboxed_int64 -> Bits64
    | Unboxed_int32 -> Bits32
    | Unboxed_simd s -> simd_vec_split_to_layout s

  let predef_to_layout : t -> Layout.t = function
    | Array -> Layout.Base Value
    | Bytes -> Layout.Base Value
    | Char -> Layout.Base Value
    | Extension_constructor -> Layout.Base Value
    | Float -> Layout.Base Value
    | Float32 -> Layout.Base Value
    | Floatarray -> Layout.Base Value
    | Int -> Layout.Base Value
    | Int32 -> Layout.Base Value
    | Int64 -> Layout.Base Value
    | Lazy_t -> Layout.Base Value
    | Nativeint -> Layout.Base Value
    | String -> Layout.Base Value
    | Simd _ -> Layout.Base Value
    | Exception -> Layout.Base Value
    | Unboxed u -> Layout.Base (unboxed_type_to_base_layout u)


  let equal_simd_vec_split = fun s1 s2 ->
    match s1, s2 with
    | Int8x16, Int8x16 -> true
    | Int16x8, Int16x8 -> true
    | Int32x4, Int32x4 -> true
    | Int64x2, Int64x2 -> true
    | Float32x4, Float32x4 -> true
    | Float64x2, Float64x2 -> true
    | Int8x32, Int8x32 -> true
    | Int16x16, Int16x16 -> true
    | Int32x8, Int32x8 -> true
    | Int64x4, Int64x4 -> true
    | Float32x8, Float32x8 -> true
    | Float64x4, Float64x4 -> true
    | Int8x64, Int8x64 -> true
    | Int16x32, Int16x32 -> true
    | Int32x16, Int32x16 -> true
    | Int64x8, Int64x8 -> true
    | Float32x16, Float32x16 -> true
    | Float64x8, Float64x8 -> true
    | _, _ -> false


  let equal_unboxed = fun u1 u2 ->
    match u1, u2 with
    | Unboxed_float, Unboxed_float -> true
    | Unboxed_float32, Unboxed_float32 -> true
    | Unboxed_nativeint, Unboxed_nativeint -> true
    | Unboxed_int64, Unboxed_int64 -> true
    | Unboxed_int32, Unboxed_int32 -> true
    | Unboxed_simd s1, Unboxed_simd s2 -> equal_simd_vec_split s1 s2
    | _, _ -> false

  let equal p1 p2 =
    match p1, p2 with
    | Array, Array -> true
    | Bytes, Bytes -> true
    | Char, Char -> true
    | Extension_constructor, Extension_constructor -> true
    | Float, Float -> true
    | Float32, Float32 -> true
    | Floatarray, Floatarray -> true
    | Int, Int -> true
    | Int32, Int32 -> true
    | Int64, Int64 -> true
    | Lazy_t, Lazy_t -> true
    | Nativeint, Nativeint -> true
    | String, String -> true
    | Simd s1, Simd s2 -> equal_simd_vec_split s1 s2
    | Exception, Exception -> true
    | Unboxed u1, Unboxed u2 -> equal_unboxed u1 u2
    | _, _ -> false

end





type var = Ident.t
type t = { hash:int; uid: Uid.t option; desc: desc; approximated: bool }
and desc =
  | Var of var
  | Abs of var * t
  | App of t * t
  | Struct of t Item.Map.t
  | Alias of t
  | Leaf
  | Type_decl of tds
  | Proj of t * Item.t
  | Comp_unit of string
  | Error of string

and without_layout = Layout_to_be_determined

and 'a ts =
  | Ts_constr of (t * 'a) * without_layout ts list
  | Ts_tuple of 'a ts list
  | Ts_unboxed_tuple of 'a ts list
  | Ts_var of string option * 'a
  | Ts_predef of Predef.t * without_layout ts list
  | Ts_arrow of without_layout ts * without_layout ts
  | Ts_variant of 'a ts poly_variant_constructors
  | Ts_other of 'a

and 'a poly_variant_constructors = 'a poly_variant_constructor list

and 'a poly_variant_constructor =
  { pv_constr_name : string;
    pv_constr_args : 'a list
  }


and tds_desc =
  | Tds_variant of
      { simple_constructors : string list;
        complex_constructors :
          (without_layout ts * Layout.t)
          complex_constructors
      }
  | Tds_variant_unboxed of
      { name : string;
        arg_name : string option;
            (** if this is [None], we are looking at a singleton tuple;
            otherwise, it is a singleton record. *)
        arg_shape : without_layout ts;
        arg_layout : Layout.t
      }
      (** An unboxed variant corresponds to the [@@unboxed] annotation.
      It must have a single, complex constructor. *)
  | Tds_record of
      { fields :
          (string * without_layout ts * Layout.t) list;
        kind : record_kind
      }
  | Tds_alias of without_layout ts
  | Tds_other

and record_kind =
  | Record_unboxed
  | Record_unboxed_product
  | Record_boxed
  | Record_mixed of mixed_product_shape
  | Record_floats

and 'a complex_constructors = 'a complex_constructor list

and 'a complex_constructor =
  { name : string;
    kind : constructor_representation;
    args : 'a complex_constructor_arguments list
  }

and 'a complex_constructor_arguments =
  { field_name : string option;
    field_value : 'a
  }

and constructor_representation =
| Constructor_uniform_value
| Constructor_mixed of mixed_product_shape

and mixed_product_shape = base_layout array

and tds =
  {
    definition : tds_desc;
    type_params : without_layout ts list
  }

let rec equal_desc d1 d2 =
  if d1 == d2 then true else
  match d1, d2 with
  | Var v1, Var v2 -> Ident.equal v1 v2
  | Alias a1, Alias a2 -> equal a1 a2
  | Error s1, Error s2 -> String.equal s1 s2
  | Abs (v1, t1), Abs (v2, t2) ->
    if Ident.equal v1 v2 then equal t1 t2
    else false
  | App (v1, t1), App (v2, t2) ->
    if not (equal t1 t2) then false
    else equal v1 v2
  | Leaf, Leaf -> true
  | Type_decl tds1, Type_decl tds2 -> equal_tds tds1 tds2
  | Struct t1, Struct t2 ->
    Item.Map.equal equal t1 t2
  | Proj (t1, i1), Proj (t2, i2) ->
    if Item.compare i1 i2 <> 0 then false
    else equal t1 t2
  | Comp_unit c1, Comp_unit c2 -> String.equal c1 c2
  | Var _, (Abs _ | App _ | Struct _ | Leaf  | Type_decl _ | Proj _ | Comp_unit _ | Alias _ | Error _)
  | Abs _, (Var _ | App _ | Struct _ | Leaf  | Type_decl _ | Proj _ | Comp_unit _ | Alias _ | Error _)
  | App _, (Var _ | Abs _ | Struct _ | Leaf  | Type_decl _ | Proj _ | Comp_unit _ | Alias _ | Error _)
  | Struct _, (Var _ | Abs _ | App _ | Leaf  | Type_decl _ | Proj _ | Comp_unit _ | Alias _ | Error _)
  | Leaf, (Var _ | Abs _ | App _ | Struct _ | Proj _ | Type_decl _ | Comp_unit _ | Alias _ | Error _)
  | Type_decl _, (Var _ | Abs _ | App _ | Struct _ | Leaf  | Proj _ | Comp_unit _ | Alias _ | Error _)
  | Proj _, (Var _ | Abs _ | App _ | Struct _ | Leaf  | Type_decl _ | Comp_unit _ | Alias _ | Error _)
  | Comp_unit _, (Var _ | Abs _ | App _ | Struct _ | Leaf  | Type_decl _ | Proj _ | Alias _ | Error _)
  | Alias _, (Var _ | Abs _ | App _ | Struct _ | Leaf  | Type_decl _ | Proj _ | Comp_unit _ | Error _)
  | Error _, (Var _ | Abs _ | App _ | Struct _ | Leaf  | Type_decl _ | Proj _ | Comp_unit _ | Alias _)
    -> false

and equal t1 t2 =
  if t1.hash <> t2.hash then false
  else if not (Bool.equal t1.approximated t2.approximated) then false
  else if not (Option.equal Uid.equal t1.uid t2.uid) then false
  else equal_desc t1.desc t2.desc

and equal_tds t1 t2 =
  equal_tds_desc t1.definition t2.definition &&
  List.equal (equal_ts equal_without_layout) t1.type_params t2.type_params

and equal_tds_desc d1 d2 =
  if d1 == d2 then true else
  match d1, d2 with
  | Tds_alias t1, Tds_alias t2 -> equal_ts equal_without_layout t1 t2
  | Tds_record {fields = fields1; kind = kind1},
    Tds_record {fields = fields2; kind = kind2} ->
    equal_record_kind kind1 kind2 &&
    List.equal equal_field fields1 fields2
  | Tds_variant { simple_constructors = simple_constructors1;
                  complex_constructors = complex_constructors1 },
    Tds_variant { simple_constructors = simple_constructors2;
                  complex_constructors = complex_constructors2 } ->
    List.equal
      equal_simple_constructor
      simple_constructors1
      simple_constructors2
    && List.equal
      (equal_complex_constructor (fun (sh1, ly1) (sh2, ly2) ->
          equal_ts equal_without_layout sh1 sh2 && Layout.equal ly1 ly2))
      complex_constructors1
      complex_constructors2
  | Tds_variant_unboxed { name = name1; arg_name = arg_name1;
                          arg_shape = arg_shape1; arg_layout = arg_layout1 },
    Tds_variant_unboxed { name = name2; arg_name = arg_name2;
                          arg_shape = arg_shape2; arg_layout = arg_layout2 } ->
    String.equal name1 name2 &&
    Option.equal String.equal arg_name1 arg_name2 &&
    equal_ts equal_without_layout arg_shape1 arg_shape2 &&
    Layout.equal arg_layout1 arg_layout2
  | Tds_other, Tds_other -> true
  | Tds_alias _,
    (Tds_record _ | Tds_variant _ | Tds_variant_unboxed _ | Tds_other)
  | Tds_record _,
    (Tds_alias _ | Tds_variant _ | Tds_variant_unboxed _ | Tds_other)
  | Tds_variant _,
    (Tds_alias _ | Tds_record _ | Tds_variant_unboxed _ | Tds_other)
  | Tds_variant_unboxed _,
    (Tds_alias _ | Tds_record _ | Tds_variant _ | Tds_other)
  | Tds_other,
    (Tds_alias _ | Tds_record _ | Tds_variant _ | Tds_variant_unboxed _)
    -> false

and equal_record_kind k1 k2 =
  match k1, k2 with
  | Record_unboxed, Record_unboxed -> true
  | Record_unboxed_product, Record_unboxed_product -> true
  | Record_boxed, Record_boxed -> true
  | Record_mixed lys1, Record_mixed lys2 ->
    Misc.Stdlib.Array.equal (Jkind_types.Sort.equal_base) lys1 lys2
  | Record_floats, Record_floats -> true
  | Record_unboxed,
    (Record_unboxed_product | Record_boxed | Record_mixed _ | Record_floats)
  | Record_unboxed_product,
    (Record_unboxed | Record_boxed | Record_mixed _ | Record_floats)
  | Record_boxed,
    (Record_unboxed | Record_unboxed_product | Record_mixed _ | Record_floats)
  | Record_mixed _,
    (Record_unboxed | Record_unboxed_product | Record_boxed | Record_floats)
  | Record_floats,
    (Record_unboxed | Record_unboxed_product | Record_boxed | Record_mixed _)
    -> false

and equal_field (s1, sh1, ly1) (s2, sh2, ly2) =
  String.equal s1 s2 &&
  equal_ts equal_without_layout sh1 sh2 &&
  Layout.equal ly1 ly2

and equal_simple_constructor c1 c2 = String.equal c1 c2

and equal_complex_constructor eq
  { name = name1; kind = kind1; args = args1 }
  { name = name2; kind = kind2; args = args2 } =
  String.equal name1 name2 &&
  equal_constructor_representation kind1 kind2 &&
  List.equal (equal_complex_constructor_arguments eq) args1 args2

and equal_complex_constructor_arguments eq
  { field_name = field_name1; field_value = field_value1 }
  { field_name = field_name2; field_value = field_value2 } =
  Option.equal String.equal field_name1 field_name2 &&
  eq field_value1 field_value2

and equal_constructor_representation k1 k2 =
  match k1, k2 with
  | Constructor_uniform_value, Constructor_uniform_value -> true
  | Constructor_mixed lys1, Constructor_mixed lys2 ->
    Misc.Stdlib.Array.equal (Jkind_types.Sort.equal_base) lys1 lys2
  | Constructor_uniform_value, (Constructor_mixed _ )
  | Constructor_mixed _, (Constructor_uniform_value )
    -> false

and equal_without_layout =
  function Layout_to_be_determined ->
  function Layout_to_be_determined -> true

and equal_poly_variant_constructor eq
  { pv_constr_name = name1; pv_constr_args = args1 }
  { pv_constr_name = name2; pv_constr_args = args2 } =
  String.equal name1 name2 &&
  List.equal (equal_ts eq) args1 args2

and equal_ts :
  'a. ('a -> 'a -> bool) -> 'a ts -> 'a ts -> bool =
  fun eq t1 t2 ->
  match t1, t2 with
  | Ts_constr ((sh1, ly1), ts1), Ts_constr ((sh2, ly2), ts2) ->
    equal sh1 sh2 &&
    eq ly1 ly2 &&
    List.equal (equal_ts equal_without_layout) ts1 ts2
  | Ts_tuple ts1, Ts_tuple ts2 ->
    List.equal (equal_ts eq) ts1 ts2
  | Ts_unboxed_tuple ts1, Ts_unboxed_tuple ts2 ->
    List.equal (equal_ts eq) ts1 ts2
  | Ts_var (name1, ly1), Ts_var (name2, ly2) ->
    Option.equal String.equal name1 name2 &&
    eq ly1 ly2
  | Ts_predef (predef1, shapes1), Ts_predef (predef2, shapes2) ->
    Predef.equal predef1 predef2 &&
    List.equal (equal_ts equal_without_layout) shapes1 shapes2
  | Ts_arrow (arg1, ret1), Ts_arrow (arg2, ret2) ->
    equal_ts equal_without_layout arg1 arg2 &&
    equal_ts equal_without_layout ret1 ret2
  | Ts_variant (cstrs1), Ts_variant (cstrs2) ->
    List.equal (equal_poly_variant_constructor eq) cstrs1 cstrs2
  | Ts_other ly1, Ts_other ly2 -> eq ly1 ly2
  | Ts_constr _,
      (Ts_other _ | Ts_predef _ | Ts_arrow _ | Ts_variant _ | Ts_tuple _
       | Ts_unboxed_tuple _ | Ts_var _)
  | Ts_other _,
      (Ts_constr _ | Ts_predef _ | Ts_arrow _ | Ts_variant _ | Ts_tuple _
       | Ts_unboxed_tuple _ | Ts_var _)
  | Ts_predef _ ,
      (Ts_constr _ | Ts_other _ | Ts_arrow _ | Ts_variant _ | Ts_tuple _
       | Ts_unboxed_tuple _ | Ts_var _)
  | Ts_arrow _ ,
      (Ts_constr _ | Ts_predef _ | Ts_other _ | Ts_variant _ | Ts_tuple _
       | Ts_unboxed_tuple _ | Ts_var _)
  | Ts_variant _ ,
      (Ts_constr _ | Ts_predef _ | Ts_arrow _ | Ts_other _  | Ts_tuple _
       | Ts_unboxed_tuple _ | Ts_var _)
  | Ts_tuple _ ,
      (Ts_constr _ | Ts_predef _ | Ts_arrow _ | Ts_variant _ | Ts_other _
       | Ts_unboxed_tuple _ | Ts_var _)
  | Ts_unboxed_tuple _ ,
      (Ts_constr _ | Ts_predef _ | Ts_arrow _ | Ts_variant _ | Ts_other _
       | Ts_tuple _ | Ts_var _)
  | Ts_var _ ,
      (Ts_constr _ | Ts_predef _ | Ts_arrow _ | Ts_variant _ | Ts_other _
       | Ts_tuple _ | Ts_unboxed_tuple _)
    -> false



let rec print fmt t =
  let print_uid_opt =
    Format.pp_print_option (fun fmt -> Format.fprintf fmt "<%a>" Uid.print)
  in
  let rec aux fmt { uid; desc; hash = _ } =
    match desc with
    | Var id ->
        Format.fprintf fmt "%s%a" (Ident.name id) print_uid_opt uid
    | Abs (id, t) ->
        let rec collect_idents = function
          | { uid = None; desc = Abs(id, t) } ->
            let (ids, body) = collect_idents t in
            id :: ids, body
          | body ->
            ([], body)
        in
        let (other_idents, body) = collect_idents t in
        let pp_idents fmt idents =
          let idents_names = List.map Ident.name idents in
          let pp_sep fmt () = Format.fprintf fmt ",@ " in
          Format.pp_print_list ~pp_sep Format.pp_print_string fmt idents_names
        in
        Format.fprintf fmt "Abs@[%a@,(@[%a,@ @[%a@]@])@]"
          print_uid_opt uid pp_idents (id :: other_idents) aux body
    | App (t1, t2) ->
        Format.fprintf fmt "@[%a(@,%a)%a@]" aux t1 aux t2
          print_uid_opt uid
    | Leaf ->
        Format.fprintf fmt "<%a>" (Format.pp_print_option Uid.print) uid
    | Type_decl decl ->
      Format.fprintf fmt "<%a> = %a"
        (Format.pp_print_option Uid.print) uid
        print_type_decl_shape decl
    | Proj (t, item) ->
        begin match uid with
        | None ->
            Format.fprintf fmt "@[%a@ .@ %a@]"
              aux t
              Item.print item
        | Some uid ->
            Format.fprintf fmt "@[(%a@ .@ %a)<%a>@]"
              aux t
              Item.print item
              Uid.print uid
        end
    | Comp_unit name -> Format.fprintf fmt "CU %s" name
    | Struct map ->
        let print_map fmt =
          Item.Map.iter (fun item t ->
              Format.fprintf fmt "@[<hv 2>%a ->@ %a;@]@,"
                Item.print item
                aux t
            )
        in
        if Item.Map.is_empty map then
          Format.fprintf fmt "@[<hv>{%a}@]" print_uid_opt uid
        else
          Format.fprintf fmt "{@[<v>%a@,%a@]}" print_uid_opt uid print_map map
    | Alias t ->
        Format.fprintf fmt "Alias@[(@[<v>%a@,%a@])@]" print_uid_opt uid aux t
    | Error s ->
        Format.fprintf fmt "Error %s" s
  in
  if t.approximated then
    Format.fprintf fmt "@[(approx)@ %a@]@;" aux t
  else
    Format.fprintf fmt "@[%a@]@;" aux t

(* printing type shapes *)
and print_type_shape : type a. Format.formatter -> a ts -> unit =
  fun ppf -> function
  | Ts_predef (predef, shapes) ->
    Format.fprintf ppf "Ts_predef %s (%a)" (Predef.to_string predef)
      (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
          print_type_shape)
      shapes
  | Ts_constr ((shape, _), shapes) ->
    Format.fprintf ppf "Ts_constr shape=%a (%a)"
    print shape
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
        print_type_shape)
      shapes
  | Ts_tuple shapes ->
    Format.fprintf ppf "Ts_tuple (%a)"
      (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
          print_type_shape)
      shapes
  | Ts_unboxed_tuple shapes ->
    Format.fprintf ppf "Ts_unboxed_tuple (%a)"
      (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
          print_type_shape)
      shapes
  | Ts_var (name, _) ->
    Format.fprintf ppf "Ts_var (%a)"
      (fun ppf opt -> Format.pp_print_option Format.pp_print_string ppf opt)
      name
  | Ts_arrow (arg, ret) ->
    Format.fprintf ppf "Ts_arrow (%a, %a)" print_type_shape arg print_type_shape ret
  | Ts_variant fields ->
    Format.fprintf ppf "Ts_variant (%a)"
      (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
          (fun ppf { pv_constr_name; pv_constr_args } ->
            Format.fprintf ppf "%s (%a)" pv_constr_name
              (Format.pp_print_list
                ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
                print_type_shape)
              pv_constr_args))
      fields
  | Ts_other _ -> Format.fprintf ppf "Ts_other"

(* printing type declaration shapes *)

(* We use custom strings as separators instead of pp_print_space, because the
    latter introduces line breaks that can mess up the tables with all shapes.*)
and print_sep_string str ppf () = Format.pp_print_string ppf str

and print_one_entry print_value ppf { field_name; field_value } =
  match field_name with
  | Some name ->
    Format.fprintf ppf "%a=%a" Format.pp_print_string name print_value
      field_value
  | None -> Format.fprintf ppf "%a" print_value field_value

and print_complex_constructor print_value ppf { name; kind = _; args } =
  Format.fprintf ppf "(%a of %a)" Format.pp_print_string name
    (Format.pp_print_list ~pp_sep:(print_sep_string " * ")
        (print_one_entry print_value))
    args

and print_only_shape ppf (shape, _) = print_type_shape ppf shape

and print_field ppf
    ((name, shape, _) : _ * without_layout ts * _) =
  Format.fprintf ppf "%a: %a" Format.pp_print_string name print_type_shape
    shape

and print_record_type = function
  | Record_boxed -> "_boxed"
  | Record_floats -> "_floats"
  | Record_mixed _ -> "_mixed"
  | Record_unboxed -> " [@@unboxed]"
  | Record_unboxed_product -> "_unboxed_product"

and print_tds_desc ppf = function
  | Tds_variant { simple_constructors; complex_constructors } ->
    Format.fprintf ppf
      "Tds_variant simple_constructors=%a complex_constructors=%a"
      (Format.pp_print_list ~pp_sep:(print_sep_string " | ")
          Format.pp_print_string)
      simple_constructors
      (Format.pp_print_list ~pp_sep:(print_sep_string " | ")
          (print_complex_constructor print_only_shape))
      complex_constructors
  | Tds_variant_unboxed { name; arg_name; arg_shape; arg_layout } ->
    Format.fprintf ppf
      "Tds_variant_unboxed name=%s arg_name=%s arg_shape=%a arg_layout=%a"
      name
      (Option.value ~default:"None" arg_name)
      print_type_shape arg_shape Layout.format arg_layout
  | Tds_record { fields; kind } ->
    Format.fprintf ppf "Tds_record%s { %a }" (print_record_type kind)
      (Format.pp_print_list ~pp_sep:(print_sep_string "; ") print_field)
      fields
  | Tds_alias type_shape ->
    Format.fprintf ppf "Tds_alias %a" print_type_shape type_shape
  | Tds_other -> Format.fprintf ppf "Tds_other"

and print_type_decl_shape ppf t =
  print_tds_desc ppf t.definition

let rec strip_head_aliases = function
  | { desc = Alias t; _ } -> strip_head_aliases t
  | t -> t

let hash_var = 1
let hash_abs = 2
let hash_struct = 3
let hash_leaf = 4
let hash_proj = 5
let hash_app = 6
let hash_comp_unit = 7
let hash_alias = 8
let hash_error = 9

let fresh_var ?(name="shape-var") uid =
  let var = Ident.create_local name in
  var, { uid = Some uid; desc = Var var;
         hash = Hashtbl.hash (hash_var, uid, var);
         approximated = false }

let for_unnamed_functor_param = Ident.create_local "()"

let var uid id =
  { uid = Some uid; desc = Var id;
    hash = Hashtbl.hash (hash_var, uid, id);
    approximated = false }

let abs ?uid var body =
  { uid; desc = Abs (var, body);
    hash = Hashtbl.hash (hash_abs, uid, body.hash);
    approximated = false }

let str ?uid map =
  let h = Item.Map.fold (fun key t acc ->
    Hashtbl.hash (acc, Item.hash key, t.hash)) map 0
  in
  { uid; desc = Struct map; hash = Hashtbl.hash (hash_struct, uid, h);
    approximated = false }

let alias ?uid t =
  { uid; desc = Alias t; hash = Hashtbl.hash (hash_alias, uid, t.hash); approximated = false}

let error ?uid s =
  { uid; desc = Error s; hash = Hashtbl.hash (hash_error, uid, s); approximated = false}

let leaf' uid =
  { uid; desc = Leaf; hash = Hashtbl.hash (hash_leaf, uid);
    approximated = false }

let leaf uid = leaf' (Some uid)

let type_decl uid name =
  { uid;
    desc = Type_decl name;
    hash = Hashtbl.hash (hash_alias, uid, name);
    approximated = false }

let approx t = { t with approximated = true}

let set_approximated ~approximated t = { t with approximated}

let proj ?uid t item =
  match t.desc with
  | Leaf ->
      (* When stuck projecting in a leaf we propagate the leaf
        as a best effort *)
      approx t
  | Struct map ->
      begin try Item.Map.find item map
      with Not_found -> approx t (* ill-typed program *)
      end
  | _ ->
      { uid; desc = Proj (t, item);
        hash = Hashtbl.hash (hash_proj, t.hash, item); approximated = false }

let app ?uid f ~arg =
      { uid; desc = App (f, arg); hash = Hashtbl.hash (hash_app, f.hash, uid, arg.hash)
        ; approximated = false }

let comp_unit ?uid s =
      { uid; desc = Comp_unit s; hash = Hashtbl.hash (hash_comp_unit, uid, s);
        approximated = false }

let no_fuel_left ?uid s = { s with uid }

let decompose_abs t =
  match t.desc with
  | Abs (x, t) -> Some (x, t)
  | _ -> None

let dummy_mod = str Item.Map.empty

let of_path ~find_shape ~namespace path =
  (* We need to handle the following cases:
    Path of constructor:
      M.t.C
    Path of label:
      M.t.lbl
    Path of label of inline record:
      M.t.C.lbl
    Path of label of implicit unboxed record:
      M.t#.lbl
  *)
  let rec aux : Sig_component_kind.t -> Path.t -> t = fun ns -> function
    | Pident id -> find_shape ns id
    | Pdot (Pextra_ty (path, Punboxed_ty), name) ->
      (match ns with
       Unboxed_label -> ()
       | _ -> Misc.fatal_error "Shape.of_path");
      proj (aux Type path) (name, Label)
    | Pdot (path, name) ->
      let namespace :  Sig_component_kind.t =
        match (ns : Sig_component_kind.t) with
        | Constructor -> Type
        | Label -> Type
        | Unboxed_label -> Type
        | _ -> Module
      in
      proj (aux namespace path) (name, ns)
    | Papply (p1, p2) -> app (aux Module p1) ~arg:(aux Module p2)
    | Pextra_ty (path, extra) -> begin
        match extra with
          Pcstr_ty name -> proj (aux Type path) (name, Constructor)
        | Pext_ty -> aux Extension_constructor path
        | Punboxed_ty -> aux ns path
      end
  in
  aux namespace path

let for_persistent_unit s =
  comp_unit ~uid:(Compilation_unit s) s

let leaf_for_unpack = leaf' None

let set_uid_if_none t uid =
  match t.uid with
  | None -> { t with uid = Some uid }
  | _ -> t


let poly_variant_constructors_map f pvs =
  List.map
    (fun pv -> { pv with pv_constr_args = List.map f pv.pv_constr_args })
    pvs

let rec shape_layout (sh : Layout.t ts) =
  match sh with
  | Ts_constr ((_, ly), _) -> ly
  | Ts_tuple _ -> Layout.Base Value
  | Ts_unboxed_tuple shapes -> Layout.Product (List.map shape_layout shapes)
  | Ts_var (_, ly) -> ly
  | Ts_predef (predef, _) -> Predef.predef_to_layout predef
  | Ts_arrow _ -> Layout.Base Value
  | Ts_variant _ -> Layout.Base Value
  | Ts_other ly -> ly


let complex_constructor_map f { name; kind; args } =
  let args =
    List.map
      (fun { field_name; field_value } ->
        { field_name; field_value = f field_value })
      args
  in
  { name; kind; args }

let complex_constructors_map f = List.map (complex_constructor_map f)

let rec shape_with_layout ~(layout : Layout.t) (sh : without_layout ts) :
    Layout.t ts =
  match sh, layout with
  | Ts_constr ((sh, Layout_to_be_determined), shapes), _ ->
    Ts_constr ((sh, layout), shapes)
  | Ts_tuple shapes, Base Value ->
    let layouted_shapes =
      List.map (shape_with_layout ~layout:(Layout.Base Value)) shapes
    in
    Ts_tuple layouted_shapes
  | ( Ts_tuple _,
      ( Product _
      | Base
          ( Void | Bits32 | Bits64 | Float64 | Float32 | Word | Vec128
          | Vec256 | Vec512 ) ) ) ->
    Misc.fatal_errorf "tuple shape must have layout value, but has layout %a"
      Layout.format layout
  | Ts_unboxed_tuple shapes, Product lys
    when List.length shapes = List.length lys ->
    let shapes_and_layouts = List.combine shapes lys in
    let layouted_shapes =
      List.map
        (fun (shape, layout) -> shape_with_layout ~layout shape)
        shapes_and_layouts
    in
    Ts_unboxed_tuple layouted_shapes
  | Ts_unboxed_tuple shapes, Product lys ->
    Misc.fatal_errorf "unboxed tuple shape has %d shapes, but %d layouts"
      (List.length shapes) (List.length lys)
  | ( Ts_unboxed_tuple _,
      Base
        ( Void | Value | Float32 | Float64 | Word | Bits32 | Bits64 | Vec128
        | Vec256 | Vec512 ) ) ->
    Misc.fatal_errorf
      "unboxed tuple must have unboxed product layout, but has layout %a"
      Layout.format layout
  | Ts_var (name, Layout_to_be_determined), _ -> Ts_var (name, layout)
  | Ts_arrow (arg, ret), Base Value -> Ts_arrow (arg, ret)
  | Ts_arrow _, _ ->
    Misc.fatal_errorf "function type shape must have layout value"
  | Ts_predef (predef, shapes), _
    when Layout.equal (Predef.predef_to_layout predef) layout ->
    Ts_predef (predef, shapes)
  | Ts_predef (predef, _), _ ->
    Misc.fatal_errorf
      "predef has layout %a, but is expected to have layout %a" Layout.format
      (Predef.predef_to_layout predef)
      Layout.format layout
  | Ts_variant fields, Base Value ->
    let fields =
      poly_variant_constructors_map
        (shape_with_layout ~layout:(Layout.Base Value))
        fields
    in
    Ts_variant fields
  | Ts_variant _, _ ->
    Misc.fatal_errorf "polymorphic variant must have layout value"
  | Ts_other Layout_to_be_determined, _ -> Ts_other layout

(* CR sspies: This is a hacky "solution" to do type variable substitution in
    type expression shapes. In subsequent PRs, this code should be changed to
    use the shape mechanism instead. *)
let rec replace_tvar_type_shape t ~(pairs : (without_layout ts * without_layout ts) list) =
  match
    List.filter_map
      (fun (from, to_) -> if t = from then Some to_ else None)
      pairs
  with
  | new_type :: _ -> new_type
  | [] -> (
    match t with
    | Ts_constr (uid, shape_list) ->
      Ts_constr (uid, List.map (replace_tvar_type_shape ~pairs) shape_list)
    | Ts_tuple shape_list ->
      Ts_tuple (List.map (replace_tvar_type_shape ~pairs) shape_list)
    | Ts_unboxed_tuple shape_list ->
      Ts_unboxed_tuple (List.map (replace_tvar_type_shape ~pairs) shape_list)
    | Ts_var (name, ly) -> Ts_var (name, ly)
    | Ts_predef (predef, shape_list) -> Ts_predef (predef, shape_list)
    | Ts_arrow (arg, ret) ->
      Ts_arrow (replace_tvar_type_shape ~pairs arg, replace_tvar_type_shape ~pairs ret)
    | Ts_variant fields ->
      let fields =
        poly_variant_constructors_map (replace_tvar_type_shape ~pairs) fields
      in
      Ts_variant fields
    | Ts_other ly -> Ts_other ly)

(* CR sspies: This is a hacky "solution" to do type variable substitution in
    type declaration shapes. In subsequent PRs, this code should be changed to
    use the shape mechanism instead. *)
let replace_tvar (t : tds)
    (shapes : without_layout ts list) =
  match List.length t.type_params == List.length shapes with
  | true ->
    let subst = List.combine t.type_params shapes in
    let replace_tvar (sh, ly) = replace_tvar_type_shape ~pairs:subst sh, ly in
    let ret =
      { type_params = [];
        definition =
          (match t.definition with
          | Tds_variant { simple_constructors; complex_constructors } ->
            Tds_variant
              { simple_constructors;
                complex_constructors =
                  complex_constructors_map replace_tvar complex_constructors
              }
          | Tds_variant_unboxed { name; arg_name; arg_shape; arg_layout } ->
            Tds_variant_unboxed
              { name;
                arg_name;
                arg_shape = replace_tvar_type_shape ~pairs:subst arg_shape;
                arg_layout
              }
          | Tds_record { fields; kind } ->
            Tds_record
              { fields =
                  List.map
                    (fun (name, sh, ly) ->
                      name, replace_tvar_type_shape ~pairs:subst sh, ly)
                    fields;
                kind
              }
          | Tds_alias type_shape ->
            Tds_alias (replace_tvar_type_shape ~pairs:subst type_shape)
          | Tds_other -> Tds_other)
      }
    in
    ret
  | false -> { type_params = []; definition = Tds_other }



module Map = struct
  type shape = t
  type nonrec t = t Item.Map.t

  let empty = Item.Map.empty

  let add t item shape = Item.Map.add item shape t

  let add_value t id uid = Item.Map.add (Item.value id) (leaf uid) t
  let add_value_proj t id shape =
    let item = Item.value id in
    Item.Map.add item (proj shape item) t

  let add_type t id shape = Item.Map.add (Item.type_ id) shape t
  let add_type_proj t id shape =
    let item = Item.type_ id in
    Item.Map.add item (proj shape item) t

  let add_constr t id shape = Item.Map.add (Item.constr id) shape t
  let add_constr_proj t id shape =
    let item = Item.constr id in
    Item.Map.add item (proj shape item) t

  let add_label t id uid = Item.Map.add (Item.label id) (leaf uid) t
  let add_label_proj t id shape =
    let item = Item.label id in
    Item.Map.add item (proj shape item) t

  let add_unboxed_label t id uid =
    Item.Map.add (Item.unboxed_label id) (leaf uid) t
  let add_unboxed_label_proj t id shape =
    let item = Item.unboxed_label id in
    Item.Map.add item (proj shape item) t

  let add_module t id shape = Item.Map.add (Item.module_ id) shape t
  let add_module_proj t id shape =
    let item = Item.module_ id in
    Item.Map.add item (proj shape item) t

  let add_module_type t id uid =
    Item.Map.add (Item.module_type id) (leaf uid) t
  let add_module_type_proj t id shape =
    let item = Item.module_type id in
    Item.Map.add item (proj shape item) t

  let add_extcons t id shape =
    Item.Map.add (Item.extension_constructor id) shape t
  let add_extcons_proj t id shape =
    let item = Item.extension_constructor id in
    Item.Map.add item (proj shape item) t

  let add_class t id uid = Item.Map.add (Item.class_ id) (leaf uid) t
  let add_class_proj t id shape =
    let item = Item.class_ id in
    Item.Map.add item (proj shape item) t

  let add_class_type t id uid = Item.Map.add (Item.class_type id) (leaf uid) t
  let add_class_type_proj t id shape =
    let item = Item.class_type id in
    Item.Map.add item (proj shape item) t
end
