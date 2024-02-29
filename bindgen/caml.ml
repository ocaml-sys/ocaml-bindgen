open Parsetree
open Ast_helper

let loc = !default_loc
let with_loc txt : str = { txt; loc }

let lid name =
  Location.mkloc
    (Longident.unflatten [ String.lowercase_ascii name ] |> Option.get)
    loc

let type_name name = String.lowercase_ascii name |> with_loc

(** Converts a C identifier to one compatible with OCaml variant constuctor naming *)
let enum_valid_name ?(prefix_underscore = false) name : string =
  let rec rm_underscore_and_capitalize s =
    if String.length s = 0 then s
    else
      match String.get s 0 with
      | '_' ->
          let prefix = if prefix_underscore then "Under" else "" in
          prefix
          ^ rm_underscore_and_capitalize (String.sub s 1 (String.length s - 1))
      | _ -> String.capitalize_ascii s
  in
  rm_underscore_and_capitalize name

let variant_from_enum (ir_enum : Ir.ir_enum_variant) : constructor_declaration =
  (* C enums don't carry data so most of the fields are left empty / default *)
  {
    pcd_name = with_loc (enum_valid_name ir_enum.variant_name);
    pcd_vars = [];
    pcd_args = Pcstr_tuple [];
    pcd_res = None;
    pcd_loc = loc;
    pcd_attributes = [];
  }

let rec core_type_from_ir typ =
  match typ with
  | Ir.Abstract name -> Typ.constr (lid name) []
  | Ir.Enum { enum_name; _ } -> Typ.constr (lid enum_name) []
  | Ir.Record { rec_name; _ } -> Typ.constr (lid rec_name) []
  | Ir.Prim Int -> Typ.constr (lid "int") []
  | Ir.Prim Bool -> Typ.constr (lid "bool") []
  | Ir.Prim Char -> Typ.constr (lid "char") []
  | Ir.Prim Void -> Typ.constr (lid "unit") []
  | Ir.Ptr t -> core_type_from_ir t
  | Ir.Func { fn_ret; fn_params } ->
      List.fold_left
        (fun acc (name, typ) ->
          let label = Asttypes.Labelled name in
          let typ = core_type_from_ir typ in
          Typ.arrow label typ acc)
        (core_type_from_ir fn_ret) fn_params

let type_from_ir typ =
  match typ with
  | Ir.Abstract name -> Some (Type.mk ~loc (type_name name))
  | Ir.Enum { enum_name; enum_variants } ->
      let variants = List.map variant_from_enum enum_variants in
      let kind = Ptype_variant variants in
      Some (Type.mk ~loc ~kind (type_name enum_name))
  | Ir.Record { rec_name; rec_fields } ->
      let labels =
        List.map
          Ir.(
            fun fld ->
              let fld_type = core_type_from_ir fld.fld_type in
              Type.field (with_loc fld.fld_name) fld_type)
          rec_fields
      in
      let kind = Ptype_record labels in
      Some (Type.mk ~loc ~kind (type_name rec_name))
  | _ -> None

let str_type_from_ir typ =
  match type_from_ir typ with
  | Some typ -> Some (Str.type_ Nonrecursive [ typ ])
  | None -> None

let str_external_from_ir ({ fndcl_name; fndcl_type } : Ir.ir_fun_decl) =
  let prim =
    Val.mk
      ~prim:[ "caml_" ^ fndcl_name ]
      ~loc (with_loc fndcl_name)
      (core_type_from_ir fndcl_type)
  in
  Some (Str.primitive ~loc prim)

let from_ir (ir : Ir.t) : Parsetree.structure =
  List.filter_map
    (fun node ->
      match node with
      | Ir.Ir_type typ -> str_type_from_ir typ
      | Ir.Ir_fun_decl fun_dcl -> str_external_from_ir fun_dcl)
    ir.items
