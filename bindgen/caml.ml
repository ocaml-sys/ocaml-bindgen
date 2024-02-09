open Parsetree
open Ast_helper

let loc = !default_loc
let with_loc txt : str = { txt; loc }
let lid name = Location.mkloc (Longident.unflatten [ name ] |> Option.get) loc
let type_name name = String.lowercase_ascii name |> with_loc

let core_type_from_ir typ =
  match typ with
  | Ir.Abstract name -> Typ.constr (lid name) []
  | Ir.Enum { enum_name } -> Typ.constr (lid enum_name) []
  | Ir.Record { rec_name; _ } -> Typ.constr (lid rec_name) []
  | Ir.Prim Int -> Typ.constr (lid "int") []
  | Ir.Prim Bool -> Typ.constr (lid "bool") []
  | Ir.Prim Char -> Typ.constr (lid "char") []
  | _ -> assert false

let type_from_ir typ =
  match typ with
  | Ir.Abstract name -> Some (Type.mk ~loc (type_name name))
  | Ir.Enum { enum_name } -> Some (Type.mk ~loc (type_name enum_name))
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

let from_ir (ir : Ir.t) : Parsetree.structure =
  List.filter_map
    (fun node -> match node with Ir.Ir_type typ -> str_type_from_ir typ)
    ir.items
