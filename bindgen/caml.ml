open Parsetree
open Ast_helper

let loc = !default_loc
let with_loc txt : str = { txt; loc }

let type_from_ir typ =
  match typ with
  | Ir.Abstract name ->
      let typ = Type.mk ~loc (with_loc name) in
      Ast_helper.Str.type_ Nonrecursive [ typ ]
  | Ir.Enum { enum_name } ->
      let typ = Type.mk ~loc (with_loc enum_name) in
      Ast_helper.Str.type_ Nonrecursive [ typ ]
  | Ir.Record { rec_name; rec_fields } ->
      let labels =
        List.map
          Ir.(fun fld -> Type.field (with_loc fld.fld_name) (Typ.any ~loc ()))
          rec_fields
      in
      let kind = Ptype_record labels in
      let typ = Type.mk ~loc ~kind { txt = rec_name; loc } in
      Str.type_ Nonrecursive [ typ ]
  | Ir.Ptr _ -> assert false

let from_ir (ir : Ir.t) : Parsetree.structure =
  List.map
    (fun node -> match node with Ir.Ir_type typ -> type_from_ir typ)
    ir.items
