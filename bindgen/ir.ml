type ir_prim_type = Int | Float | Bool | Char | Void

type ir_type =
  | Abstract of string
  | Record of { rec_name : string; rec_fields : ir_field list }
  | Enum of { enum_name : string; enum_variants : ir_enum_variant list }
  | Prim of ir_prim_type
  | Ptr of ir_type
  | Func of { fn_ret : ir_type; fn_params : (string * ir_type) list }

and ir_field = { fld_name : string; fld_type : ir_type }
and ir_enum_variant = { variant_name : string; constant : int }

type ir_fun_decl = { fndcl_name : string; fndcl_type : ir_type }
type ir_item = Ir_type of ir_type | Ir_fun_decl of ir_fun_decl
type t = { items : ir_item list; lib_name : string; header : string }

module Lift = struct
  let lift_name name =
    match name with Clang.Ast.IdentifierName x -> x | _ -> assert false

  let rec lift_type (typ : Clang.Type.t) =
    (* Format.printf "lift_type: %S\n" (Clang.Type.show typ); flush stdout; *)
    match typ.desc with
    | Clang.Ast.BuiltinType Int -> Prim Int
    | Clang.Ast.BuiltinType Float -> Prim Float
    | Clang.Ast.BuiltinType Bool -> Prim Bool
    | Clang.Ast.BuiltinType Char_S -> Prim Char
    | Clang.Ast.BuiltinType Void -> Prim Void
    | Clang.Ast.Pointer t -> Ptr (lift_type t)
    | Clang.Ast.Typedef t -> Abstract (lift_name t.name)
    | _ -> assert false

  let lift_record_field (field : Clang.Ast.decl) =
    match field.desc with
    | Clang.Ast.Field { name; qual_type; _ } ->
        let fld_type = lift_type qual_type in
        { fld_name = name; fld_type }
    | _ -> assert false

  let lift_record (record : Clang.Ast.record_decl) =
    match (record.name, record.fields) with
    | "", _ -> None
    | _, [] -> Some (Ir_type (Abstract record.name))
    | _ ->
        let rec_fields = List.map lift_record_field record.fields in
        Some (Ir_type (Record { rec_name = record.name; rec_fields }))

  let lift_enum name (constants : Clang.Ast.enum_constant list)
      _complete_definition _attributes =
    let ir_variants : ir_enum_variant list =
      List.mapi
        (fun i (constant : Clang.Ast.enum_constant) ->
          let desc = constant.desc in
          { variant_name = desc.constant_name; constant = i })
        constants
    in
    if name = "" then None
    else Some (Ir_type (Enum { enum_name = name; enum_variants = ir_variants }))

  let lift_function_param (param : Clang.Ast.parameter) =
    (param.desc.name, lift_type param.desc.qual_type)

  let lift_function_type (fn_type : Clang.Ast.function_type) =
    let fn_ret = lift_type fn_type.result in
    let fn_params =
      match fn_type.parameters with
      | Some { non_variadic; _ } -> List.map lift_function_param non_variadic
      | None -> []
    in
    Func { fn_ret; fn_params }

  let lift_function (fn : Clang.Ast.function_decl) =
    let fndcl_name = lift_name fn.name in
    let fndcl_type = lift_function_type fn.function_type in
    Some (Ir_fun_decl { fndcl_name; fndcl_type })

  let lift ~name ~header (clang_ast : Clang.Ast.translation_unit) : t =
    let node : Clang.Ast.translation_unit_desc = clang_ast.desc in
    let items =
      List.filter_map
        (fun (x : Clang.Ast.decl) ->
          let desc : Clang.Ast.decl_desc = x.desc in
          match desc with
          | Clang.Ast.Function fn -> lift_function fn
          | Clang.Ast.RecordDecl record -> lift_record record
          | Clang.Ast.EnumDecl
              { name; constants; complete_definition; attributes } ->
              lift_enum name constants complete_definition attributes
          | Clang.Ast.TemplateDecl _ | Clang.Ast.TemplatePartialSpecialization _
          | Clang.Ast.CXXMethod _ | Clang.Ast.Var _ | Clang.Ast.TypedefDecl _
          | Clang.Ast.Field _ | Clang.Ast.IndirectField _
          | Clang.Ast.AccessSpecifier _ | Clang.Ast.Namespace _
          | Clang.Ast.UsingDirective _ | Clang.Ast.UsingDeclaration _
          | Clang.Ast.Constructor _ | Clang.Ast.Destructor _
          | Clang.Ast.LinkageSpec _ | Clang.Ast.TemplateTemplateParameter _
          | Clang.Ast.Friend _ | Clang.Ast.NamespaceAlias _
          | Clang.Ast.EmptyDecl | Clang.Ast.Directive _
          | Clang.Ast.StaticAssert _ | Clang.Ast.TypeAlias _
          | Clang.Ast.Decomposition _ | Clang.Ast.Concept _ | Clang.Ast.Export _
          | Clang.Ast.UnknownDecl (_, _) ->
              None)
        node.items
    in
    { lib_name = name; header; items }
end

let lift = Lift.lift

let string_of_prim prim =
  match prim with
  | Int -> "Int"
  | Char -> "Char"
  | Float | Bool | Void -> "Other (TODO)"
