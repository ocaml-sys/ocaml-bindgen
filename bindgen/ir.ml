type field = { fld_name : string }

type ir_type =
  | Abstract of string
  | Record of { rec_name : string; rec_fields : field list }
  | Enum of { enum_name : string }
  | Ptr of ir_type

type item = Ir_type of ir_type
type t = { items : item list; lib_name : string }

module Lift = struct
  let lift_record_field (field : Clang.Ast.decl) =
    match field.desc with
    | Clang.Ast.Field { name; _ } -> { fld_name = name }
    | _ -> assert false

  let lift_record (record : Clang.Ast.record_decl) =
    match (record.name, record.fields) with
    | "", _ -> None
    | _, [] -> Some (Ir_type (Abstract record.name))
    | _ ->
        let rec_fields = List.map lift_record_field record.fields in
        Some (Ir_type (Record { rec_name = record.name; rec_fields }))

  let lift_enum name _constants _complete_definition _attributes =
    if name = "" then None else Some (Ir_type (Enum { enum_name = name }))

  let lift ~name (clang_ast : Clang.Ast.translation_unit) : t =
    let node : Clang.Ast.translation_unit_desc = clang_ast.desc in
    let items =
      List.filter_map
        (fun (x : Clang.Ast.decl) ->
          let desc : Clang.Ast.decl_desc = x.desc in
          match desc with
          | Clang.Ast.RecordDecl record -> lift_record record
          | Clang.Ast.EnumDecl
              { name; constants; complete_definition; attributes } ->
              lift_enum name constants complete_definition attributes
          | Clang.Ast.TemplateDecl _ | Clang.Ast.Function _
          | Clang.Ast.TemplatePartialSpecialization _ | Clang.Ast.CXXMethod _
          | Clang.Ast.Var _ | Clang.Ast.TypedefDecl _ | Clang.Ast.Field _
          | Clang.Ast.IndirectField _ | Clang.Ast.AccessSpecifier _
          | Clang.Ast.Namespace _ | Clang.Ast.UsingDirective _
          | Clang.Ast.UsingDeclaration _ | Clang.Ast.Constructor _
          | Clang.Ast.Destructor _ | Clang.Ast.LinkageSpec _
          | Clang.Ast.TemplateTemplateParameter _ | Clang.Ast.Friend _
          | Clang.Ast.NamespaceAlias _ | Clang.Ast.EmptyDecl
          | Clang.Ast.Directive _ | Clang.Ast.StaticAssert _
          | Clang.Ast.TypeAlias _ | Clang.Ast.Decomposition _
          | Clang.Ast.Concept _ | Clang.Ast.Export _
          | Clang.Ast.UnknownDecl (_, _) ->
              None)
        node.items
    in
    { lib_name = name; items }
end

let lift = Lift.lift
