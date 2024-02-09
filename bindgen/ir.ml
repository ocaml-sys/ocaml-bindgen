type item = Ir_record of { rec_name : string }
type t = {
  items: item list;
  lib_name: string
}


module Lift = struct
  let lift_record (record : Clang.Ast.record_decl) =
    Ir_record { rec_name = record.name }

  let lift ~name (clang_ast : Clang.Ast.translation_unit) : t =
    let node : Clang.Ast.translation_unit_desc = clang_ast.desc in
    let items = List.filter_map
      (fun (x : Clang.Ast.decl) ->
        let desc : Clang.Ast.decl_desc = x.desc in
        match desc with
        | Clang.Ast.RecordDecl record -> Some (lift_record record)
        | Clang.Ast.EnumDecl _ | Clang.Ast.TemplateDecl _ | Clang.Ast.Function _
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
    { lib_name=name; items }
end

let lift = Lift.lift
