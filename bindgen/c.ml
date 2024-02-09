open Clang
open Ast

let from_ir (ir : Ir.t) : Clang.Ast.translation_unit =
  let items =
    List.map
      (fun node ->
        match node with
        | Ir.Ir_record { rec_name } ->
            let record =
              {
                keyword = Obj.magic 0;
                attributes = [];
                nested_name_specifier = None;
                name = rec_name;
                bases = [];
                fields = [];
                final = true;
                complete_definition = true;
                is_injected_class_name = false;
              }
            in
            Ast.node (Ast.RecordDecl record))
      ir.items
  in
  let desc = { filename = "file.c"; items } in
  Ast.node desc
