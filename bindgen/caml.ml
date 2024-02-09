open Ast_helper

let from_ir (ir: Ir.t): Parsetree.structure = 
  let loc = !default_loc in
  List.map (fun node ->
    match node with 
    | Ir.Ir_record {rec_name} -> 
        let typ = Type.mk ~loc ({txt=rec_name; loc}) in
        Ast_helper.Str.type_ Nonrecursive [typ]
  ) ir.items
