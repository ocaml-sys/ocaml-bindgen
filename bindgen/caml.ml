open Ast_helper

let from_ir (ir: Ir.t): Parsetree.structure = 
  let loc = !default_loc in
  List.map (fun node ->
    match node with 
    | Ir.Ir_record {rec_name=_} -> [%stri type a ]
  ) ir.items
