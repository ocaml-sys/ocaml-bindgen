let translate file = 
  let ast = Parser.parse file in
  let ir = Ir.lift ast in
  let caml = Codegen.to_caml ir in
  Format.printf "%a" (Printast.structure 0) caml
