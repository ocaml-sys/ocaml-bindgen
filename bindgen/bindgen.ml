let translate ~file ~name = 
  let ast = Parser.parse file in
  let ir = Ir.lift ast in
  let caml = Caml.from_ir ir in
  let c = C.from_ir ir in
  Codegen.write_caml_files caml (name ^ ".ml");
  Codegen.write_c_files c ("caml_"^ name ^ ".c");
  ()
