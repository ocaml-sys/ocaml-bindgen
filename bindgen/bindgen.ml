let translate ~file ~name =
  let ast = Parser.parse file in
  let ir = Ir.lift ~name ast in
  let dunefile = Dunefile.from_ir ir in
  let caml = Caml.from_ir ir in
  let c = C.from_ir ir in
  Codegen.write_dune_file dunefile;
  Codegen.write_caml_files caml dunefile;
  Codegen.write_c_files c dunefile;
  ()
