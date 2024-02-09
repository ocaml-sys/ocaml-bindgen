type t = { lib_name : string; caml_file_name : string; c_file_name : string }

let from_ir (ir : Ir.t) : t =
  {
    lib_name = ir.lib_name;
    caml_file_name = ir.lib_name ^ ".ml";
    c_file_name = "caml_" ^ ir.lib_name ^ ".c";
  }
