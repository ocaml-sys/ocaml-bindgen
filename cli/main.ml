let () = 
  let arg = Sys.argv.(1) in
  Bindgen.translate arg
