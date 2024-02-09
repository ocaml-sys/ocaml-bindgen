let () =
  let file = Sys.argv.(1) in
  let name = Sys.argv.(2) in
  Bindgen.translate ~file ~name
