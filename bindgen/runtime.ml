type lifetime =
  | Function
      (** The value can live for the lifetime of the function call, which upon return will signal that the 
               value can be dropped (finalizer?) *)
  | Ocaml  (** The value is managed by the OCaml runtime *)
  | C
      (** The value is allocated and passed to C which is then in charge of cleaning it up *)

type 'a cptr = { lifetime : lifetime; addr : nativeint }

external bindgen_alloc : size:int -> nativeint = "bindgen_alloc"
external bindgen_free : nativeint -> unit = "bindgen_free"

let sizeof _ = 4 (* TODO: how to handle different types? *)

let create_ptr (value : 'a) : 'a cptr =
  let addr = bindgen_alloc ~size:(sizeof value) in
  print_endline ("Addr: " ^ Nativeint.to_string addr);
  Gc.finalise bindgen_free addr;
  { lifetime = Ocaml; addr }
