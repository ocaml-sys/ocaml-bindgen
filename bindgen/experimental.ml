type lifetime =
  Function (** The value can live for the lifetime of the function call, which upon return will signal that the 
               value can be dropped (finalizer?) *)
  | Ocaml  (** The value is managed by the OCaml runtime *)
  | C      (** The value is allocated and passed to C which is then in charge of cleaning it up *)

type 'a ptr = {
  lifetime: lifetime;
  value: 'a;
}

(* The easiest way to call a function that requires a int ptr is to do *)

(* basically we need a funtion
  string -> char ptr that we can then pass to the function   
*)


(* let char_ptr (s: string) : char ptr = {} *)