(* open Doggo *)

module Doggo = struct
  include Doggo_sys
  (** Here is where we would write our wrappers around the raw bindings *)

  let wrapper_print_age (age : int) = print_age ~age:(create_ptr age)
end

let () = Doggo.wrapper_print_age 25
