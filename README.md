# `bindgen`

`bindgen` automatically generates OCaml FFI bindings to C (and some C++)
librariesboth as OCaml external definitions, types, constants, and the
appropriate C shim code.

For example, given the C header `doggo.h`:

```c
typedef struct Doggo {
    int many;
    char wow;
} Doggo;

void eleven_out_of_ten_majestic_af(Doggo* pupper);
```

`bindgen` produces OCaml FFI code allowing you to call into the `doggo` library's function and use its types:

```ocaml
(* automatically generated by ocaml-bindgen 0.0.1 *)

type doggo = {
  many: int
  wow: char
}

external eleven_out_of_ten_majestic_af : doggo -> unit = "caml_bindgen_doggo__eleven_out_of_ten_majestic_af"
```

And the follow C shim:

```c
/* automatically generated by ocaml-bindgen 0.0.1 */

value caml_Doggo_to_value(struct Doggo *pupper) {
  CAMLparam0();
  CAMLlocal1(caml_pupper);
  event = caml_alloc_tuple(2);
  Store_field(caml_pupper, 0, Val_int(pupper->many));
  Store_field(caml_pupper, 1, Val_char(pupper->wow));
  CAMLreturn(caml_pupper);
}

Doggo* caml_Doggo_of_value(value caml_pupper) {
    Doggo* pupper = malloc(sizeof(struct Doggo));
    pupper.many = Int_val(Field(caml_pupper, 0));
    pupper.wow = Char_val(Field(caml_pupper, 1));
    return pupper
}

CAMLprim value caml_bindgen_doggo__eleven_out_of_ten_majestic_af(value caml_pupper) {
    CAMLparam1(caml_pupper);
    Doggo* pupper = caml_Doggo_of_value(caml_pupper);
    // actual C call
    eleven_out_of_ten_majestic_af(pupper);
    CAMLreturn0();
}
```
