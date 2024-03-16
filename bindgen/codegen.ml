let with_file file fn =
  let oc = open_out file in
  let fmt = Format.formatter_of_out_channel oc in
  fn fmt;
  close_out oc;
  ()

let read_file file = In_channel.with_open_bin file In_channel.input_all

let write_dune_file (dune : Dunefile.t) =
  with_file "dune" @@ fun fmt ->
  Format.fprintf fmt
    {|
; automatically generated by ocaml-bindgen 0.0.1
(library
 (name %s)
 (foreign_stubs
  (language c)
  (names %s)
  (flags
   (:standard -O2))))
|}
    dune.lib_name dune.c_file_name;
  Format.fprintf fmt "\n%!"

let write_caml_files caml (dune : Dunefile.t) =
  let runtime_ocaml = read_file "../bindgen/runtime.ml" in
  with_file dune.caml_file_name @@ fun fmt ->
  Format.fprintf fmt "(* automatically generated by ocaml-bindgen 0.0.1 *)\n";
  Format.fprintf fmt "%s\n" runtime_ocaml;
  Format.fprintf fmt "%s\n%!" (Format.asprintf "%a" Pprintast.structure caml)

let write_c_files (c : C.program) (dune : Dunefile.t) =
  with_file dune.c_file_name @@ fun fmt ->
  Format.fprintf fmt "/* automatically generated by ocaml-bindgen 0.0.1 */\n";
  Format.fprintf fmt "\n%s\n%!" (Format.asprintf "%a" C.pp c);
  Format.fprintf fmt
    {|#include <stdlib.h>
value bindgen_alloc(value caml_size) {
    CAMLparam1(caml_size);

    // Convert OCaml integer to C size
    size_t size = Int_val(caml_size);
    printf("Allocated size %%ld \n", size);

    void* ptr = malloc(sizeof(size));
    if (ptr == NULL) {
        // TODO: handle allocation failure
        CAMLreturn(Val_unit);
    }
    
    // Wrap the pointer as an OCaml value
    CAMLreturn(caml_copy_nativeint(ptr));
}

void bindgen_free(value caml_addr) {
    free(Nativeint_val(caml_addr));
} |}
