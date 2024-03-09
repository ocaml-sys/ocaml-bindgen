(lang dune 3.13)
(using menhir 3.0)
(using directory-targets 0.1)
(cram enable)

(name bindgen)

(generate_opam_files true)

(source
 (github ocaml-sys/ocaml-bindgen))

(authors "Leandro Ostera <leandro@abstractmachines.dev>")

(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")

(license MIT)

(package
 (name bindgen)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   (clangml (>= "4.8.0"))
   stringext
   ocaml
   dune)
 (tags
  (topics "to describe" your project)))
