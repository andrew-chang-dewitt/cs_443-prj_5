(lang dune 2.9)
(name c443)
(version 0.1)
(using menhir 2.1)

(generate_opam_files true)

(package
 (name c443)
 (synopsis "Compiler for IIT CS443")
 (depends
  (ocaml (>= 4.14.2))
  menhir
  ppx_deriving))
