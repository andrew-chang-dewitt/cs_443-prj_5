(lang dune 2.9)
(name cs443-proj2)
(version 0.1)
(using menhir 2.1)

(generate_opam_files true)

(package
 (name cs443-proj2)
 (synopsis "IITRAN to LLVM Compiler")
 (depends
  (ocaml (>= 4.14.2))
  menhir
  ppx_deriving))
