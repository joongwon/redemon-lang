(lang dune 3.18)
(using menhir 3.0)

(name redemon-lang)

(generate_opam_files true)

(source
 (github joongwon/redemon-lang))

(package
 (name redemon-lang)
 (depends
  ocaml
  menhir
  dune
  base
  js_of_ocaml
  js_of_ocaml-ppx
  yojson
  ppx_yojson_conv))
