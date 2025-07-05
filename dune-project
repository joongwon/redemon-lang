(lang dune 3.17)

(using menhir 3.0)

(name redemon_lang)

(generate_opam_files true)

(source
 (github joongwon/redemon-lang))

(package
 (name redemon_lang)
 (depends
  ocaml
  menhir
  dune
  base
  logs
  tls-lwt
  cohttp-lwt-unix
  js_of_ocaml
  js_of_ocaml-ppx
  js_of_ocaml-lwt
  promise_jsoo_lwt
  ppx_deriving
  yojson
  ppx_yojson_conv
  (alcotest :with-test)))
