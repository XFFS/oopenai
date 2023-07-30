(lang dune 2.9)
(name oopenai)
(cram enable)
(generate_opam_files true)

(license MIT)
(authors "Freda Xin" "Shon Feder")
(maintainers "Freda Xin" "Shon Feder")
(source (github XFFS/oopenai))

(package
 (name oopenai)
 (synopsis "An OCaml Client Library for the OpenAI API")
 (description "This is an unofficial client library for the [OpenAI API](https://platform.openai.com/docs/api-reference) implemented in OCaml. The library was generated initially by the OCamlClientCodegen code generator for [openapi-generator](https://openapi-generator.tech). However, due to various errors in the code generation, a significant portion of this library was manually implemented.")
 (depends
  (dune (> 2.9))
  (ocaml (>= 5.0))

  cohttp
  cohttp-lwt-unix
  tls
  tls-lwt
  ocaml-migrate-parsetree
  (multipart_form (>= 0.5.0))
  (multipart_form-lwt (>= 0.5.0))
  (multipart_form-cohttp-lwt (>= 0.5.0))
  uuidm

  ppx_deriving
  ppx_deriving_yojson

  (ppx_expect (and :with-test (>= v0.15.1)))
  (alcotest :with-test)
  (alcotest-lwt :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)))
