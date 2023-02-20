(lang dune 2.9)
(cram enable)
(generate_opam_files true)

(name oopenai)
(license MIT)
(authors "Shon Feder")
(maintainers "Shon Feder")
(source (github shonfeder/oopenai))

(package
 (name oopenai)
 (synopsis "Short description")
 (description "Longer description")
 (depends
  (dune (> 2.9))
  (ocaml (>= 5.0))

  (http-lwt-client (>= 0.2.1))
  (yojson (>= 2.0.2))

  (ppx_expect (and :wit-test (>= v0.15.1)))
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
  ))
