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
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
))
