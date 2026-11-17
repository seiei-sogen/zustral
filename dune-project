(lang dune 2.7)
(using menhir 2.0)

(name zustral)
(version 0.0.1)

(package
 (name zustral)
 (synopsis "The bootstrapping compiler for Zustral")
 (description "The bootstrapping compiler for Zustral.")
 (homepage https://austral-lang.org/)
 (bug_reports https://github.com/austral/austral/issues)
 (depends
  (ppxlib (>= 0.25.1))
  (ppx_deriving (= 5.2.1))
  (ounit2 (= 2.2.7))
  (menhir (= 20220210))
  (zarith (= 1.12))
  (yojson (= 2.1.0))))

(generate_opam_files)
