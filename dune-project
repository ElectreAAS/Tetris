(lang dune 3.7)

(name tetriml)

(generate_opam_files true)

(package
 (name tetriml)
 (synopsis "tetriml")
 (depends
  gamelle
  (ocamlformat
   (and
    :with-dev-setup
    (>= 0.26.2)))))
