(lang dune 3.20)

(name ocamlr)

(generate_opam_files true)

(source
 (github asleep-cult/ocamlr))

(package
 (name ocamlr)
 (synopsis "An LR paraser hobby project to teach me functional programming
  concepts in OCaml.")
 (depends ocaml))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
