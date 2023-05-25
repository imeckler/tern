(lang dune 3.3)

(name tern)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Izaak Meckler")

(maintainers "Izaak Meckler")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tern)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  ppx_deriving_yojson
   core eio_main ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
