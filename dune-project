(lang dune 3.11)

(name botocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name botocaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   (ocaml (>= 5.1.1))
   (alcotest :with-test)
   dune 
   angstrom
   cohttp
   lwt
   mirage-crypto
   ocurl
   timedesc
   uri
   yojson
   ppx_deriving
   ppx_yojson_conv
   ppx_fields_conv
   ezcurl
   ezcurl-lwt
 )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
