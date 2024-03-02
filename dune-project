(lang dune 3.13)

(name url_shortner)

(generate_opam_files true)

(source
 (github sravan-s/url_shortner))

(authors "Sravan")

(maintainers "Sravan")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name url_shortner)
 (synopsis "Trying to build a url shortner webserver in ocaml")
 (description ":)")
 (depends 
   (ocaml (= "5.1.1"))
   dune
   (dream (>= "1.0.0"))
   (ppx_yojson_conv (= "0.16.0"))
   (sqlite3 (= "5.1.0"))
  )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
