(lang dune 3.14)

(using menhir 3.0)

(name atp)

(generate_opam_files true)

(source
 (github henrytill/atp))

(authors "Henry Till <henrytill@gmail.com>")

(maintainers "Henry Till <henrytill@gmail.com>")

(license Unlicense)

(documentation https://url/to/documentation)

(package
 (name atp)
 (synopsis "Handbook of Practical Logic and Automated Reasoning")
 (description "Work from Harrison's \"Handbook of Practical Logic and Automated Reasoning\"")
 (depends ocaml dune menhir (alcotest (and :with-test (>= 1.7.0)))))
