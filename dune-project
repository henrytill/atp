(lang dune 3.16)

(using menhir 3.0)

(using mdx 0.2)

(name atp)

(generate_opam_files true)

(source
 (github henrytill/atp))

(authors "Henry Till <henrytill@gmail.com>")

(maintainers "Maintainer Name")

(license Unlicense)

(package
 (name atp)
 (synopsis "Handbook of Practical Logic and Automated Reasoning")
 (description
  "Work from Harrison's \"Handbook of Practical Logic and Automated Reasoning\"")
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  menhir
  (alcotest
   (and
    :with-test
    (>= 1.7.0)))
  (mdx
   (and
    :with-test
    (>= 2.4.1)))
  (ppxlib
   (>= 0.33.0))
  (zarith
   (>= 1.14))))
