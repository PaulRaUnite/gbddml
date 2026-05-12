(lang dune 3.14)

(license "CECILL-2.1")

(authors "Pascal Raymond")

(maintainers "erwan.jahier@univ-grenoble-alpes.fr")

(package
 (name gbddml)
 (synopsis "The Verimag BDD library")
 (description "An OCaml wrapper of C++ BDD library")
 (depends
  (ocaml
   (>= 4.05))
  (base-unix (= "build"))
  (dune
   (>= 2.0))
  (camlidl
   (>= 1.09))))
