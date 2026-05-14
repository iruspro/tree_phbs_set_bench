(lang dune 3.21)

(name tree_phbs_set_bench)

(generate_opam_files true)

(source
 (github iruspro/tree_phbs_set_bench))

(authors "Ruslan Urazbakhtin, Lovro Zupan Škrlj")

(package
 (name tree_phbs_set_bench)
 (depends ocaml bentley_saxe_transform phf aa_tree))
