
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("==========================================================================================================================================")
  packageStartupMessage('The "ngram_sequential" and "ngram_overlap" stemmers were removed from the "sparse_term_matrix" and "tokenize_transform_vec_docs" functions')
  packageStartupMessage('For details see the "News.md" file of version 1.0.5')
  packageStartupMessage("==========================================================================================================================================")
}
