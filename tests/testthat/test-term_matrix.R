

if (.Platform$OS.type == "windows") {

  PATH = paste0(getwd(), path.expand("\\term_matrix_file.csv"))

  PATH_txt = paste0(getwd(), path.expand("\\term_matrix_file.txt"))
}

if (.Platform$OS.type == "unix") {

  PATH = paste0(getwd(), path.expand("/term_matrix_file.csv"))

  PATH_txt = paste0(getwd(), path.expand("/term_matrix_file.txt"))
}


docs = as.vector(read.csv(PATH, header = FALSE, stringsAsFactors = F)[, 1])


context('term matrix class')


#----------------------------------
# initialization [ error handling ]
#----------------------------------


testthat::test_that("in case that both vector_data and file_data are NULL it returns an error", {

  testthat::expect_error( sparse_term_matrix$new(vector_data = NULL, file_data = NULL, document_term_matrix = TRUE) )
})


testthat::test_that("in case that both vector_data and file_data are not NULL it returns an error", {

  testthat::expect_error( sparse_term_matrix$new(vector_data = docs, file_data = PATH, document_term_matrix = TRUE) )
})


testthat::test_that("in case that the vector_data parameter is not a vector of documents it returns an error", {

  testthat::expect_error( sparse_term_matrix$new(vector_data = list(), file_data = NULL, document_term_matrix = TRUE) )
})


testthat::test_that("in case that the file_data parameter is not a valid path to a file it returns an error", {

  testthat::expect_error( sparse_term_matrix$new(vector_data = NULL, file_data = "/invalid/path", document_term_matrix = TRUE) )
})


testthat::test_that("in case that the document_term_matrix parameter is not logical it returns an error", {

  testthat::expect_error( sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = 'TRUE') )
})



#------------------------------------------------------------------
# document-term-matrix OR term-document-matrix   [ error handling ]
#------------------------------------------------------------------


testthat::test_that("in case that the sort_terms parameter is not logical it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = 'FALSE', to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the to_lower parameter is not logical it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = 'FALSE', to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the to_upper parameter is not logical it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = 'FALSE', utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the utf_locale parameter is not a character string it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = NULL, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the remove_char parameter is not a character string it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = NULL, remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})




testthat::test_that("in case that the remove_punctuation_string parameter is not logical it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = 'FALSE', remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the remove_punctuation_vector parameter is not logical it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = 'FALSE',

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the remove_numbers parameter is not logical it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = 'FALSE', trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the trim_token parameter is not logical it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = 'FALSE', split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the split_string parameter is not logical it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = 'FALSE', split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the split_separator parameter is not a character string it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = NULL, remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the remove_stopwords parameter is not logical it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = NULL, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the language parameter is not one of the available it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "INVALID",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the min_num_char parameter is less than 1 it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 0, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the max_num_char parameter is less than the min_num_char it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = -Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the stemmer parameter is not one of the available it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = 'NULL', min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the min_n_gram parameter is less than 1 it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 0, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the max_n_gram parameter is less than 1 it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 0, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the min_n_gram is greater than the max_n_gram parameter it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 3, max_n_gram = 2, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})

testthat::test_that("in case that the skip_n_gram parameter is less than 1 it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 0, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the skip_distance parameter is less than 0 it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = -1, n_gram_delimiter = " ",

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the n_gram_delimiter parameter is not a character string it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = NULL,

                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the print_every_rows parameter is less than 1 it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 0, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the normalize parameter is not one of c(l1, l2) it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 100, normalize = 'NULL', tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the tf_idf parameter is not logical it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 100, normalize = NULL, tf_idf = 'FALSE',

                                           threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the threads parameter is less than 1 it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                                           threads = 0, verbose = FALSE) )
})


testthat::test_that("in case that the verbose parameter is not logical it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init$Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = 'FALSE') )
})



testthat::test_that("in case that either the to_lower or the to_upper parameter is TRUE and the language is other than english then a warning will be printed to the console", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_warning( init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                           remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "greek",

                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                           print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                                           threads = 1, verbose = FALSE) )
})




#---------------------------------------------------------------
# document-term-matrix OR term-document-matrix   [ expect true ]
#---------------------------------------------------------------



testthat::test_that("in case that the data is a vector of documents and the document_term_matrix parameter is TRUE it returns a sparse document_term_matrix with the correct number of rows (documents)", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = TRUE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_true( inherits(res, "dgCMatrix") && nrow(res) == length(docs)  )
})



testthat::test_that("in case that the data is a vector of documents AND the document_term_matrix parameter is TRUE AND a user defined list of stopwords is given, it returns a sparse

                    document_term_matrix with the correct number of rows (documents)", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = c("a", "this", "is"), language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_true( inherits(res, "dgCMatrix") && nrow(res) == length(docs)  )
})



testthat::test_that("in case that the data is a vector of documents and the document_term_matrix parameter is FALSE it returns a sparse term_document_matrix with the correct number of columns (documents)", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = FALSE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_true( inherits(res, "dgCMatrix") && ncol(res) == length(docs)  )
})



testthat::test_that("in case that the data input is a path to a file and the document_term_matrix parameter is TRUE it returns a sparse document_term_matrix with the correct number of rows (documents)", {

  init = sparse_term_matrix$new(vector_data = NULL, file_data = PATH_txt, document_term_matrix = TRUE)

  res = suppressWarnings(init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE))

  testthat::expect_true( inherits(res, "dgCMatrix") && nrow(res) == length(docs)  )
})


testthat::test_that("in case that the data input is a path to a file and the document_term_matrix parameter is FALSE it returns a sparse term_document_matrix with the correct number of columns (documents)", {

  init = sparse_term_matrix$new(vector_data = NULL, file_data = PATH_txt, document_term_matrix = FALSE)

  res = suppressWarnings(init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                          remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                          min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                          print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                                          threads = 1, verbose = FALSE))

  testthat::expect_true( inherits(res, "dgCMatrix") && ncol(res) == length(docs)  )
})


#---------------------------------------
# Term Matrix Adjust  [ error handling ]
#---------------------------------------


testthat::test_that("in case that the Term_Matrix method is not run in first place it returns an error", {

  init1 = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init1$Term_Matrix_Adjust(sparsity_thresh = 1.0) )
})



testthat::test_that("in case that sparsity_thresh is not numeric it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$Term_Matrix_Adjust(sparsity_thresh = NULL) )
})


testthat::test_that("in case that sparsity_thresh is less than or equal to 0.0 it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$Term_Matrix_Adjust(sparsity_thresh = 0.0) )
})



testthat::test_that("in case that sparsity_thresh is greater than 1.0 it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$Term_Matrix_Adjust(sparsity_thresh = 1.00001) )
})


testthat::test_that("in case that sparsity_thresh gives an empty matrix it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$Term_Matrix_Adjust(sparsity_thresh = 0.1) )
})




#------------------------------------
# Term Matrix Adjust  [ expect true ]
#------------------------------------


testthat::test_that("it returns a reduced sparse matrix in case that the sparsity_thresh is less than 1.0 and the document_term_matrix parameter is TRUE", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  res_sp = init$Term_Matrix_Adjust(sparsity_thresh = 0.8)

  testthat::expect_true( inherits(res, "dgCMatrix") && inherits(res_sp, "dgCMatrix") && ncol(res) > ncol(res_sp) )
})



testthat::test_that("it returns a reduced sparse matrix in case that the sparsity_thresh is less than 1.0 and the document_term_matrix parameter is FALSE", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = FALSE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  res_sp = init$Term_Matrix_Adjust(sparsity_thresh = 0.8)

  testthat::expect_true( inherits(res, "dgCMatrix") && inherits(res_sp, "dgCMatrix") && nrow(res) > nrow(res_sp) )
})



#--------------------------------------
# term associations  [ error handling ]
#--------------------------------------


testthat::test_that("in case that the Term_Matrix method is not run in first place it returns an error", {

  init1 = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init1$term_associations(Terms = c("the"), keep_terms = 10, threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that Terms is not a character vector it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$term_associations(Terms = NULL, keep_terms = 10, threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that Terms is a character vector with length less than 1 it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$term_associations(Terms = character(0), keep_terms = 10, threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that keep_terms is not a numeric value it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$term_associations(Terms = c("the", "and"), keep_terms = list(), threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that keep_terms is a numeric value less than 1 it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$term_associations(Terms = c("the", "and"), keep_terms = 0, threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the threads parameter is less than 1 it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$term_associations(Terms = c("the", "and"), keep_terms = NULL, threads = 0, verbose = FALSE) )
})



testthat::test_that("in case that the verbose parameter is not a boolean it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$term_associations(Terms = c("the", "and"), keep_terms = NULL, threads = 1, verbose = 'FALSE') )
})




#-----------------------------------
# term associations  [ expect true ]
#-----------------------------------


testthat::test_that("it returns the correct output in case of a single term", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  target_term = "is"

  single_out = init$term_associations(Terms = target_term, keep_terms = NULL, threads = 1, verbose = FALSE)

  testthat::expect_true( inherits(single_out, c("data.table","data.frame")) && nrow(single_out) == ncol(res) - 1 && ncol(single_out) == 2 && sum(colnames(single_out) %in% c('term', 'correlation')) == 2 && !target_term %in% single_out$term )
})


testthat::test_that("it returns the correct output in case of a single term ( if term-document-matrix is TRUE )", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = FALSE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  target_term = "is"

  single_out = init$term_associations(Terms = target_term, keep_terms = NULL, threads = 1, verbose = FALSE)

  testthat::expect_true( inherits(single_out, c("data.table","data.frame")) && nrow(single_out) == nrow(res) - 1 && ncol(single_out) == 2 && sum(colnames(single_out) %in% c('term', 'correlation')) == 2 && !target_term %in% single_out$term )
})




testthat::test_that("it returns the correct output in case that the Term_Matrix_Adjust method is called first", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = FALSE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  res1 = init$Term_Matrix_Adjust(sparsity_thresh = 0.85)

  target_term = "of"

  single_out = init$term_associations(Terms = target_term, keep_terms = NULL, threads = 1, verbose = FALSE)

  testthat::expect_true( inherits(single_out, c("data.table","data.frame")) && nrow(single_out) == nrow(res1) - 1 && ncol(single_out) == 2 && sum(colnames(single_out) %in% c('term', 'correlation')) == 2 && !target_term %in% single_out$term )
})




testthat::test_that("it returns the correct output in case of a multiple terms", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  tmp_terms = c("is", "a")

  mult_out = init$term_associations(Terms =  tmp_terms, keep_terms = NULL, threads = 1, verbose = FALSE)

  testthat::expect_true( sum(unlist(lapply(1:length(mult_out), function(x) inherits(mult_out[[x]], c("data.table","data.frame")) && nrow(mult_out[[x]]) == ncol(res) - 1 && ncol(mult_out[[x]]) == 2 &&

                                             sum(colnames(mult_out[[x]]) %in% c('term', 'correlation')) == 2  && !tmp_terms[x] %in% mult_out[[x]]$term))) == length(tmp_terms) )
})



testthat::test_that("it returns the correct output in case of a multiple terms ( if term-document-matrix is FALSE )", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = FALSE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  tmp_terms = c("is", "a")

  mult_out = init$term_associations(Terms =  tmp_terms, keep_terms = NULL, threads = 1, verbose = FALSE)

  testthat::expect_true( sum(unlist(lapply(1:length(mult_out), function(x) inherits(mult_out[[x]], c("data.table","data.frame")) && nrow(mult_out[[x]]) == nrow(res) - 1 && ncol(mult_out[[x]]) == 2 &&

                                             sum(colnames(mult_out[[x]]) %in% c('term', 'correlation')) == 2  && !tmp_terms[x] %in% mult_out[[x]]$term))) == length(tmp_terms) )
})


#----------------------------------------
# most frequent terms  [ error handling ]
#----------------------------------------


testthat::test_that("in case that the Term_Matrix method is not run in first place it returns an error", {

  init1 = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  testthat::expect_error( init1$most_frequent_terms(keep_terms = NULL, threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the keep_terms parameter is not a numeric value it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$most_frequent_terms(keep_terms = 'NULL', threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the keep_terms parameter is less than 1  it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$most_frequent_terms(keep_terms = 0, threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the threads parameter is less than 1  it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$most_frequent_terms(keep_terms = 1, threads = 0, verbose = FALSE) )
})



testthat::test_that("in case that the verbose parameter is not logical it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$most_frequent_terms(keep_terms = 1, threads = 1, verbose = 'FALSE') )
})



testthat::test_that("in case that either the normalize parameter is not NULL or the tf_idf parameter is TRUE it returns an error", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = T,

                         threads = 1, verbose = FALSE)

  testthat::expect_error( init$most_frequent_terms(keep_terms = 1, threads = 1, verbose = FALSE) )
})



#-------------------------------------
# most frequent terms  [ expect true ]
#-------------------------------------



testthat::test_that("it returns the correct output if the keep_terms parameter is NULL", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  res_freq = init$most_frequent_terms(keep_terms = NULL, threads = 1, verbose = FALSE)

  testthat::expect_true( inherits(res_freq, c("data.table","data.frame")) && nrow(res_freq) == ncol(res) && ncol(res_freq) == 2 && sum(colnames(res_freq) %in% c('term', 'frequency')) == 2 )
})



testthat::test_that("it returns the correct output if the keep_terms parameter is a numeric value", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  tmp_keep = 5

  res_freq = init$most_frequent_terms(keep_terms = tmp_keep, threads = 1, verbose = FALSE)

  testthat::expect_true( inherits(res_freq, c("data.table","data.frame")) && nrow(res_freq) == tmp_keep && ncol(res_freq) == 2 && sum(colnames(res_freq) %in% c('term', 'frequency')) == 2 )
})



testthat::test_that("it returns the correct output if the Term_Matrix_Adjust method is called first", {

  init = sparse_term_matrix$new(vector_data = docs, file_data = NULL, document_term_matrix = TRUE)

  res = init$Term_Matrix(sort_terms = FALSE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                         remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                         min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                         print_every_rows = 100, normalize = NULL, tf_idf = FALSE,

                         threads = 1, verbose = FALSE)

  res1 = init$Term_Matrix_Adjust(sparsity_thresh = 0.85)

  res_freq = init$most_frequent_terms(keep_terms = NULL, threads = 1, verbose = FALSE)

  testthat::expect_true( inherits(res_freq, c("data.table","data.frame")) && nrow(res_freq) == ncol(res1) && ncol(res_freq) == 2 && sum(colnames(res_freq) %in% c('term', 'frequency')) == 2 )
})

