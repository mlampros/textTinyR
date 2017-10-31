

if (.Platform$OS.type == "windows") {

  WRITE_PATH = paste0(getwd(),"\\")

  VOCAB_PARSER = paste0(getwd(),"\\VOCAB_PARSER\\")

  PATH = paste0(getwd(), path.expand("\\demo_text.xml"))
}

if (.Platform$OS.type == "unix") {

  WRITE_PATH = paste0(getwd(),"/")

  VOCAB_PARSER = paste0(getwd(),"/VOCAB_PARSER/")

  PATH = paste0(getwd(), path.expand("/demo_text.xml"))
}




context('vocabulary parser and bytes converter')


#-------------------------------------
# vocabulary parser [ error handling ]
#-------------------------------------


testthat::test_that("in case that the input_path_file parameter is not valid it returns an error", {

  if (.Platform$OS.type == "windows") {

    tmp_PATH = paste0("\\wrong_input_path","\\")
  }

  if (.Platform$OS.type == "unix") {

    tmp_PATH = paste0("/wrong_input_path","/")
  }

  testthat::expect_error( vocabulary_parser(input_path_file = tmp_PATH, start_query = NULL, end_query = NULL, vocabulary_path_file = NULL, min_lines = 1, trimmed_line = FALSE,

                                            max_num_char = Inf, remove_char = "", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE,

                                            language = "english", min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                            threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the start_query parameter is not a character string it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = NULL, end_query = NULL, vocabulary_path_file = NULL, min_lines = 1, trimmed_line = FALSE,

                                            max_num_char = Inf, remove_char = "", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE,

                                            language = "english", min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                            threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the end_query parameter is not a character string it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = NULL, vocabulary_path_file = NULL, min_lines = 1, trimmed_line = FALSE,

                                            max_num_char = Inf, remove_char = "", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE,

                                            language = "english", min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                            threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the vocabulary_path_file parameter is NULL it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = NULL, min_lines = 1, trimmed_line = FALSE,

                                            max_num_char = Inf, remove_char = "", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE,

                                            language = "english", min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                            threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the vocabulary_path_file parameter is not a character string it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = list(), min_lines = 1, trimmed_line = FALSE,

                                            max_num_char = Inf, remove_char = "", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE,

                                            language = "english", min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                            threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the min_lines parameter is less than 1 it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 0, trimmed_line = FALSE,

                                            max_num_char = Inf, remove_char = "", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE,

                                            language = "english", min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                            threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the trimmed_line parameter is not logical it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = 'FALSE',

                                            max_num_char = Inf, remove_char = "", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE,

                                            language = "english", min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                            threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the to_lower parameter is not logical it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = 'FALSE', to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the to_upper parameter is not logical it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = 'FALSE', utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the utf_locale parameter is not a character string it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = NULL, max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the remove_char parameter is not a character string it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = NULL, remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the remove_punctuation_string parameter is not logical it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = 'FALSE', remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the remove_punctuation_vector parameter is not logical it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = 'FALSE',

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the remove_numbers parameter is not logical it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = 'FALSE', trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the trim_token parameter is not logical it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = 'FALSE', split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the split_string parameter is not logical it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = 'FALSE', split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the split_separator parameter is not a character it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = NULL, remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the remove_stopwords parameter is not either TRUE, FALSE or a character vector of length greater than 0, it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = list(), language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the language parameter is invalid, it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "invalid",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the min_num_char parameter is less than 1 it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 0, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the max_num_char parameter is less than the min_num_char it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = -Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the stemmer parameter is not porter2_stemmer it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = 'NULL', min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the min_n_gram parameter is less than 1 it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 0, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})

testthat::test_that("in case that the max_n_gram parameter is less than 1 it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 0, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the min_n_gram is greater than the max_n_gram parameter it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 3, max_n_gram = 2, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the skip_n_gram parameter is less than 1 it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 0, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the skip_distance parameter is less than 0 it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = -1, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the n_gram_delimiter parameter is not a character it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = NULL, threads = 1, verbose = FALSE) )
})



testthat::test_that("in case that the number of threads is less than 1 it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 0, verbose = FALSE) )
})



testthat::test_that("in case that the verbose parameter is not logical it returns an error", {

  testthat::expect_error( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = 'FALSE') )
})



testthat::test_that("in case that the verbose parameter is not logical it returns an error", {

  testthat::expect_warning( vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                                            to_lower = TRUE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                            remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "greek",

                                            min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE) )
})


#----------------------------------
# vocabulary parser [ expect true ]
#----------------------------------


testthat::test_that("it returns a single vocabulary file", {

  res = vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                          to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                          remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = TRUE, language = "english",

                          min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE)

  lst = length(list.files(VOCAB_PARSER))

  testthat::expect_true( lst == 1 )
})



testthat::test_that("it returns a single vocabulary file with user defined stopwords", {

  res = vocabulary_parser(input_path_file = PATH, start_query = '<structure', end_query = '</structure>', vocabulary_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), min_lines = 1, trimmed_line = FALSE,

                          to_lower = FALSE, to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                          remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = c("a", "this", "is"), language = "english",

                          min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", threads = 1, verbose = FALSE)

  lst = length(list.files(VOCAB_PARSER))

  testthat::expect_true( lst == 1 )
})



#------------------------------------
# bytes converter [ error handling ]
#------------------------------------


testthat::test_that("in case that the input_path_file parameter is not a valid path to a file it returns an error", {

  testthat::expect_error( bytes_converter(input_path_file = NULL, unit = "MB" ) )
})


testthat::test_that("in case that the unit parameter is not valid it returns an error", {

  testthat::expect_error( bytes_converter(input_path_file = PATH, unit = "invalid" ) )
})


#--------------------------------
# bytes converter [ expect true ]
#--------------------------------


testthat::test_that("in case that the unit parameter is not valid it returns an error", {

  res = bytes_converter(input_path_file = PATH, unit = "MB" )

  testthat::expect_true( is.numeric(res)  )
})



#------------------------------------
# text file parser [ error handling ]
#------------------------------------


testthat::test_that("in case that the input_path_file parameter is not a valid path to a file it returns an error", {

  testthat::expect_error( text_file_parser(input_path_file = NULL, output_path_file = NULL, start_query = NULL, end_query = NULL, min_lines = 1, trimmed_line = FALSE, verbose = FALSE) )
})


testthat::test_that("in case that the output_path_file parameter is not a valid path to a file it returns an error", {

  testthat::expect_error( text_file_parser(input_path_file = PATH, output_path_file = NULL, start_query = NULL, end_query = NULL, min_lines = 1, trimmed_line = FALSE, verbose = FALSE) )
})


testthat::test_that("in case that the output_path_file parameter is not NULL and not a character string valid path to a file it returns an error", {

  testthat::expect_error( text_file_parser(input_path_file = PATH, output_path_file = list(), start_query = NULL, end_query = NULL, min_lines = 1, trimmed_line = FALSE, verbose = FALSE) )
})


testthat::test_that("in case that the start_query parameter is not a character string it returns an error", {

  testthat::expect_error( text_file_parser(input_path_file = PATH, output_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), start_query = NULL, end_query = NULL, min_lines = 1, trimmed_line = FALSE, verbose = FALSE) )
})


testthat::test_that("in case that the end_query parameter is not a character string it returns an error", {

  testthat::expect_error( text_file_parser(input_path_file = PATH, output_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), start_query = '<structure', end_query = NULL, min_lines = 1, trimmed_line = FALSE, verbose = FALSE) )
})


testthat::test_that("in case that the min_lines parameter is less than 1 it returns an error", {

  testthat::expect_error( text_file_parser(input_path_file = PATH, output_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), start_query = '<structure', end_query = '</structure>', min_lines = 0, trimmed_line = FALSE, verbose = FALSE) )
})


testthat::test_that("in case that the trimmed_line parameter is not logical it returns an error", {

  testthat::expect_error( text_file_parser(input_path_file = PATH, output_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), start_query = '<structure', end_query = '</structure>', min_lines = 1, trimmed_line = 'FALSE', verbose = FALSE) )
})



testthat::test_that("in case that the verbose parameter is not logical it returns an error", {

  testthat::expect_error( text_file_parser(input_path_file = PATH, output_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), start_query = '<structure', end_query = '</structure>', min_lines = 1, trimmed_line = FALSE, verbose = 'FALSE') )
})


#---------------------------------
# text file parser [ expect true ]
#---------------------------------

testthat::test_that("it returns a single pre-processed file", {

  res = text_file_parser(input_path_file = PATH, output_path_file = paste0(VOCAB_PARSER, 'vocab_file.txt'), start_query = '<structure', end_query = '</structure>', min_lines = 1, trimmed_line = FALSE, verbose = FALSE)

  lst = length(list.files(VOCAB_PARSER))

  testthat::expect_true( lst == 1 )
})



#---------------------------------
# dice distance [ error handling ]
#---------------------------------


testthat::test_that("in case that the word1 parameter is not a character string it returns an error", {

  w1 = NULL
  w2 = 'word_one'
  n = 2

  testthat::expect_error( dice_distance(w1, w2, n)  )
})



testthat::test_that("in case that the word2 parameter is not a character string it returns an error", {

  w1 = 'word_one'
  w2 = NULL
  n = 2

  testthat::expect_error( dice_distance(w1, w2, n)  )
})




testthat::test_that("in case that the n_grams parameter is not numeric it returns an error", {

  w1 = 'word_one'
  w2 = 'word_two'
  n = NULL

  testthat::expect_error( dice_distance(w1, w2, n)  )
})


#------------------------------
# dice distance [ expect true ]
#------------------------------


testthat::test_that("in case that the n_grams parameter is not numeric it returns an error", {

  w1 = 'word_one'
  w2 = 'word_two'
  n = 2

  testthat::expect_true( is.numeric(dice_distance(w1, w2, n))  )
})



#----------------------------------------
# levenshtein distance [ error handling ]
#----------------------------------------


testthat::test_that("in case that the word1 parameter is not a character string it returns an error", {

  w1 = NULL
  w2 = 'word_two'

  testthat::expect_error( levenshtein_distance(w1, w2) )
})


testthat::test_that("in case that the word2 parameter is not a character string it returns an error", {

  w1 = 'word_two'
  w2 = NULL

  testthat::expect_error( levenshtein_distance(w1, w2) )
})


#-------------------------------------
# levenshtein distance [ expect true ]
#-------------------------------------


testthat::test_that("in case that the word2 parameter is not a character string it returns an error", {

  w1 = 'word_two'
  w2 = 'word_one'

  testthat::expect_true( is.numeric( levenshtein_distance(w1, w2) ) )
})



#-----------------------------------
# cosine distance [ error handling ]
#-----------------------------------


testthat::test_that("in case that the sentence1 parameter is not a character string it returns an error", {

  s1 = NULL
  s2 = 'sentence with two words'
  sep = " "

  testthat::expect_error( cosine_distance(s1, s2, split_separator = sep) )
})


testthat::test_that("in case that the sentence2 parameter is not a character string it returns an error", {

  s1 = 'sentence with two words'
  s2 = NULL
  sep = " "

  testthat::expect_error( cosine_distance(s1, s2, split_separator = sep) )
})


testthat::test_that("in case that the split_separator parameter is not a character string it returns an error", {

  s1 = 'sentence with two words'
  s2 = 'sentence with three words'
  sep = NULL

  testthat::expect_error( cosine_distance(s1, s2, split_separator = sep) )
})


#--------------------------------
# cosine distance [ expect true ]
#--------------------------------


testthat::test_that("in case that the split_separator parameter is not a character string it returns an error", {

  s1 = 'sentence with two words'
  s2 = 'sentence with three words'
  sep = " "

  testthat::expect_true( is.numeric(cosine_distance(s1, s2, split_separator = sep)) )
})

