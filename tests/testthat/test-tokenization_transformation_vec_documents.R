

if (.Platform$OS.type == "windows") {

  WRITE_PATH = paste0(getwd(),"\\")

  PATH = paste0(getwd(), path.expand("\\test_text.txt"))
}

if (.Platform$OS.type == "unix") {

  WRITE_PATH = paste0(getwd(),"/")

  PATH = paste0(getwd(), path.expand("/test_text.txt"))
}



text_rows = textTinyR::read_rows(input_file = PATH)$data


context('tokenization and transformation')


# cnt_tsts = 1


while(T) {
  
  #################
  # error handling
  #################
  
  
  
  testthat::test_that("in case that the object parameter is NULL it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = NULL, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  
  })
  
  
  
  testthat::test_that("in case that the object parameter is NULL it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = list(), to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                                         remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                                         split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                                         max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  
  })
  
  
  
  testthat::test_that("in case that the object parameter is NULL it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = c('one sentence'), to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                                         remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                                         split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                                         max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  
  })
  
  
  
  testthat::test_that("in case that the as_token parameter is not boolean it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = c('one sentence'), as_token = 'FALSE', to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                                         remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                                         split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                                         max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  
  })
  
  
  
  testthat::test_that("in case that the to_lower parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = 'FALSE', to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  testthat::test_that("in case that the to_upper parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = 'FALSE', utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  
  
  testthat::test_that("in case that the utf_locale parameter is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = NULL, remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the remove_char parameter is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = NULL,
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  
  testthat::test_that("in case that the remove_punctuation_string parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = 'FALSE', remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the remove_punctuation_vector parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = 'FALSE', remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the remove_numbers parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = 'FALSE', trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  testthat::test_that("in case that the trim_token parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = 'FALSE', split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the split_string parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = 'FALSE',
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the split_separator parameter is not a character it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = NULL, remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the remove_stopwords parameter is not either TRUE, FALSE or a character vector of length greater than 0, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = list(), language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the language parameter is invalid it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "invalid", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the min_num_char parameter is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 0, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the max_num_char parameter is less than the min_num_char it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = -Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the stemmer parameter is not porter2_stemmer it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'NULL', min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  testthat::test_that("in case that the min_n_gram parameter is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 0,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the max_n_gram parameter is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
  
                                               max_n_gram = 0, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the min_n_gram is greater than the max_n_gram parameter it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 3,
  
                                               max_n_gram = 2, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the skip_n_gram parameter is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 0, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the skip_distance parameter is less than 0 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = -1, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the n_gram_delimiter parameter is not a character it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = NULL, concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the concat_delimiter parameter is not a character it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = list(), path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the path_2folder parameter does not end in slash it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = getwd(), threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the path_2folder parameter is not a character it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = NULL, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the number of threads is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 0, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  testthat::test_that("in case that the verbose parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = 'FALSE')  )
  })
  
  
  testthat::test_that("in case that the vocabulary_path_file parameter is not NULL and it is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error(  tokenize_transform_vec_docs(object = text_rows, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = list(), verbose = FALSE)  )
  })
  
  
  
  
  testthat::test_that("in case that the to_lower or to_upper parameter is not TRUE and the language is not english it returns a warning", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_warning(  tokenize_transform_vec_docs(object = text_rows, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                                 remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                                 split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "greek", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                                 max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  #####################################################
  # tests for the  tokenize_transform_vec_docs function
  #####################################################
  
  
  testthat::test_that("the function returns a vector of words if string split is TRUE and the remove_stopwords parameter is FALSE", {
  
    tmp =  tokenize_transform_vec_docs(object = text_rows, as_token = TRUE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                             remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    res_tes = names(tmp) == 'token'  && length(tmp$token) == 5 && is.list(tmp$token) && sum(unlist(lapply(tmp$token, function(x) is.character(x)))) == 5 && class(tmp) == "tokenization and transformation"
  
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes  )
  })
  
  
  
  testthat::test_that("the function returns a vector of words if string split is TRUE and the remove_stopwords parameter is TRUE", {
  
    tmp =  tokenize_transform_vec_docs(object = text_rows, as_token = TRUE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                       remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                                       split_separator = " \r\n\t.,;:()?!//", remove_stopwords = TRUE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                       max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    res_tes = names(tmp) == 'token'  && length(tmp$token) == 5 && is.list(tmp$token) && sum(unlist(lapply(tmp$token, function(x) is.character(x)))) == 5 && class(tmp) == "tokenization and transformation" && tmp$token[[1]][1] == 'term'
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes  )
  })
  
  
  
  testthat::test_that("the function returns a vector of words if string split is TRUE and the remove_stopwords parameter is a vector of user defined stop-words", {
  
    sample_stopwords = c("the", "this", "is")
  
    tmp =  tokenize_transform_vec_docs(object = text_rows, as_token = TRUE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                             remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = sample_stopwords, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    res_tes = names(tmp) == 'token'  && length(tmp$token) == 5 && is.list(tmp$token) && sum(unlist(lapply(tmp$token, function(x) is.character(x)))) == 5 && class(tmp) == "tokenization and transformation" && tmp$token[[1]][1] == 'term'
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes  )
  })
  
  
  testthat::test_that("the function returns a vector of words if string split is TRUE and stemmer is porter2_stemmer", {
  
    tmp =  tokenize_transform_vec_docs(object = text_rows, as_token = TRUE, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                             remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = TRUE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
  
                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    res_tes = names(tmp) == 'token'  && length(tmp$token) == 5 && is.list(tmp$token) && sum(unlist(lapply(tmp$token, function(x) is.character(x)))) == 5 && class(tmp) == "tokenization and transformation" && tmp$token[[2]][1] == 'planetari'
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes  )
  })
  
  
  
  testthat::test_that("the function returns a vector of words if string split is TRUE and the remove_stopwords parameter is TRUE, AND as_token = FALSE", {
  
    tmp =  tokenize_transform_vec_docs(object = text_rows, as_token = F, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                       remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                                       split_separator = " \r\n\t.,;:()?!//", remove_stopwords = TRUE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                       max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    res_tes = names(tmp) == 'token'  && length(tmp$token) == 5 && is.vector(tmp$token) && class(tmp) == "tokenization and transformation" && substring(tmp$token[1], 1, 4) == 'term'
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes  )
  })
  
  
  
  testthat::test_that("the function returns a vector of words if string split is TRUE and the remove_stopwords parameter is a vector of user defined stop-words, AND as_token = FALSE", {
  
    sample_stopwords = c("the", "this", "is")
  
    tmp =  tokenize_transform_vec_docs(object = text_rows, as_token = F, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                       remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                                       split_separator = " \r\n\t.,;:()?!//", remove_stopwords = sample_stopwords, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                       max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    res_tes = names(tmp) == 'token'  && length(tmp$token) == 5 && is.vector(tmp$token) && class(tmp) == "tokenization and transformation" && substring(tmp$token[1], 1, 4) == 'term'
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes  )
  })
  
  
  testthat::test_that("the function returns a vector of words if string split is TRUE and stemmer is porter2_stemmer, AND as_token = FALSE", {
  
    tmp =  tokenize_transform_vec_docs(object = text_rows, as_token = F, to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                       remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                                       split_separator = " \r\n\t.,;:()?!//", remove_stopwords = TRUE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
  
                                       max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    res_tes = names(tmp) == 'token'  && length(tmp$token) == 5 && is.vector(tmp$token) && class(tmp) == "tokenization and transformation" && substring(tmp$token[2], 1, 9) == 'planetari'
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation_vec_documents.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes  )
  })
  
  break    # exit loop for tests ( count iterations / tests for debugging )
}
