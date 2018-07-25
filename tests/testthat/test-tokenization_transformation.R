

if (.Platform$OS.type == "windows") {

  WRITE_PATH = paste0(getwd(),"\\")

  PATH = paste0(getwd(), path.expand("\\test_text.txt"))
}

if (.Platform$OS.type == "unix") {

  WRITE_PATH = paste0(getwd(),"/")

  PATH = paste0(getwd(), path.expand("/test_text.txt"))
}


n_gram_overlap_lst = c("the_", "erm_", "net_", "_is_", "_anc", "ith_", "_tie", "_to_",
                       "_his", "trol", "ienc", "_myt", "_and", "_rel", "_sev", "ets_",
                       "_in_", "the_", "_sol", "_sys", "_can", "_be_", "_see", "ith_",
                       "the_", "_nak", "_eye", "ese_", "_wer", "_reg", "_by_", "_man",
                       "arly", "_cul", "_as_", "ine_", "_or_", "_as_", "_emi", "_of_",
                       "_dei", "_as_", "enti", "_kno", "_adv", "_hum", "cept", "_of_",
                       "the_", "ets_", "ange", "atin", "_a_", "_num", "_of_", "arat",
                       "cts_", "_in_", "006_", "the_", "_int", "mica", "_uni", "_iau",
                       "_off", "_ado", "_a_", "_res", "inin", "ets_", "_wit", "the_",
                       "_sol", "_sys", "_thi", "init", "_is_", "cont", "_bec", "_it_",
                       "_exc", "_man", "cts_", "_of_", "ary_", "_mas", "_bas", "_on_",
                       "_whe", "_or_", "_wha", "hey_", "bit_", "_alt", "_eig", "_of_",
                       "the_", "ary_", "_bod", "cove", "_bef", "1950", "_rem", "ets_",
                       "_und", "the_", "_mod", "init", "_som", "_cel", "_bod", "_suc",
                       "_as_", "_cer", "_pal", "_jun", "_and", "_ves", "_eac", "_an_",
                       "ect_", "_in_", "the_", "_sol", "aste", "_bel", "_and", "_plu",
                       "the_", "_fir", "_tra", "ect_", "cove", "_tha", "_wer", "_onc",
                       "cons", "ets_", "_by_", "the_", "enti", "_com", "_are", "_no_",
                       "_lon", "_vie", "_as_", "_suc", "the_", "ets_", "_wer", "_tho",
                       "_by_", "_pto", "_to_", "bit_", "arth", "_in_", "defe", "_and",
                       "_epi", "_mot", "_alt", "the_", "_ide", "_tha", "the_", "ets_",
                       "bite", "the_", "_sun", "_had", "_bee", "_sug", "_man", "_tim",
                       "_it_", "_was", "_not", "_unt", "the_", "17th", "_cen", "_tha",
                       "_thi", "iew_", "_was", "_sup", "_by_", "_evi", "_fro", "the_",
                       "_fir", "_tel", "mica", "ions", "erfo", "_by_", "ileo", "ilei",
                       "_at_", "_abo", "the_", "_sam", "ime_", "_by_", "_car", "_ana",
                       "_of_", "_pre", "_obs", "_dat", "_col", "_by_", "_tyc", "_bra",
                       "_joh", "_kep", "_fou", "the_", "ets_", "bits", "_wer", "_not",
                       "_cir", "_but", "_ell", "_as_", "iona", "_too", "_imp", "ers_",
                       "_saw", "_tha", "_lik", "arth", "the_", "ets_", "_rot", "_aro",
                       "_til", "_axe", "_and", "_som", "ared", "_suc", "_fea", "_as_",
                       "_ice", "_cap", "_and", "_sea", "_sin", "the_", "_daw", "_of_",
                       "the_", "_spa", "_age", "_clo", "_obs", "_by_", "_spa", "_pro",
                       "_has", "_fou", "_tha", "arth", "_and", "the_", "_oth", "ets_",
                       "are_", "acte", "_suc", "_as_", "_vol", "_hur", "_tec", "_and",
                       "_eve", "_hyd", "ets_", "_are", "_gen", "ided", "into", "_two",
                       "_mai", "_typ", "_lar", "_low", "_gia", "ets_", "_and", "_sma",
                       "_roc", "_ter", "_und", "_iau", "init", "_the", "_are", "_eig",
                       "ets_", "_in_", "the_", "_sol", "_sys", "_in_", "_ord", "_of_",
                       "asin", "dist", "_fro", "the_", "_sun", "hey_", "_are", "the_",
                       "four", "_ter", "_mer", "_ven", "arth", "_and", "_mar", "hen_",
                       "the_", "four", "_gia", "ets_", "_jup", "satu", "_ura", "_and",
                       "_nep", "_six", "_of_", "the_", "ets_", "_are", "bite", "_by_",
                       "_one", "_or_", "_mor", "_nat", "atel", "__")


text_path = textTinyR::read_characters(PATH, characters = 2280)$data



context('tokenization and transformation')



# cnt_tsts = 1


while(T) {
  
    
  #################
  # error handling
  #################
  
  
  
  testthat::test_that("in case that the object parameter is NULL it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = NULL, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the batches parameter is not a numeric value it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = '4', read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the batches parameter is a numeric value and the path_2folder parameter is an empty character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = 3, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the length of the object parameter is greater than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = c(text_path, text_path), batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  testthat::test_that("in case that the batches parameter is not NULL the object parameter should be a valid path to a file, otherwise return error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = 3, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_PATH, stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the to_lower parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = 'FALSE', to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  testthat::test_that("in case that the to_upper parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = 'FALSE', utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the read_file_delimiter parameter is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = NULL, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the utf_locale parameter is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = NULL, remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the remove_char parameter is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = NULL,
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  
  testthat::test_that("in case that the remove_punctuation_string parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = 'FALSE', remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the remove_punctuation_vector parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = 'FALSE', remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the remove_numbers parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = 'FALSE', trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  testthat::test_that("in case that the trim_token parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = 'FALSE', split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the split_string parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = 'FALSE',
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the split_separator parameter is not a character it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = NULL, remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the remove_stopwords parameter is not either TRUE, FALSE or a character vector of length greater than 0, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = list(), language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the language parameter is invalid it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "invalid", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the min_num_char parameter is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 0, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the max_num_char parameter is less than the min_num_char it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = -Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the stemmer parameter is not one of porter2_stemmer, ngram_sequential or ngram_overlap it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'NULL', min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  testthat::test_that("in case that the min_n_gram parameter is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 0,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the max_n_gram parameter is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
  
                                               max_n_gram = 0, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the min_n_gram is greater than the max_n_gram parameter it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 3,
  
                                               max_n_gram = 2, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the skip_n_gram parameter is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 0, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the skip_distance parameter is less than 0 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = -1, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the n_gram_delimiter parameter is not a character it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = NULL, concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the concat_delimiter parameter is not a character it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = list(), path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the path_2folder parameter does not end in slash it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = getwd(), stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the path_2folder parameter is not a character it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = NULL, stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  testthat::test_that("in case that the stemmer is gram_sequential and the stemmer_ngram is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = "ngram_sequential", min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 0, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the stemmer is gram_sequential and the stemmer_gamma is less than 0.0 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = "ngram_sequential", min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = -0.1,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  testthat::test_that("in case that the stemmer is gram_sequential and the stemmer_truncate is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = "ngram_sequential", min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 0, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  
  testthat::test_that("in case that the stemmer is ngram_sequential and the stemmer_batches is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = "ngram_sequential", min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 0, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  testthat::test_that("in case that the stemmer is ngram_overlap and the stemmer_ngram is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = "ngram_overlap", min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 0, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  testthat::test_that("in case that the number of threads is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 0, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  testthat::test_that("in case that the verbose parameter is not logical it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = 'FALSE')  )
  })
  
  
  testthat::test_that("in case that the vocabulary_path_file parameter is not NULL and it is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = list(), verbose = FALSE)  )
  })
  
  
  
  
  testthat::test_that("in case that the to_lower or to_upper parameter is not TRUE and the language is not english it returns a warning", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_warning( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                                               remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
  
                                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "greek", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
  })
  
  
  
  ###########################################
  # tests for the tokenize_transform_text function
  ###########################################
  
  
  testthat::test_that("the function returns a vector of words if string split is TRUE and the remove_stopwords parameter is FALSE", {
  
    tmp = tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                             remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
  
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes  )
  })
  
  
  
  testthat::test_that("the function returns a vector of words if string split is TRUE and the remove_stopwords parameter is TRUE", {
  
    tmp = tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                             remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = TRUE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    sample_stopwords = c("the", "a", "")
  
    res_stpw = sum(!sample_stopwords %in% tmp$token) == 3
  
    res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation" && res_stpw
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes  )
  })
  
  
  
  # testthat::test_that("the function returns a vector of words if string split is TRUE and the remove_stopwords parameter is a vector of user defined stop-words", {
  # 
  #   sample_stopwords = c("a", "this", "is")
  # 
  #   tmp = tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  # 
  #                            remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  # 
  #                            split_separator = " \r\n\t.,;:()?!//", remove_stopwords = sample_stopwords, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  # 
  #                            max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  # 
  #                            stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  # 
  #   res_stpw = sum(!sample_stopwords %in% tmp$token) == 3
  # 
  #   res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation" && res_stpw
  #   
  #   #-------------------------------------------------------------------- debug tests
  #   cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
  #   
  #   cnt_tsts <<- cnt_tsts + 1
  #   #-------------------------------------------------------------------- 
  #   
  #   testthat::expect_true( res_tes  )
  # })
  
  
  testthat::test_that("the function returns a vector of words if string split is TRUE and stemmer is porter2_stemmer", {
  
    tmp = tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                             remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
  
                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    stem = c('planetari', 'knowledg', 'hydrolog')
  
    res_stem = sum(stem %in% tmp$token) == 3
  
    res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation" && res_stem
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes  )
  })
  
  
  
  
  testthat::test_that("the function returns a vector of words if string split is TRUE and stemmer is ngram_sequential", {
  
    tmp = tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                             remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'ngram_sequential', min_n_gram = 1,
  
                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    stem_seq = c('resolut', 'transneptun', 'pretelesco', 'characterist')
  
    res_stem = sum(stem_seq %in% tmp$token) == 4
  
    res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation" && res_stem
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes  )
  })
  
  
  
  testthat::test_that("the function returns a vector of words if string split is TRUE and stemmer is ngram_overlap", {
  
    tmp = tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                             remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'ngram_overlap', min_n_gram = 1,
  
                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    res_stem = length(which(!n_gram_overlap_lst == tmp$token)) == 0
  
    res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation" && res_stem
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes  )
  })
  
  
  
  
  testthat::test_that("the function reads from a file and returns a vector", {
  
    tmp = tokenize_transform_text(object = PATH, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                             remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
  
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes  )
  })
  
  
  
  testthat::test_that("the function reads from a file and writes to a file", {
  
    tmp = tokenize_transform_text(object = PATH, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                             remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = paste0(WRITE_PATH, "WRITE_FOLDER/"), stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    res_tes = names(tmp) == 'token'  && length(tmp) == 1 && tmp$token == "" && class(tmp) == "tokenization and transformation"
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes )
  })
  
  
  
  
  testthat::test_that("the function reads from a file and writes to a file in case that batches is greater than 1", {
  
    tmp = tokenize_transform_text(object = PATH, batches = 2, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
  
                             remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE,
  
                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
  
                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = " ", path_2folder = paste0(WRITE_PATH, "WRITE_FOLDER/"), stemmer_ngram = 4, stemmer_gamma = 0.0,
  
                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
    res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
    
    #-------------------------------------------------------------------- debug tests
    cat("test-tokenization_transformation.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( res_tes )
  })
  
  break    # exit loop for tests ( count iterations / tests for debugging )
}

