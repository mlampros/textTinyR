
if (.Platform$OS.type == "windows") {
  
  WRITE_PATH = paste0(getwd(),"\\WRITE_BIG_TEXT\\")
  
  WRITE_PARSER = paste0(getwd(),"\\WRITE_PARSER\\")
  
  WRITE_TOKEN = paste0(getwd(),"\\WRITE_TOKEN\\")
  
  WRITE_VOCAB = paste0(getwd(),"\\WRITE_VOCAB\\")
  
  INVALID_PATH = paste0(getwd(),"\\WRITE_VOCAB")
  
  WRITE_VOCAB_single = paste0(getwd(),"\\WRITE_VOCAB_single\\vocab.txt")
  
  PATH = paste0(getwd(), path.expand("\\demo_text.xml"))
}

if (.Platform$OS.type == "unix") {
  
  WRITE_PATH = paste0(getwd(),"/WRITE_BIG_TEXT/")
  
  WRITE_PARSER = paste0(getwd(),"/WRITE_PARSER/")
  
  WRITE_TOKEN = paste0(getwd(),"/WRITE_TOKEN/")
  
  WRITE_VOCAB = paste0(getwd(),"/WRITE_VOCAB/")
  
  INVALID_PATH = paste0(getwd(),"/WRITE_VOCAB")
  
  WRITE_VOCAB_single = paste0(getwd(),"/WRITE_VOCAB_single/vocab.txt")
  
  PATH = paste0(getwd(), path.expand("/demo_text.xml"))
}

btch = 2



context('big tokenize transform')



#-------------------------------------------
# initialization function [ error handling ]
#-------------------------------------------


testthat::test_that("in case that the min_lines parameter is less than 1 it returns an error", {
  
  testthat::expect_error( big_tokenize_transform$new(verbose = 'FALSE') )
})



#---------------------------------------
# text file splitter  [ error handling ]
#---------------------------------------

testthat::test_that("in case that the input_path_file parameter is not valid it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  if (.Platform$OS.type == "windows") {
    
    tmp_PATH = paste0("\\wrong_input_path","\\")
  }
  
  if (.Platform$OS.type == "unix") {
    
    tmp_PATH = paste0("/wrong_input_path","/")
  }
  
  testthat::expect_error( init$big_text_splitter(input_path_file = tmp_PATH, output_path_folder = WRITE_PATH, end_query = "</structure>", batches = 2, trimmed_line = FALSE) )
})



testthat::test_that("in case that the output_path_folder parameter is NULL it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_splitter(input_path_file = PATH, output_path_folder = NULL, end_query = "</structure>", batches = 2, trimmed_line = FALSE) )
})



testthat::test_that("in case that the output_path_folder parameter is not a character string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_splitter(input_path_file = PATH, output_path_folder = list(), end_query = "</structure>", batches = 2, trimmed_line = FALSE) )
})


testthat::test_that("in case that the output_path_folder parameter does not end in slash it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  if (.Platform$OS.type == "windows") {
    
    tmp_PATH = "\\wrong_input_path"
  }
  
  if (.Platform$OS.type == "unix") {
    
    tmp_PATH = "/wrong_input_path"
  }
  
  testthat::expect_error( init$big_text_splitter(input_path_file = PATH, output_path_folder = tmp_PATH, end_query = "</structure>", batches = 2, trimmed_line = FALSE) )
})


testthat::test_that("in case that the batches parameter is not numeric it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_splitter(input_path_file = PATH, output_path_folder = WRITE_PATH, end_query = "</structure>", batches = NULL, trimmed_line = FALSE) )
})


testthat::test_that("in case that the batches parameter is less than 2 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_splitter(input_path_file = PATH, output_path_folder = WRITE_PATH, end_query = "</structure>", batches = 1, trimmed_line = FALSE) )
})


testthat::test_that("in case that the trimmed_line parameter is not logical it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_splitter(input_path_file = PATH, output_path_folder = WRITE_PATH, end_query = "</structure>", batches = 2, trimmed_line = 'FALSE') )
})


testthat::test_that("in case that the end_query parameter is not either NULL or a character string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_splitter(input_path_file = PATH, output_path_folder = WRITE_PATH, end_query = list(), batches = 2, trimmed_line = FALSE) )
})



#---------------------------------------
# text file splitter  [ expect true ]
#---------------------------------------


testthat::test_that("it splits the data in two text files and saves them in the correct folder", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  res = init$big_text_splitter(input_path_file = PATH, output_path_folder = WRITE_PATH, end_query = "</structure>", batches = btch, trimmed_line = FALSE)
  
  len = length(list.files(WRITE_PATH))
  
  testthat::expect_true( len == btch )
})



testthat::test_that("it splits the data in two text files and saves them in the correct folder if the end_query is NULL", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  res = init$big_text_splitter(input_path_file = PATH, output_path_folder = WRITE_PATH, end_query = NULL, batches = btch, trimmed_line = FALSE)
  
  len = length(list.files(WRITE_PATH))
  
  testthat::expect_true( len == btch )
})



#---------------------------------------
# text folder parser  [ error handling ]
#---------------------------------------



testthat::test_that("in case that the input_path_folder parameter is NULL it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_parser(input_path_folder = NULL, output_path_folder = WRITE_PARSER, start_query = "<structure", end_query = "</structure>", min_lines = 1, trimmed_line = TRUE) )
})


testthat::test_that("in case that the input_path_folder parameter is not a character string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_parser(input_path_folder = list(), output_path_folder = WRITE_PARSER, start_query = "<structure", end_query = "</structure>", min_lines = 1, trimmed_line = TRUE) )
})


testthat::test_that("in case that the input_path_folder parameter does not end in slash it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  if (.Platform$OS.type == "windows") {
    
    tmp_PATH = "\\wrong_input_path"
  }
  
  if (.Platform$OS.type == "unix") {
    
    tmp_PATH = "/wrong_input_path"
  }
  
  testthat::expect_error( init$big_text_parser(input_path_folder = tmp_PATH, output_path_folder = WRITE_PARSER, start_query = "<structure", end_query = "</structure>", min_lines = 1, trimmed_line = TRUE) )
})



testthat::test_that("in case that the output_path_folder parameter is NULL it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_parser(input_path_folder = WRITE_PATH, output_path_folder = NULL, start_query = "<structure", end_query = "</structure>", min_lines = 1, trimmed_line = TRUE) )
})



testthat::test_that("in case that the output_path_folder parameter is not a character string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_parser(input_path_folder = WRITE_PATH, output_path_folder = list(), start_query = "<structure", end_query = "</structure>", min_lines = 1, trimmed_line = TRUE) )
})


testthat::test_that("in case that the output_path_folder parameter does not end in slash it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  if (.Platform$OS.type == "windows") {
    
    tmp_PATH = "\\wrong_input_path"
  }
  
  if (.Platform$OS.type == "unix") {
    
    tmp_PATH = "/wrong_input_path"
  }
  
  testthat::expect_error( init$big_text_parser(input_path_folder = WRITE_PATH, output_path_folder = tmp_PATH, start_query = "<structure", end_query = "</structure>", min_lines = 1, trimmed_line = TRUE) )
})



testthat::test_that("in case that the start_query parameter is not a character string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_parser(input_path_folder = WRITE_PATH, output_path_folder = WRITE_PARSER, start_query = NULL, end_query = "</structure>", min_lines = 1, trimmed_line = TRUE) )
})



testthat::test_that("in case that the end_query parameter is not a character string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_parser(input_path_folder = WRITE_PATH, output_path_folder = WRITE_PARSER, start_query = "<structure", end_query = NULL, min_lines = 1, trimmed_line = TRUE) )
})



testthat::test_that("in case that the min_lines parameter is less than 1 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_parser(input_path_folder = WRITE_PATH, output_path_folder = WRITE_PARSER, start_query = "<structure", end_query = "</structure>", min_lines = 0, trimmed_line = TRUE) )
})

 

testthat::test_that("in case that the trimmed_line parameter is not logical it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_parser(input_path_folder = WRITE_PATH, output_path_folder = WRITE_PARSER, start_query = "<structure", end_query = "</structure>", min_lines = 1, trimmed_line = 'TRUE') )
})



#---------------------------------------
# text folder parser  [ expect true ]
#---------------------------------------


testthat::test_that("it pre-process the two text files and saves two new files to another folder", {
  
  init = big_tokenize_transform$new(verbose = FALSE)

  res = init$big_text_parser(input_path_folder = WRITE_PATH, output_path_folder = WRITE_PARSER, start_query = "<structure", end_query = "</structure>", min_lines = 1, trimmed_line = TRUE)
  
  len = length(list.files(WRITE_PARSER))
  
  testthat::expect_true( len == btch )
})



#------------------------------------------
# text folder tokenizer  [ error handling ]
#------------------------------------------


testthat::test_that("in case that the input_path_folder parameter is NULL it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)

  testthat::expect_error( init$big_text_tokenizer(input_path_folder = NULL, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "",
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL) )
})


testthat::test_that("in case that the input_path_folder parameter is not a character string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = list(), batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "",
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL) )
})


testthat::test_that("in case that the input_path_folder parameter does not end in slash it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  if (.Platform$OS.type == "windows") {
    
    tmp_PATH = "\\wrong_input_path"
  }
  
  if (.Platform$OS.type == "unix") {
    
    tmp_PATH = "/wrong_input_path"
  }
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = tmp_PATH, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "",
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the batches parameter is not numeric it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "",
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the batches parameter is less than 2 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 1, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "",
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the read_file_delimiter parameter is not a character string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = NULL, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "",
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the path_2folder parameter is NULL it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = NULL,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the path_2folder parameter is an empty string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "",
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the path_2folder parameter is not a character string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = list(),
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the path_2folder parameter does not end in slash it returns an error", {

  if (.Platform$OS.type == "windows") {
    
    tmp_PATH = "\\wrong_input_path"
  }
  
  if (.Platform$OS.type == "unix") {
    
    tmp_PATH = "/wrong_input_path"
  }
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = tmp_PATH,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the to_lower parameter is not logical it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = 'FALSE', to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the to_upper parameter is not logical it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = 'FALSE', utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the remove_char parameter is not a character string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = NULL,
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the utf_locale parameter is not a character string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = NULL, remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})




testthat::test_that("in case that the remove_punctuation_string parameter is not logical it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = 'FALSE', remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the remove_punctuation_vector parameter is not logical it returns an error", {
  
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = 'FALSE', remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the remove_numbers parameter is not logical it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = 'FALSE', trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the trim_token parameter is not logical it returns an error", {
  
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = 'FALSE', split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the split_string parameter is not logical it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = 'FALSE',
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the split_separator parameter is not a character it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = NULL, remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the remove_stopwords parameter is not either TRUE, FALSE or a character vector of length greater than 0, it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = list(), language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the language parameter is invalid, it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "invalid", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the min_num_char parameter is less than 1 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 0, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the max_num_char parameter is less than the min_num_char it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = -Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the stemmer parameter is not one of porter2_stemmer, ngram_sequential or ngram_overlap it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'NULL',
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the min_n_gram parameter is less than 1 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 0, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})

testthat::test_that("in case that the max_n_gram parameter is less than 1 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 0, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the min_n_gram is greater than the max_n_gram parameter it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 3, max_n_gram = 2, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the skip_n_gram parameter is less than 1 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 0, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the skip_distance parameter is less than 0 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = -1, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the n_gram_delimiter parameter is not a character it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = NULL, concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the concat_delimiter parameter is not either NULL or a character it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = list(), path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})




testthat::test_that("in case that the stemmer is gram_sequential and the stemmer_ngram is less than 1 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'ngram_sequential',
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 0, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the stemmer is gram_sequential and the stemmer_gamma is less than 0.0 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'ngram_sequential',
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 1, stemmer_gamma = -0.1, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the stemmer is gram_sequential and the stemmer_truncate is less than 1 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'ngram_sequential',
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 1, stemmer_gamma = 0.0, stemmer_truncate = 0, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})




testthat::test_that("in case that the stemmer is ngram_sequential and the stemmer_batches is less than 1 it returns an error", {
  
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'ngram_sequential',
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 1, stemmer_gamma = 0.0, stemmer_truncate = 1, stemmer_batches = 0, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})


testthat::test_that("in case that the stemmer is ngram_overlap and the stemmer_ngram is less than 1 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'ngram_overlap',
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 0, stemmer_gamma = 0.0, stemmer_truncate = 1, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the number of threads is less than 1 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'ngram_overlap',
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 1, stemmer_gamma = 0.0, stemmer_truncate = 1, stemmer_batches = 1, threads = 0, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the save_2single_file parameter is not logical it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'ngram_overlap',
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 1, stemmer_gamma = 0.0, stemmer_truncate = 1, stemmer_batches = 1, threads = 1, save_2single_file = 'FALSE',
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})




testthat::test_that("in case that the increment_batch_nr parameter is less than 0 returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'ngram_overlap',
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 1, stemmer_gamma = 0.0, stemmer_truncate = 1, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = -1, vocabulary_path_folder = NULL)  )
})



testthat::test_that("in case that the vocabulary_path_folder parameter is not either NULL or a character string returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'ngram_overlap',
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 1, stemmer_gamma = 0.0, stemmer_truncate = 1, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = list())  )
})




testthat::test_that("in case that the vocabulary_path_folder parameter does not end in slash it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "greek", min_num_char = 1, max_num_char = Inf, stemmer = 'ngram_overlap',
                                                  
                                                  min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                  
                                                  stemmer_ngram = 1, stemmer_gamma = 0.0, stemmer_truncate = 1, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                  
                                                  increment_batch_nr = 1, vocabulary_path_folder = INVALID_PATH)  )
})



testthat::test_that("in case that the language is not english and the to_lower or to_upper parameter is TRUE it returns a warning", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_warning( init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = 2, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                                    
                                                    remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,
                                                    
                                                    split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "greek", min_num_char = 1, max_num_char = Inf, stemmer = 'ngram_overlap',
                                                    
                                                    min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                                    
                                                    stemmer_ngram = 1, stemmer_gamma = 0.0, stemmer_truncate = 1, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                                    
                                                    increment_batch_nr = 1, vocabulary_path_folder = NULL)  )
})



#---------------------------------------
# text folder tokenizer  [ expect true ]
#---------------------------------------


testthat::test_that("the function saves the batches in the WRITE_TOKEN folder", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  btch_tok = 2
  
  res = init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = btch_tok, read_file_delimiter = "\n", to_lower = FALSE, to_upper = TRUE, utf_locale = "", remove_char = "",
                                
                                remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE,
                                
                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = TRUE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                
                                min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                
                                stemmer_ngram = 1, stemmer_gamma = 0.0, stemmer_truncate = 1, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                
                                increment_batch_nr = 1, vocabulary_path_folder = NULL)
  
  len = length(list.files(WRITE_TOKEN))
  
  testthat::expect_true( len == btch * btch_tok  )
})



testthat::test_that("the function saves the batches in the WRITE_TOKEN folder with user defined stopwords", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  btch_tok = 2
  
  res = init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = btch_tok, read_file_delimiter = "\n", to_lower = FALSE, to_upper = TRUE, utf_locale = "", remove_char = "",
                                
                                remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE,
                                
                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = c('a', 'this', 'is'), language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                
                                min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                
                                stemmer_ngram = 1, stemmer_gamma = 0.0, stemmer_truncate = 1, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                
                                increment_batch_nr = 1, vocabulary_path_folder = NULL)
  
  len = length(list.files(WRITE_TOKEN))
  
  testthat::expect_true( len == btch * btch_tok  )
})



testthat::test_that("the function saves the batches in the WRITE_TOKEN folder AND the vocabulary in the WRITE_VOCAB folder", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  btch_tok = 2
  
  res = init$big_text_tokenizer(input_path_folder = WRITE_PARSER, batches = btch_tok, read_file_delimiter = "\n", to_lower = FALSE, to_upper = TRUE, utf_locale = "", remove_char = "",
                                
                                remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = TRUE,
                                
                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,
                                
                                min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_TOKEN,
                                
                                stemmer_ngram = 1, stemmer_gamma = 0.0, stemmer_truncate = 1, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,
                                
                                increment_batch_nr = 1, vocabulary_path_folder = WRITE_VOCAB)
  
  len_tok = length(list.files(WRITE_TOKEN))
  
  len_vocab = length(list.files(WRITE_VOCAB))
  
  testthat::expect_true( len_tok == btch * btch_tok && len_vocab == btch_tok )
})



#------------------------------------------------------------
# vocabulary counts from a folder of files [ error handling ]
#------------------------------------------------------------


testthat::test_that("in case that the input_path_folder parameter is NULL it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$vocabulary_accumulator(input_path_folder = NULL, vocabulary_path_file = NULL, max_num_chars = 100) )
})


testthat::test_that("in case that the input_path_folder parameter is not a character string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$vocabulary_accumulator(input_path_folder = list(), vocabulary_path_file = NULL, max_num_chars = 100) )
})


testthat::test_that("in case that the input_path_folder parameter does not end in slash it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  if (.Platform$OS.type == "windows") {
    
    tmp_PATH = "\\wrong_input_path"
  }
  
  if (.Platform$OS.type == "unix") {
    
    tmp_PATH = "/wrong_input_path"
  }
  
  testthat::expect_error( init$vocabulary_accumulator(input_path_folder = tmp_PATH, vocabulary_path_file = NULL, max_num_chars = 100) )
})



testthat::test_that("in case that the vocabulary_path_file parameter is NULL it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$vocabulary_accumulator(input_path_folder = WRITE_TOKEN, vocabulary_path_file = NULL, max_num_chars = 100) )
})



testthat::test_that("in case that the vocabulary_path_file parameter is not a character string it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$vocabulary_accumulator(input_path_folder = WRITE_TOKEN, vocabulary_path_file = list(), max_num_chars = 100) )
})


testthat::test_that("in case that the max_num_chars parameter is less than 1 it returns an error", {
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  testthat::expect_error( init$vocabulary_accumulator(input_path_folder = WRITE_TOKEN, vocabulary_path_file = WRITE_VOCAB_single, max_num_chars = 0) )
})


#---------------------------------------------------------
# vocabulary counts from a folder of files [ expect true ]
#---------------------------------------------------------


testthat::test_that("it returns a single vocabulary file", {
  
  if (.Platform$OS.type == "windows") {

    WRITE_VOCAB_single_folder = paste0(getwd(),"\\WRITE_VOCAB_single\\")
  }
  
  if (.Platform$OS.type == "unix") {

    WRITE_VOCAB_single_folder = paste0(getwd(),"/WRITE_VOCAB_single/")
  }
  
  init = big_tokenize_transform$new(verbose = FALSE)
  
  res = init$vocabulary_accumulator(input_path_folder = WRITE_VOCAB, vocabulary_path_file = WRITE_VOCAB_single, max_num_chars = 100)
  
  len_vocab = length(list.files(WRITE_VOCAB_single_folder))
  
  testthat::expect_true( len_vocab == 1 )
})






