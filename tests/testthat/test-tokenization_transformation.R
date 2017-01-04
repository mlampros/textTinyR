

if (.Platform$OS.type == "windows") {
  
  WRITE_PATH = paste0(getwd(),"\\")
  
  PATH = paste0(getwd(), path.expand("\\test_text.txt"))
}

if (.Platform$OS.type == "unix") {
  
  WRITE_PATH = paste0(getwd(),"/")
  
  PATH = paste0(getwd(), path.expand("/test_text.txt"))
}


n_gram_overlap_lst = c("the_", "erm_", "net_", "_is_", "ent_", "ith_", "_tie", "_to_", 
                       "ory_", "logy", "ence", "logy", "_and", "gion", "ral_", "ets_", 
                       "_in_", "the_", "lar_", "stem", "_can", "_be_", "_see", "ith_", 
                       "the_", "aked", "_eye", "ese_", "_wer", "ded_", "_by_", "_man", 
                       "arly", "res_", "_as_", "ine_", "_or_", "_as_", "ies_", "_of_", 
                       "ies_", "_as_", "fic_", "dge_", "ced_", "man_", "ion_", "_of_", 
                       "the_", "ets_", "ged_", "ing_", "_a_", "ber_", "_of_", "ate_", 
                       "cts_", "_in_", "006_", "the_", "nal_", "cal_", "ion_", "_iau", 
                       "ally", "pted", "_a_", "ion_", "ing_", "ets_", "hin_", "the_", 
                       "lar_", "stem", "_thi", "ion_", "_is_", "ial_", "ause", "_it_", 
                       "des_", "_man", "cts_", "_of_", "ary_", "_mas", "ased", "_on_", 
                       "ere_", "_or_", "_wha", "hey_", "bit_", "ugh_", "ght_", "_of_", 
                       "the_", "ary_", "dies", "ered", "fore", "1950", "ain_", "ets_", 
                       "der_", "the_", "dern", "ion_", "_som", "ial_", "dies", "_suc", 
                       "_as_", "eres", "las_", "_jun", "_and", "esta", "_eac", "_an_", 
                       "ect_", "_in_", "the_", "lar_", "oid_", "_bel", "_and", "luto", 
                       "the_", "irst", "ian_", "ect_", "ered", "_tha", "_wer", "_onc", 
                       "ered", "ets_", "_by_", "the_", "fic_", "ity_", "_are", "_no_", 
                       "ger_", "ewed", "_as_", "_suc", "the_", "ets_", "_wer", "ght_", 
                       "_by_", "emy_", "_to_", "bit_", "arth", "_in_", "ent_", "_and", 
                       "cle_", "ions", "ugh_", "the_", "_ide", "_tha", "the_", "ets_", 
                       "ited", "the_", "_sun", "_had", "_bee", "sted", "_man", "imes", 
                       "_it_", "_was", "_not", "ntil", "the_", "17th", "tury", "_tha", 
                       "_thi", "iew_", "_was", "rted", "_by_", "ence", "_fro", "the_", 
                       "irst", "opic", "cal_", "ions", "med_", "_by_", "ileo", "ilei", 
                       "_at_", "bout", "the_", "_sam", "ime_", "_by_", "eful", "sis_", 
                       "_of_", "opic", "ion_", "_dat", "cted", "_by_", "cho_", "ahe_", 
                       "nes_", "ler_", "ound", "the_", "ets_", "bits", "_wer", "_not", 
                       "lar_", "_but", "cal_", "_as_", "nal_", "ols_", "oved", "ers_", 
                       "_saw", "_tha", "_lik", "arth", "the_", "ets_", "ated", "ound", 
                       "lted", "_axe", "_and", "_som", "ared", "_suc", "res_", "_as_", 
                       "_ice", "_cap", "_and", "ons_", "ince", "the_", "_daw", "_of_", 
                       "the_", "ace_", "_age", "lose", "ion_", "_by_", "ace_", "bes_", 
                       "_has", "ound", "_tha", "arth", "_and", "the_", "her_", "ets_", 
                       "are_", "ics_", "_suc", "_as_", "ism_", "anes", "ics_", "_and", 
                       "_eve", "logy", "ets_", "_are", "ally", "ded_", "into", "_two", 
                       "_mai", "pes_", "arge", "ity_", "ant_", "ets_", "_and", "ler_", 
                       "cky_", "als_", "der_", "_iau", "ions", "ere_", "_are", "ght_", 
                       "ets_", "_in_", "the_", "lar_", "stem", "_in_", "der_", "_of_", 
                       "ing_", "nce_", "_fro", "the_", "_sun", "hey_", "_are", "the_", 
                       "four", "als_", "cury", "enus", "arth", "_and", "_mar", "hen_", 
                       "the_", "four", "ant_", "ets_", "iter", "turn", "anus", "_and", 
                       "tune", "_six", "_of_", "the_", "ets_", "_are", "ited", "_by_", 
                       "_one", "_or_", "_mor", "ral_", "ites", "__")


text_path = textTinyR::read_characters(PATH, characters = 2280)$data



context('tokenization and transformation')


#################
# error handling
#################



testthat::test_that("in case that the object parameter is NULL it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = NULL, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the batches parameter is not a numeric value it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = '4', read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the batches parameter is a numeric value and the path_2folder parameter is an empty character string it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = 3, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the length of the object parameter is greater than 1 it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = c(text_path, text_path), batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})



testthat::test_that("in case that the batches parameter is not NULL the object parameter should be a valid path to a file, otherwise return error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = 3, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = WRITE_PATH, stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the to_lower parameter is not logical it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = 'FALSE', to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})



testthat::test_that("in case that the to_upper parameter is not logical it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = 'FALSE', utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the read_file_delimiter parameter is not a character string it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = NULL, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the utf_locale parameter is not a character string it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = NULL, remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the remove_char parameter is not a character string it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = NULL,
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})




testthat::test_that("in case that the remove_punctuation_string parameter is not logical it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = 'FALSE', remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the remove_punctuation_vector parameter is not logical it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = 'FALSE', remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the remove_numbers parameter is not logical it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = 'FALSE', trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})



testthat::test_that("in case that the trim_token parameter is not logical it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = 'FALSE', split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the split_string parameter is not logical it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = 'FALSE', 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the split_separator parameter is not a character it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = NULL, remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the remove_stopwords parameter is not either TRUE, FALSE or a character vector of length greater than 0, it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = list(), language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the language parameter is invalid it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "invalid", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the min_num_char parameter is less than 1 it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 0, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the max_num_char parameter is less than the min_num_char it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = -Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the stemmer parameter is not one of porter2_stemmer, ngram_sequential or ngram_overlap it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'NULL', min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})



testthat::test_that("in case that the min_n_gram parameter is less than 1 it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 0,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the max_n_gram parameter is less than 1 it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
                                             
                                             max_n_gram = 0, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the min_n_gram is greater than the max_n_gram parameter it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 3,
                                             
                                             max_n_gram = 2, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the skip_n_gram parameter is less than 1 it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 0, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the skip_distance parameter is less than 0 it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = -1, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the n_gram_delimiter parameter is not a character it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = NULL, concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the concat_delimiter parameter is not a character it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = list(), path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the path_2folder parameter does not end in slash it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = getwd(), stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the path_2folder parameter is not a character it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = NULL, stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})



testthat::test_that("in case that the stemmer is gram_sequential and the stemmer_ngram is less than 1 it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = "ngram_sequential", min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 0, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the stemmer is gram_sequential and the stemmer_gamma is less than 0.0 it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = "ngram_sequential", min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = -0.1, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})



testthat::test_that("in case that the stemmer is gram_sequential and the stemmer_truncate is less than 1 it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = "ngram_sequential", min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 0, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})




testthat::test_that("in case that the stemmer is ngram_sequential and the stemmer_batches is less than 1 it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = "ngram_sequential", min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 0, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})


testthat::test_that("in case that the stemmer is ngram_overlap and the stemmer_ngram is less than 1 it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = "ngram_overlap", min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 0, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)  )
})



testthat::test_that("in case that the number of threads is less than 1 it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 0, vocabulary_path_file = NULL, verbose = FALSE)  )
})



testthat::test_that("in case that the verbose parameter is not logical it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = 'FALSE')  )
})


testthat::test_that("in case that the vocabulary_path_file parameter is not NULL and it is not a character string it returns an error", {
  
  testthat::expect_error( tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",
                                             
                                             remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, 
                                             
                                             split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                             
                                             max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                             
                                             stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = list(), verbose = FALSE)  )
})




testthat::test_that("in case that the to_lower or to_upper parameter is not TRUE and the language is not english it returns a warning", {
  
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
  
  testthat::expect_true( res_tes  )
})



testthat::test_that("the function returns a vector of words if string split is TRUE and the remove_stopwords parameter is a vector of user defined stop-words", {
  
  sample_stopwords = c("a", "this", "is")
  
  tmp = tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
                           
                           remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                           
                           split_separator = " \r\n\t.,;:()?!//", remove_stopwords = sample_stopwords, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                           
                           max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                           
                           stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
  res_stpw = sum(!sample_stopwords %in% tmp$token) == 3
  
  res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation" && res_stpw
  
  testthat::expect_true( res_tes  )
})


testthat::test_that("the function returns a vector of words if string split is TRUE and stemmer is porter2_stemmer", {
  
  tmp = tokenize_transform_text(object = text_path, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
                           
                           remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                           
                           split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = 'porter2_stemmer', min_n_gram = 1,
                           
                           max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                           
                           stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)

  stem = c('planetari', 'knowledg', 'hydrolog')
  
  res_stem = sum(stem %in% tmp$token) == 3
  
  res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation" && res_stem
  
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
  
  testthat::expect_true( res_tes  )
})




testthat::test_that("the function reads from a file and returns a vector", {
  
  tmp = tokenize_transform_text(object = PATH, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
                           
                           remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                           
                           split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                           
                           max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                           
                           stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
  res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
  
  
  testthat::expect_true( res_tes  )
})



testthat::test_that("the function reads from a file and writes to a file", {
  
  tmp = tokenize_transform_text(object = PATH, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
                           
                           remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                           
                           split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                           
                           max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = paste0(WRITE_PATH, "WRITE_FOLDER/"), stemmer_ngram = 4, stemmer_gamma = 0.0, 
                           
                           stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
  res_tes = names(tmp) == 'token'  && length(tmp) == 1 && tmp$token == "" && class(tmp) == "tokenization and transformation"
  
  
  testthat::expect_true( res_tes )
})




testthat::test_that("the function reads from a file and writes to a file in case that batches is greater than 1", {
  
  tmp = tokenize_transform_text(object = PATH, batches = 2, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "", remove_char = "",
                           
                           remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                           
                           split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                           
                           max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = " ", path_2folder = paste0(WRITE_PATH, "WRITE_FOLDER/"), stemmer_ngram = 4, stemmer_gamma = 0.0, 
                           
                           stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
  
  res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
  
  
  testthat::expect_true( res_tes )
})

