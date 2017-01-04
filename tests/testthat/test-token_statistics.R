

if (.Platform$OS.type == "windows") {

  PATH = paste0(getwd(), path.expand("\\VOCAB_token_stats.txt"))
  
  PATH_folder = paste0(getwd(), path.expand("\\VOCAB_token_stats\\"))
  
  PATH_parser = paste0(getwd(), path.expand("\\TOKEN_ngram_PARSER.txt"))
}

if (.Platform$OS.type == "unix") {

  PATH = paste0(getwd(), path.expand("/VOCAB_token_stats.txt"))
  
  PATH_folder = paste0(getwd(), path.expand("/VOCAB_token_stats/"))
  
  PATH_parser = paste0(getwd(), path.expand("/TOKEN_ngram_PARSER.txt"))
}

voc = read.table(PATH, quote = "\"", comment.char = "")

voc_vec = as.vector(voc[, 1])


context('token statistics')



#----------------------------------
# initialization [ error handling ]
#----------------------------------


testthat::test_that("in case that the x_vec parameter is not NULL or a valid path to a file it returns an error", {

  testthat::expect_error( token_stats$new(x_vec = list(), path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_") )
})


testthat::test_that("in case that the path_2folder parameter is not NULL or a valid path to a folder it returns an error", {
  
  testthat::expect_error( token_stats$new(x_vec = NULL, path_2folder = list(), path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_") )
})


testthat::test_that("in case that the path_2folder parameter is a character string but does not end in slash it returns an error", {
  
  if (.Platform$OS.type == "windows") {

    PATH_folder1 = paste0(getwd(), path.expand("\\VOCAB_token_stats"))
  }
  
  if (.Platform$OS.type == "unix") {

    PATH_folder1 = paste0(getwd(), path.expand("/VOCAB_token_stats"))
  }
  
  testthat::expect_error( token_stats$new(x_vec = NULL, path_2folder = PATH_folder1, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_") )
})


testthat::test_that("in case that the path_2file parameter is not NULL or a valid path to a folder it returns an error", {
  
  testthat::expect_error( token_stats$new(x_vec = NULL, path_2folder = NULL, path_2file = list(), file_delimiter = '\n', n_gram_delimiter = "_") )
})


testthat::test_that("in case that the file_delimiter parameter is not a character string it returns an error", {
  
  testthat::expect_error( token_stats$new(x_vec = NULL, path_2folder = NULL, path_2file = NULL, file_delimiter = NULL, n_gram_delimiter = "_") )
})


testthat::test_that("in case that the n_gram_delimiter parameter is not a character string it returns an error", {
  
  testthat::expect_error( token_stats$new(x_vec = NULL, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = NULL) )
})



#-----------------------------
# path 2vector [ expect true ]
#-----------------------------


testthat::test_that("it returns a word vector from a single file", {
  
  init = token_stats$new(x_vec = NULL, path_2folder = NULL, path_2file = PATH, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec = init$path_2vector()
  
  testthat::expect_true( length(vec) == 353 && inherits(vec, c('vector', 'character')) )
})



testthat::test_that("it returns a word vector from a folder of files", {
  
  init = token_stats$new(x_vec = NULL, path_2folder = PATH_folder, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec = init$path_2vector()
  
  testthat::expect_true( length(vec) == 353 * 2 && inherits(vec, c('vector', 'character')) )
})


#-------------------------------------
# freq distribution [ error handling ]
#-------------------------------------


testthat::test_that("it gives an error if the subset value is not numeric", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$freq_distribution()
  
  testthat::expect_error( init$print_frequency('invalid') )
})


#----------------------------------
# freq distribution [ expect true ]
#----------------------------------

testthat::test_that("it returns the frequency distribution (all data) using a character vector", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$freq_distribution()
  
  testthat::expect_output( init$print_frequency() )
})


testthat::test_that("it returns the frequency distribution (a subset of the data) using a character vector", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$freq_distribution()
  
  testthat::expect_output( init$print_frequency(1:10) )
})



testthat::test_that("it returns the frequency distribution (all data) using a path to a folder", {
  
  init = token_stats$new(x_vec = NULL, path_2folder = PATH_folder, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$freq_distribution()
  
  testthat::expect_output( init$print_frequency() )
})


testthat::test_that("it returns the frequency distribution (a subset of the data) using a path to a folder", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = PATH_folder, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$freq_distribution()
  
  testthat::expect_output( init$print_frequency(1:10) )
})


testthat::test_that("it returns the frequency distribution (all data) using a path to a file", {
  
  init = token_stats$new(x_vec = NULL, path_2folder = NULL, path_2file = PATH, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$freq_distribution()
  
  testthat::expect_output( init$print_frequency() )
})


testthat::test_that("it returns the frequency distribution (a subset of the data) using a path to a file", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = PATH, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$freq_distribution()
  
  testthat::expect_output( init$print_frequency(1:10) )
})



#-----------------------------------
# count character [ error handling ]
#-----------------------------------


testthat::test_that("it gives an error if the number parameter is not a numeric value", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$count_character()

  testthat::expect_error( init$print_count_character(number = NULL) )
})



testthat::test_that("it gives an error if the number parameter is a vector", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$count_character()
  
  testthat::expect_error( init$print_count_character(number = 1:3) )
})


#--------------------------------
# count character [ expect true ]
#--------------------------------


testthat::test_that("the count_character() method returns the unique numbers for the characters of the vector", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$count_character()
  
  testthat::expect_true( inherits(vec_tmp, c('vector', 'numeric')) )
})


testthat::test_that("the print_count_character() method returns the correct words for a specific value (for a vector of words)", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$count_character()
  
  val = 3
  
  res = init$print_count_character(val)
  
  testthat::expect_true( unique(sapply(res, nchar)) == val )
})


testthat::test_that("the print_count_character() method returns the correct words for a specific value (for a folder of files)", {
  
  init = token_stats$new(x_vec = NULL, path_2folder = PATH_folder, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$count_character()
  
  val = 3
  
  res = init$print_count_character(val)
  
  testthat::expect_true( unique(sapply(res, nchar)) == val )
})


testthat::test_that("the print_count_character() method returns the correct words for a specific value (for a file)", {
  
  init = token_stats$new(x_vec = NULL, path_2folder = NULL, path_2file = PATH, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$count_character()
  
  val = 3
  
  res = init$print_count_character(val)
  
  testthat::expect_true( unique(sapply(res, nchar)) == val )
})



#--------------------------------------
#  collocation-words [ error handling ]
#--------------------------------------


testthat::test_that("it gives an error if the word parameter of the print_collocations method is not a character string", {
  
  tok = tokenize_transform_text(PATH, to_lower = T, split_string = T, min_n_gram = 3, max_n_gram = 3, n_gram_delimiter = "_")
  
  init = token_stats$new(x_vec = tok$token, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$collocation_words()
  
  testthat::expect_error( init$print_collocations(word = NULL) )
})



#-----------------------------------
#  collocation-words [ expect true ]
#-----------------------------------


testthat::test_that("it returns a named vector with the collocations for a specific word in case that the input is a vector", {
  
  tok = tokenize_transform_text(PATH, to_lower = T, split_string = T, min_n_gram = 3, max_n_gram = 3, n_gram_delimiter = "_")
  
  init = token_stats$new(x_vec = tok$token, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$collocation_words()
  
  res = init$print_collocations(word = "ancient")
  
  testthat::expect_true( inherits(res, c('numeric', 'vector')) )
})



testthat::test_that("it returns a named vector with the collocations for a specific word in case that the input is a path to a file", {

  init = token_stats$new(x_vec = NULL, path_2folder = NULL, path_2file = PATH_parser, file_delimiter = '\n', n_gram_delimiter = "_")
  
  vec_tmp = init$collocation_words()
  
  res = init$print_collocations(word = "ancient")
  
  testthat::expect_true( inherits(res, c('numeric', 'vector')) )
})





#------------------------------------------------
#  string dissimilarity matrix [ error handling ]
#------------------------------------------------


testthat::test_that("it gives an error if the dice_n_gram parameter is not numeric", {
  
  init = token_stats$new(x_vec = voc_vec[1:30], path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")

  testthat::expect_error( init$string_dissimilarity_matrix(dice_n_gram = NULL, method = "dice", split_separator = " ", dice_thresh = 0.3, upper = TRUE, diagonal = TRUE, threads = 1) )
})


testthat::test_that("it gives an error if the dice_n_gram parameter is a vector", {
  
  init = token_stats$new(x_vec = voc_vec[1:30], path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  testthat::expect_error( init$string_dissimilarity_matrix(dice_n_gram = 1:5, method = "dice", split_separator = " ", dice_thresh = 0.3, upper = TRUE, diagonal = TRUE, threads = 1) )
})


testthat::test_that("it gives an error if the dice_n_gram parameter is less than 1", {
  
  init = token_stats$new(x_vec = voc_vec[1:30], path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  testthat::expect_error( init$string_dissimilarity_matrix(dice_n_gram = 0, method = "dice", split_separator = " ", dice_thresh = 0.3, upper = TRUE, diagonal = TRUE, threads = 1) )
})



testthat::test_that("it gives an error if the method is not one of dice, levenshtein, cosine", {
  
  init = token_stats$new(x_vec = voc_vec[1:30], path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  testthat::expect_error( init$string_dissimilarity_matrix(dice_n_gram = 2, method = "invalid", split_separator = " ", dice_thresh = 0.3, upper = TRUE, diagonal = TRUE, threads = 1) )
})


testthat::test_that("it gives an error if the split_separator is not a character string", {
  
  init = token_stats$new(x_vec = voc_vec[1:30], path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  testthat::expect_error( init$string_dissimilarity_matrix(dice_n_gram = 2, method = "dice", split_separator = NULL, dice_thresh = 0.3, upper = TRUE, diagonal = TRUE, threads = 1) )
})


testthat::test_that("it gives an error if the dice_thresh is greater than 1.0", {
  
  init = token_stats$new(x_vec = voc_vec[1:30], path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  testthat::expect_error( init$string_dissimilarity_matrix(dice_n_gram = 2, method = "dice", split_separator = " ", dice_thresh = 1.1, upper = TRUE, diagonal = TRUE, threads = 1) )
})



testthat::test_that("it gives an error if the upper parameter is not logical", {
  
  init = token_stats$new(x_vec = voc_vec[1:30], path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  testthat::expect_error( init$string_dissimilarity_matrix(dice_n_gram = 2, method = "dice", split_separator = " ", dice_thresh = 0.3, upper = 'TRUE', diagonal = TRUE, threads = 1) )
})


testthat::test_that("it gives an error if the diagonal parameter is not logical", {
  
  init = token_stats$new(x_vec = voc_vec[1:30], path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  testthat::expect_error( init$string_dissimilarity_matrix(dice_n_gram = 2, method = "dice", split_separator = " ", dice_thresh = 0.3, upper = TRUE, diagonal = 'TRUE', threads = 1) )
})


testthat::test_that("it gives an error if the threads parameter is less than 1", {
  
  init = token_stats$new(x_vec = voc_vec[1:30], path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  testthat::expect_error( init$string_dissimilarity_matrix(dice_n_gram = 2, method = "dice", split_separator = " ", dice_thresh = 0.3, upper = TRUE, diagonal = TRUE, threads = 0) )
})


#---------------------------------------------
#  string dissimilarity matrix [ expect true ]
#---------------------------------------------


testthat::test_that("it returns a matrix for the dice method", {
  
  cols = 30
  
  init = token_stats$new(x_vec = voc_vec[1:cols], path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  res = init$string_dissimilarity_matrix(dice_n_gram = 2, method = "dice", split_separator = " ", dice_thresh = 0.3, upper = TRUE, diagonal = TRUE, threads = 1)
  
  testthat::expect_true( is.matrix(res) && ncol(res) == cols )
})


testthat::test_that("it returns a matrix for the levenshtein method", {
  
  cols = 30
  
  init = token_stats$new(x_vec = voc_vec[1:cols], path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  res = init$string_dissimilarity_matrix(dice_n_gram = 2, method = "levenshtein", split_separator = " ", dice_thresh = 0.3, upper = TRUE, diagonal = TRUE, threads = 1)
  
  testthat::expect_true( is.matrix(res) && ncol(res) == cols )
})



testthat::test_that("it returns a matrix for the cosine method", {
  
  VEC = c('the first sentece', 'the second sentence', 'the third sentence', 'the fourth sentence')
  
  init = token_stats$new(x_vec = VEC, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  res = init$string_dissimilarity_matrix(dice_n_gram = 2, method = "cosine", split_separator = " ", dice_thresh = 0.3, upper = TRUE, diagonal = TRUE, threads = 1)
  
  testthat::expect_true( is.matrix(res) && ncol(res) == length(VEC) )
})



#---------------------------------
# look up table [ error handling ]
#---------------------------------


testthat::test_that("it gives an error if the n_grams parameter is not a numeric value", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  testthat::expect_error( init$look_up_table(n_grams = NULL) )
})


testthat::test_that("it gives an error if the n_grams parameter is a vector", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  testthat::expect_error( init$look_up_table(n_grams = 1:3) )
})


testthat::test_that("it gives an error if the n_grams parameter is less than 1", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  testthat::expect_error( init$look_up_table(n_grams = 0) )
})



#------------------------------
# look up table [ expect true ]
#------------------------------


testthat::test_that("it returns a character n-gram vector", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  is_vec = init$look_up_table(n_grams = 3)
  
  testthat::expect_true( inherits(is_vec, c('character', 'vector'))  )
})



#-----------------------------------------
# print a look up table [ error handling ]
#-----------------------------------------


testthat::test_that("it gives an error if the n_grams parameter is not a numeric value", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  lktbl = init$look_up_table(n_grams = 4)
  
  testthat::expect_error( init$print_words_lookup_tbl(n_gram = NULL) )
})


testthat::test_that("it gives an error if the n_grams parameter is not a character vector", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  lktbl = init$look_up_table(n_grams = 4)
  
  testthat::expect_error( init$print_words_lookup_tbl(n_gram = c("ts'_", "tune")) )
})


#--------------------------------------
# print a look up table [ expect true ]
#--------------------------------------


testthat::test_that("it returns a vector of n-grams", {
  
  init = token_stats$new(x_vec = voc_vec, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_")
  
  lktbl = init$look_up_table(n_grams = 4)
  
  res = init$print_words_lookup_tbl(n_gram = "_abo") 
  
  testthat::expect_true( inherits(res, c('character', 'vector')) )
})

