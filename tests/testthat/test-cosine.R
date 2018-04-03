
#-------------------------
# sample data for testing
#-------------------------

vec1 = c('use this', 'function to compute the')

vec2 = c('cosine distance', 'between text sequences')



context('cosine similarity')


#==================================
# 'cosine distance' error handling
#==================================


testthat::test_that("in case that the text_vector1 parameter is not a character vector, it returns an error", {

  testthat::expect_error( COS_TEXT(text_vector1 = NULL, text_vector2 = vec2, threads = 1, separator = " ") )
})


testthat::test_that("in case that the text_vector2 parameter is not a character vector, it returns an error", {

  testthat::expect_error( COS_TEXT(text_vector1 = vec1, text_vector2 = NULL, threads = 1, separator = " ") )
})


testthat::test_that("in case that the text_vector1 is not of the same length with text_vector2, it returns an error", {

  testthat::expect_error( COS_TEXT(text_vector1 = vec1, text_vector2 = c(vec2, vec2), threads = 1, separator = " ") )
})


testthat::test_that("in case that the text_vector2 parameter is not a character vector, it returns an error", {

  testthat::expect_error( COS_TEXT(text_vector1 = vec1, text_vector2 = vec2, threads = NULL, separator = " ") )
})


testthat::test_that("in case that the text_vector2 parameter is not a character vector, it returns an error", {

  testthat::expect_error( COS_TEXT(text_vector1 = vec1, text_vector2 = vec2, threads = 1, separator = FALSE) )
})


#==============================
# 'cosine distance' expect true
#==============================


testthat::test_that("it returns the correct output", {

  out = COS_TEXT(text_vector1 = vec1, text_vector2 = vec2, threads = 1, separator = " ")

  testthat::expect_true( length(out) == length(vec1) )
})





