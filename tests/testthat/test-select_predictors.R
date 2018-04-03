
#------
# data
#-----

set.seed(1)
resp = runif(100)

set.seed(2)
col = runif(100)

matr = matrix(c(col, col^4, col^6, col^8, col^10), nrow = 100, ncol = 5)


context('correlation function')


#===================================
# 'select_predictors' error handling
#===================================


testthat::test_that("in case that the response parameter is not a numeric vector, it returns an error", {

  testthat::expect_error( select_predictors(response_vector = NULL, predictors_matrix = NULL, threads = 1) )
})


testthat::test_that("in case that the predictors parameter is not a matrix, it returns an error", {

  testthat::expect_error( select_predictors(response_vector = resp, predictors_matrix = NULL, threads = 1) )
})


testthat::test_that("in case that the threads parameter is not a numeric value, it returns an error", {

  testthat::expect_error( select_predictors(response_vector = resp, predictors_matrix = matr, threads = T) )
})


testthat::test_that("in case that the verbose parameter is not a boolean, it returns an error", {
  
  testthat::expect_error( select_predictors(response_vector = resp, predictors_matrix = matr, threads = 1, verbose = 'TRUE') )
})


#================================
# 'select_predictors' expect true
#================================


testthat::test_that("it returns the correct output", {

  out = select_predictors(resp, matr, predictors_upper_thresh = 0.75)

  testthat::expect_true( is.vector(out) && length(out) != 0 )
})


testthat::test_that("it returns the correct output if response_lower_thresh = 0.0 AND predictors_upper_thresh = 1.0", {
  
  out = select_predictors(resp, matr, response_lower_thresh = 0.0, predictors_upper_thresh = 1.0)
  
  testthat::expect_true( is.vector(out) && length(out) == ncol(matr) )
})

