
#-------------------------
# random data for testing
#-------------------------

m1 = matrix(runif(100), 10, 10)

m2 = matrix(runif(100), 10, 10)


m1_binary = matrix(sample(0:1, 100, replace = T), 10, 10)

m2_binary = matrix(sample(0:1, 100, replace = T), 10, 10)


context('dissimilarity matrix calculation')


#======================================
# 'dissimilarity matrix' error handling
#======================================


testthat::test_that("in case that the first_matr parameter is not a matrix, it returns an error", {

  testthat::expect_error( TEXT_DOC_DISSIM(first_matr = NULL, second_matr = m2, method = 'euclidean', batches = NULL, threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the second_matr parameter is not a matrix, it returns an error", {

  testthat::expect_error( TEXT_DOC_DISSIM(first_matr = m1, second_matr = NULL, method = 'euclidean', batches = NULL, threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the rows of the matrices are not equal, it returns an error", {

  testthat::expect_error( TEXT_DOC_DISSIM(first_matr = m1, second_matr = rbind(m2, m2), method = 'euclidean', batches = NULL, threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the columns of the matrices are not equal, it returns an error", {

  testthat::expect_error( TEXT_DOC_DISSIM(first_matr = m1, second_matr = cbind(m2, m2), method = 'euclidean', batches = NULL, threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the method parameter is not of type character, it returns an error", {

  testthat::expect_error( TEXT_DOC_DISSIM(first_matr = m1, second_matr = m2, method = TRUE, batches = NULL, threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the method parameter is not valid, it returns an error", {

  testthat::expect_error( TEXT_DOC_DISSIM(first_matr = m1, second_matr = m2, method = 'invalid', batches = NULL, threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the batches parameter is not numeric, it returns an error", {

  testthat::expect_error( TEXT_DOC_DISSIM(first_matr = m1, second_matr = m2, method = 'euclidean', batches = 'NULL', threads = 1, verbose = FALSE) )
})


testthat::test_that("in case that the threads parameter is not numeric, it returns an error", {

  testthat::expect_error( TEXT_DOC_DISSIM(first_matr = m1, second_matr = m2, method = 'euclidean', batches = NULL, threads = NULL, verbose = FALSE) )
})


testthat::test_that("in case that the verbose parameter is not boolean, it returns an error", {

  testthat::expect_error( TEXT_DOC_DISSIM(first_matr = m1, second_matr = m2, method = 'euclidean', batches = NULL, threads = 1, verbose = 'FALSE') )
})


#===================================
# 'dissimilarity matrix' expect true
#===================================


testthat::test_that("it returns the correct output if the batches parameter is NULL", {

  dist_vec_cont = c("euclidean", "manhattan", "chebyshev", "canberra", "braycurtis", "pearson_correlation", "cosine", "hamming")

  vec_out = rep(NA, length(dist_vec_cont))

  for (i in 1:length(dist_vec_cont)) {

    tmp_out = TEXT_DOC_DISSIM(first_matr = m1, second_matr = m2, method = dist_vec_cont[i], batches = NULL, threads = 1, verbose = FALSE)

    vec_out[i] = length(tmp_out) == nrow(m1)
  }

  testthat::expect_true( all(vec_out) )
})


testthat::test_that("it returns the correct output if the batches parameter is NULL", {

  dist_vec_cont = c("simple_matching_coefficient", "jaccard_coefficient", "Rao_coefficient")

  vec_out = rep(NA, length(dist_vec_cont))

  for (i in 1:length(dist_vec_cont)) {

    tmp_out = TEXT_DOC_DISSIM(first_matr = m1_binary, second_matr = m2_binary, method = dist_vec_cont[i], batches = NULL, threads = 1, verbose = FALSE)

    vec_out[i] = length(tmp_out) == nrow(m1)
  }

  testthat::expect_true( all(vec_out) )
})


testthat::test_that("it returns the correct output if the batches parameter is not NULL", {

  tmp_out = TEXT_DOC_DISSIM(first_matr = m1, second_matr = m2, method = 'euclidean', batches = 2, threads = 1, verbose = FALSE)

  testthat::expect_true( length(tmp_out) == nrow(m1) )
})

