
#------
# data
#-----

tok_lst = list(c('the', 'the', 'tokens', 'of', 'first', 'document'),
               c('the', 'tokens', 'of', 'of', 'second', 'document'),
               c('the', 'tokens', 'of', 'third', 'third', 'document'))

vec_clust = rep(1:6, 3)




context('cluster-frequency function')


#===================================
# 'cluster_frequency' error handling
#===================================


testthat::test_that("in case that the verbose parameter is not a boolean, it returns an error", {
  
  testthat::expect_error( cluster_frequency(tok_lst, vec_clust, verbose = 'TRUE') )
})


#================================
# 'cluster_frequency' expect true
#================================


testthat::test_that("it returns the correct output", {

  res = cluster_frequency(tok_lst, vec_clust)

  testthat::expect_true( inherits(res, 'list') && inherits(res[[1]], "data.table") && length(res) == length(tok_lst)  )
})

