
#------
# data
#-----

tok_lst = list(c('the', 'the', 'tokens', 'of', 'first', 'document'),
               c('the', 'tokens', 'of', 'of', 'second', 'document'),
               c('the', 'tokens', 'of', 'third', 'third', 'document'))

vec_clust = rep(1:6, 3)




context('cluster-frequency function')


# cnt_tsts = 1


while(T) {
    
  
  #===================================
  # 'cluster_frequency' error handling
  #===================================
  
  
  testthat::test_that("in case that the verbose parameter is not a boolean, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-cluster_frequency.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( cluster_frequency(tok_lst, vec_clust, verbose = 'TRUE') )
  })
  
  
  #================================
  # 'cluster_frequency' expect true
  #================================
  
  
  testthat::test_that("it returns the correct output", {
  
    res = cluster_frequency(tok_lst, vec_clust)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-cluster_frequency.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
  
    testthat::expect_true( inherits(res, 'list') && inherits(res[[1]], "data.table") && length(res) == length(tok_lst)  )
  })
  
  break    # exit loop for tests ( count iterations / tests for debugging )
}

