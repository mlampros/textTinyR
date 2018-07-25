
#-------------------------
# sample data for testing
#-------------------------

lst1 = list(c('use', 'this', 'function', 'to'), c('either', 'compute', 'the', 'jaccard'))

lst2 = list(c('or', 'the', 'dice', 'distance'), c('for', 'two', 'same', 'sized', 'lists'))



context('jaccard-dice similarity')


# cnt_tsts = 1


while(T) {
  
  #===============================
  # 'jaccard-dice' error handling
  #===============================
  
  
  testthat::test_that("in case that the token_list1 parameter is not a list, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-jaccard_dice.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( JACCARD_DICE(token_list1 = NULL, token_list2 = lst2, method = 'jaccard', threads = 1) )
  })
  
  
  testthat::test_that("in case that the token_list2 parameter is not a list, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-jaccard_dice.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( JACCARD_DICE(token_list1 = lst1, token_list2 = NULL, method = 'jaccard', threads = 1) )
  })
  
  
  testthat::test_that("in case that the length of the token_list1 is not equal with the length of the token_list2, it returns an error", {
  
    tmp_lst = unlist(list(lst2, lst2), recursive = F)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-jaccard_dice.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( JACCARD_DICE(token_list1 = lst1, token_list2 = tmp_lst, method = 'jaccard', threads = 1) )
  })
  
  
  testthat::test_that("in case that the method parameter is not of type character, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-jaccard_dice.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( JACCARD_DICE(token_list1 = lst1, token_list2 = lst2, method = FALSE, threads = 1) )
  })
  
  
  testthat::test_that("in case that the method parameter is not one of 'jaccard' or 'dice', it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-jaccard_dice.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( JACCARD_DICE(token_list1 = lst1, token_list2 = lst2, method = 'invalid', threads = 1) )
  })
  
  
  testthat::test_that("in case that the threads parameter is not of type numeric, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-jaccard_dice.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( JACCARD_DICE(token_list1 = lst1, token_list2 = lst2, method = 'dice', threads = TRUE) )
  })
  
  
  #============================
  # 'jaccard-dice' expect true
  #============================
  
  
  testthat::test_that("it returns the correct output if method is jaccard", {
  
    out = JACCARD_DICE(token_list1 = lst1, token_list2 = lst2, method = 'jaccard', threads = 1)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-jaccard_dice.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( length(out) == length(lst1) )
  })
  
  
  testthat::test_that("it returns the correct output if method is dice", {
  
    out = JACCARD_DICE(token_list1 = lst1, token_list2 = lst2, method = 'dice', threads = 1)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-jaccard_dice.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( length(out) == length(lst1) )
  })

  break    # exit loop for tests ( count iterations / tests for debugging )
}
