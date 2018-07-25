
#------
# data
#-----

tok1 = list(c('compare', 'this', 'text'),

            c('and', 'this', 'text'))

tok2 = list(c('with', 'another', 'set'),

            c('of', 'text', 'documents'))


context('intersection of text sequences')


# cnt_tsts = 1


while(T) {
   
    
  #================================
  # 'text_intersect' error handling
  #================================
  
  
  testthat::test_that("in case that the token_list1 parameter is not a list, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( text_intersect$new(token_list1 = NULL, token_list2 = NULL) )
  })
  
  
  testthat::test_that("in case that the token_list2 parameter is not a list, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( text_intersect$new(token_list1 = tok1, token_list2 = NULL) )
  })
  
  
  testthat::test_that("in case that the size of token_list1 is not equal with the size of token_list2, it returns an error", {
  
    tmp_tok = unlist(list(tok2, tok2), recursive = F)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( text_intersect$new(token_list1 = tok1, token_list2 = tmp_tok) )
  })
  
  
  testthat::test_that("in case that the distinct parameter is not a boolean (function : count_intersect), it returns an error", {
  
    init = text_intersect$new(token_list1 = tok1, token_list2 = tok2)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( init$count_intersect(distinct = 'FALSE', letters = FALSE) )
  })
  
  
  testthat::test_that("in case that the letters parameter is not a boolean (function : count_intersect), it returns an error", {
  
    init = text_intersect$new(token_list1 = tok1, token_list2 = tok2)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( init$count_intersect(distinct = FALSE, letters = 'FALSE') )
  })
  
  
  testthat::test_that("in case that the distinct parameter is not a boolean (function : ratio_intersect), it returns an error", {
  
    init = text_intersect$new(token_list1 = tok1, token_list2 = tok2)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( init$ratio_intersect(distinct = 'FALSE', letters = FALSE) )
  })
  
  
  testthat::test_that("in case that the letters parameter is not a boolean (function : ratio_intersect), it returns an error", {
  
    init = text_intersect$new(token_list1 = tok1, token_list2 = tok2)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( init$ratio_intersect(distinct = FALSE, letters = 'FALSE') )
  })
  
  
  #================================
  # 'text_intersect' error handling
  #================================
  
  
  testthat::test_that("it returns the correct output (function : count_intersect, distinct : F, letters : F)", {
  
    init = text_intersect$new(token_list1 = tok1, token_list2 = tok2)
  
    out = init$count_intersect(distinct = FALSE, letters = FALSE)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( !is.null(out) && is.vector(out) && length(out) == length(tok1) )
  })
  
  
  testthat::test_that("it returns the correct output (function : count_intersect, distinct : T, letters : F", {
  
    init = text_intersect$new(token_list1 = tok1, token_list2 = tok2)
  
    out = init$count_intersect(distinct = T, letters = FALSE)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( !is.null(out) && is.vector(out) && length(out) == length(tok1) )
  })
  
  
  testthat::test_that("it returns the correct output (function : count_intersect, distinct : T, letters : T", {
  
    init = text_intersect$new(token_list1 = tok1, token_list2 = tok2)
  
    out = init$count_intersect(distinct = T, letters = T)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( !is.null(out) && is.vector(out) && length(out) == length(tok1) )
  })
  
  
  testthat::test_that("it returns the correct output (function : count_intersect, distinct : F, letters : T", {
  
    init = text_intersect$new(token_list1 = tok1, token_list2 = tok2)
  
    out = init$count_intersect(distinct = F, letters = T)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( !is.null(out) && is.vector(out) && length(out) == length(tok1) )
  })
  
  
  testthat::test_that("it returns the correct output (function : ratio_intersect, distinct : F, letters : F)", {
  
    init = text_intersect$new(token_list1 = tok1, token_list2 = tok2)
  
    out = init$ratio_intersect(distinct = FALSE, letters = FALSE)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( !is.null(out) && is.vector(out) && length(out) == length(tok1) )
  })
  
  
  testthat::test_that("it returns the correct output (function : ratio_intersect, distinct : T, letters : F", {
  
    init = text_intersect$new(token_list1 = tok1, token_list2 = tok2)
  
    out = init$ratio_intersect(distinct = T, letters = FALSE)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( !is.null(out) && is.vector(out) && length(out) == length(tok1) )
  })
  
  
  testthat::test_that("it returns the correct output (function : ratio_intersect, distinct : T, letters : T", {
  
    init = text_intersect$new(token_list1 = tok1, token_list2 = tok2)
  
    out = init$ratio_intersect(distinct = T, letters = T)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( !is.null(out) && is.vector(out) && length(out) == length(tok1) )
  })
  
  
  testthat::test_that("it returns the correct output (function : ratio_intersect, distinct : F, letters : T", {
  
    init = text_intersect$new(token_list1 = tok1, token_list2 = tok2)
  
    out = init$ratio_intersect(distinct = F, letters = T)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-text_intersect.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( !is.null(out) && is.vector(out) && length(out) == length(tok1) )
  })
  
  break    # exit loop for tests ( count iterations / tests for debugging )
}   
