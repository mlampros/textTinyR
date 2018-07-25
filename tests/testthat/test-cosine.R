
#-------------------------
# sample data for testing
#-------------------------

vec1 = c('use this', 'function to compute the')

vec2 = c('cosine distance', 'between text sequences')



context('cosine similarity')


# cnt_tsts = 1


while(T) {
  

  #==================================
  # 'cosine distance' error handling
  #==================================
  
  
  testthat::test_that("in case that the text_vector1 parameter is not a character vector, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-cosine.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    
    testthat::expect_error( COS_TEXT(text_vector1 = NULL, text_vector2 = vec2, threads = 1, separator = " ") )
  })
  
  
  testthat::test_that("in case that the text_vector2 parameter is not a character vector, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-cosine.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    
    testthat::expect_error( COS_TEXT(text_vector1 = vec1, text_vector2 = NULL, threads = 1, separator = " ") )
  })
  
  
  testthat::test_that("in case that the text_vector1 is not of the same length with text_vector2, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-cosine.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    
    testthat::expect_error( COS_TEXT(text_vector1 = vec1, text_vector2 = c(vec2, vec2), threads = 1, separator = " ") )
  })
  
  
  testthat::test_that("in case that the text_vector2 parameter is not a character vector, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-cosine.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    
    testthat::expect_error( COS_TEXT(text_vector1 = vec1, text_vector2 = vec2, threads = NULL, separator = " ") )
  })
  
  
  testthat::test_that("in case that the text_vector2 parameter is not a character vector, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-cosine.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    
    testthat::expect_error( COS_TEXT(text_vector1 = vec1, text_vector2 = vec2, threads = 1, separator = FALSE) )
  })
  
  
  #==============================
  # 'cosine distance' expect true
  #==============================
  
  
  testthat::test_that("it returns the correct output", {
  
    out = COS_TEXT(text_vector1 = vec1, text_vector2 = vec2, threads = 1, separator = " ")
    
    #-------------------------------------------------------------------- debug tests
    cat("test-cosine.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    
    testthat::expect_true( length(out) == length(vec1) )
  })

  break    # exit loop for tests ( count iterations / tests for debugging )
}



