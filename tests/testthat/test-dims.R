

if (.Platform$OS.type == "windows") {

  PATH = paste0(getwd(), path.expand("\\example_word_vecs.txt"))
}

if (.Platform$OS.type == "unix") {

  PATH = paste0(getwd(), path.expand("/example_word_vecs.txt"))
}


context('dimensions of Word Vectors')


# cnt_tsts = 1


while(T) {
  
    
  #===================================
  # 'dims_of_word_vecs' error handling
  #===================================
  
  
  testthat::test_that("in case that the data is not a valid path to a file, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-dims.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( dims_of_word_vecs(input_file = NULL, read_delimiter = '\n') )
  })
  
  
  testthat::test_that("in case that the read_delimiter parameter is not of type character, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-dims.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( dims_of_word_vecs(input_file = PATH, read_delimiter = TRUE) )
  })
  
  
  #================================
  # 'dims_of_word_vecs' expect true
  #================================
  
  
  testthat::test_that("it returns the correct dimensions of a word vectors file", {
  
    tmp = dims_of_word_vecs(input_file = PATH, read_delimiter = '\n')
    
    #-------------------------------------------------------------------- debug tests
    cat("test-dims.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( tmp == 100 )
  })
  
  break    # exit loop for tests ( count iterations / tests for debugging )
}
