

if (.Platform$OS.type == "windows") {

  PATH = paste0(getwd(), path.expand("\\example_word_vecs.txt"))
  INVALID_PATH = paste0(getwd(), path.expand("\\INVALID\\example_word_vecs.txt"))
  TRIPLET = paste0(getwd(), path.expand("\\test_triplet_data.RDATA"))
}

if (.Platform$OS.type == "unix") {

  PATH = paste0(getwd(), path.expand("/example_word_vecs.txt"))
  INVALID_PATH = paste0(getwd(), path.expand("/INVALID/example_word_vecs.txt"))
  TRIPLET = paste0(getwd(), path.expand("/test_triplet_data.RDATA"))
}


#-------------------------
# sample data for testing
#-------------------------

tok_text = list(c('the', 'result', 'of'), c('doc2vec', 'are', 'vector', 'features'))


context('doc2vec similarity')


# cnt_tsts = 1


while(T) {
  
  #==========================
  # 'Doc2Vec' error handling
  #==========================
  
  
  testthat::test_that("in case that the token_list parameter is not of type character, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( Doc2Vec$new(token_list = NULL) )
  })
  
  
  testthat::test_that("in case that the word_vector_FILE parameter is not a valid path to a file, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( Doc2Vec$new(token_list = tok_text, word_vector_FILE = NULL) )
  })
  
  
  testthat::test_that("in case that the verbose parameter is not a boolean, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, verbose = 'FALSE') )
  })
  
  
  testthat::test_that("in case that the print_every_rows parameter is not a numeric value, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, print_every_rows = '10000', verbose = T) )
  })
  
  
  testthat::test_that("in case that the copy_data parameter is not a boolean, it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, copy_data = 'FALSE') )
  })
  
  
  
  #=================================
  # 'doc2vec_methods' error handling
  #=================================
  
  
  testthat::test_that("in case that the method parameter is invalid, it returns an error", {
    
    INIT = Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, verbose = F)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( INIT$doc2vec_methods(method = "invalid") )
  })
  
  
  testthat::test_that("in case that the threads parameter is not greater than 0, it returns an error", {
    
    INIT = Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, verbose = F)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( INIT$doc2vec_methods(method = "sum_sqrt", threads = 0) )
  })
  
  
  testthat::test_that("in case that the method parameter is 'idf' and the 'global_term_weights' parameter is not a valid object, it returns an error", {
    
    INIT = Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, copy_data = F)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( INIT$doc2vec_methods(method = "idf", global_term_weights = list(invalid_name1 = tok_text[[1]], invalid_name2 = tok_text[[2]])) )
  })
  
  
  testthat::test_that("in case that the method parameter is 'idf' and the 'global_term_weights' parameter is NULL, it returns an error", {
    
    INIT = Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, copy_data = F)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( INIT$doc2vec_methods(method = "idf", global_term_weights = NULL) )
  })
  
  
  #==================================
  # 'pre_processed_wv' error handling
  #==================================
  
  
  testthat::test_that("in case that the copy_data parameter is set to FALSE the 'pre_processed_wv' method returns an message", {
    
    INIT = Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, verbose = F, copy_data = F)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_output( INIT$pre_processed_wv() )
  })
  
  
  #======================
  # 'Doc2Vec' expect true
  #======================
  
  
  testthat::test_that("in case that the method parameter is 'sum_sqrt', it returns the correct output", {
    
    INIT = Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, copy_data = F)
    
    res = INIT$doc2vec_methods(method = "sum_sqrt")
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( is.matrix(res) && nrow(res) == length(tok_text) )
  })
  
  
  testthat::test_that("in case that the method parameter is 'sum_sqrt', it returns the correct output [ copy_data = TRUE ]", {
    
    INIT = Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, copy_data = T)
    
    res = INIT$doc2vec_methods(method = "sum_sqrt")
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( is.matrix(res) && nrow(res) == length(tok_text) )
  })
  
  
  testthat::test_that("in case that the method parameter is 'min_max_norm', it returns the correct output", {
    
    INIT = Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, copy_data = F)
    
    res = INIT$doc2vec_methods(method = "min_max_norm")
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( is.matrix(res) && nrow(res) == length(tok_text) )
  })
  
  
  
  testthat::test_that("in case that the method parameter is 'idf', it returns the correct output", {
    
    INIT = Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, copy_data = F)
    
    load(TRIPLET)
    
    res = INIT$doc2vec_methods(method = "idf", global_term_weights = gl_term_w)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( is.matrix(res) && nrow(res) == length(tok_text) )
  })
  
  
  
  #===============================
  # 'pre_processed_wv' expect true
  #===============================
  
  
  testthat::test_that("in case that the copy_data parameter is set to TRUE it returns a list", {
    
    INIT = Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, verbose = F, copy_data = T)
    
    dat = INIT$pre_processed_wv()
    
    obj_vec = is.vector(dat$terms_reduced_wordvecs)
    
    obj_list = is.list(dat$reduced_wordvecs_pointer)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-doc2vec.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( obj_vec && obj_list && length(obj_vec) > 0 && length(obj_list) > 0 )
  })
  
  break    # exit loop for tests ( count iterations / tests for debugging )
}
