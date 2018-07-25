
if (.Platform$OS.type == "windows") {

  WRITE_PATH = paste0(getwd(),"\\SAVE_SPARSE\\")
  
  read_CHARS_ROWS = paste0(getwd(),"\\read_CHARS_ROWS\\write_rows_chars.txt")
  
  PATH = paste0(getwd(), path.expand("\\test_text.txt"))
}

if (.Platform$OS.type == "unix") {

  WRITE_PATH = paste0(getwd(),"/SAVE_SPARSE/")
  
  PATH = paste0(getwd(), path.expand("/test_text.txt"))
  
  read_CHARS_ROWS = paste0(getwd(),"/read_CHARS_ROWS/write_rows_chars.txt")
}



context('sparse functions AND read text')


# cnt_tsts = 1


while(T) {
  
  
  #---------------------------------
  # dense 2sparse [ error handling ]
  #---------------------------------
  
  testthat::test_that("in case that the dense_mat parameter is not a matrix it returns an error", {
  
    dsm = data.frame(matrix(1:100, 10, 10))
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( dense_2sparse(dsm) )
  })
  
  
  #------------------------------
  # dense 2sparse [ expect true ]
  #------------------------------
  
  
  testthat::test_that("it returns a sparse matrix", {
    
    dsm = matrix(1:100, 10, 10)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( inherits(dense_2sparse(dsm), "dgCMatrix") )
  })
  
  
  #-------------------------------
  # sparse Sums [ error handling ]
  #-------------------------------
  
  
  testthat::test_that("in case that the sparse_matrix parameter is not a sparse matrix it returns an error", {
    
    dsm = matrix(1:100, 10, 10)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( sparse_Sums(dsm, rowSums = FALSE) )
  })
  
  
  
  testthat::test_that("in case that the sparse_matrix parameter is not a sparse matrix it returns an error", {
    
    dsm = dense_2sparse(matrix(1:100, 10, 10))
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( sparse_Sums(dsm, rowSums = 'FALSE') )
  })
  
  
  #----------------------------
  # sparse Sums [ expect true ]
  #----------------------------
  
  
  testthat::test_that("it returns the column sums of the sparse matrix", {
    
    dsm = dense_2sparse(matrix(1, 10, 10))
    
    sm = sparse_Sums(dsm, rowSums = FALSE)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( is.vector(sm) && sum(sm) == 100 )
  })
  
  
  testthat::test_that("it returns the row sums of the sparse matrix", {
    
    dsm = dense_2sparse(matrix(1, 10, 10))
    
    sm = sparse_Sums(dsm, rowSums = TRUE)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( is.vector(sm) && sum(sm) == 100 )
  })
  
  
  
  #--------------------------------
  # sparse Means [ error handling ]
  #--------------------------------
  
  
  testthat::test_that("in case that the sparse_matrix parameter is not a sparse matrix it returns an error", {
    
    dsm = matrix(1:100, 10, 10)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( sparse_Means(dsm, rowMeans = FALSE) )
  })
  
  
  
  testthat::test_that("in case that the sparse_matrix parameter is not a sparse matrix it returns an error", {
    
    dsm = dense_2sparse(matrix(1:100, 10, 10))
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( sparse_Means(dsm, rowMeans = 'FALSE') )
  })
  
  
  #-----------------------------
  # sparse Means [ expect true ]
  #-----------------------------
  
  
  testthat::test_that("it returns the column means of the sparse matrix", {
    
    dsm = dense_2sparse(matrix(1, 10, 10))
    
    sm = sparse_Means(dsm, rowMeans = FALSE)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( is.vector(sm) && sum(sm) == 10 )
  })
  
  
  testthat::test_that("it returns the row means of the sparse matrix", {
    
    dsm = dense_2sparse(matrix(1, 10, 10))
    
    sm = sparse_Means(dsm, rowMeans = TRUE)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( is.vector(sm) && sum(sm) == 10 )
  })
  
  
  
  
  #-----------------------------------
  # matrix sparsity [ error handling ]
  #-----------------------------------
  
  
  testthat::test_that("in case that the sparse_matrix parameter is not a sparse matrix it returns an error", {
    
    dsm = matrix(1:100, 10, 10)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( matrix_sparsity(dsm) )
  })
  
  
  
  #--------------------------------
  # matrix sparsity [ expect true ]
  #--------------------------------
  
  
  testthat::test_that("it returns a float number", {
    
    dsm = dense_2sparse(matrix(sample(0:1, 100, replace = T), 10, 10))
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_output( matrix_sparsity(dsm) )
  })
  
  
  
  #--------------------------------------
  # save sparse binary [ error handling ]
  #--------------------------------------
  
  
  testthat::test_that("in case that the sparse_matrix parameter is not a sparse matrix it returns an error", {
    
    dsm = matrix(1:100, 10, 10)
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( save_sparse_binary(dsm, file_name = paste0(WRITE_PATH, "save_sparse.mat")) )
  })
  
  
  
  testthat::test_that("in case that the file_name parameter is NULL it returns an error", {
    
    dsm = dense_2sparse(matrix(1:100, 10, 10))
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( save_sparse_binary(dsm, file_name = NULL) )
  })
  
  
  testthat::test_that("in case that the file_name parameter is not a character string it returns an error", {
    
    dsm = dense_2sparse(matrix(1:100, 10, 10))
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( save_sparse_binary(dsm, file_name = list()) )
  })
  
  
  #------------------------------------
  # save sparse binary  [ expect true ]
  #------------------------------------
  
  
  testthat::test_that("it saves the sparse matrix in a file", {
    
    dsm = dense_2sparse(matrix(1:100, 10, 10))
    
    res = save_sparse_binary(dsm, paste0(WRITE_PATH, "save_sparse.mat"))
    
    lst = length(list.files(WRITE_PATH))
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( lst == 1 )
  })
  
  
  
  #--------------------------------------
  # load sparse binary [ error handling ]
  #--------------------------------------
  
  
  testthat::test_that("in case that the file_name parameter is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( load_sparse_binary(file_name = NULL) )
  })
  
  
  #-----------------------------------
  # load sparse binary [ expect true ]
  #-----------------------------------
  
  testthat::test_that("it loads the sparse matrix", {
    
    mt = load_sparse_binary(file_name = paste0(WRITE_PATH, "save_sparse.mat"))
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( inherits(mt, "dgCMatrix") && nrow(mt) == 10 && ncol(mt) == 10 )
  })
  
  
  
  #-----------------------------------
  # read characters [ error handling ]
  #-----------------------------------
  
  
  testthat::test_that("in case that the input_file parameter is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( read_characters(input_file = NULL, characters = 100, write_2file = "") )
  })
  
  
  testthat::test_that("in case that the input_file parameter is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( read_characters(input_file = PATH, characters = 10, write_2file = NULL) )
  })
  
  
  testthat::test_that("in case that the characters parameter is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( read_characters(input_file = PATH, characters = 0, write_2file = "") )
  })
  
  
  
  #--------------------------------
  # read characters [ expect true ]
  #--------------------------------
  
  
  testthat::test_that("it returns the correct output if write_2file is an empty string", {
    
    NCHAR = 5
    
    res = read_characters(input_file = PATH, characters = NCHAR, write_2file = "")
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( nchar(res$data) == NCHAR )
  })
  
  
  testthat::test_that("it returns the correct output if write_2file is a valid path to a file", {
    
    NCHAR = 5
    
    res = read_characters(input_file = PATH, characters = NCHAR, write_2file = read_CHARS_ROWS)
    
    res_write = read_characters(input_file = read_CHARS_ROWS, characters = NCHAR * 2, write_2file = "")
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( nchar(res_write$data) == NCHAR )
  })
  
  
  
  #-----------------------------
  # read rows [ error handling ]
  #-----------------------------
  
  
  testthat::test_that("in case that the input_file parameter is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( read_rows(input_file = NULL, read_delimiter = "\n", rows = 100, write_2file = "") )
  })
  
  
  
  testthat::test_that("in case that the read_delimiter parameter is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( read_rows(input_file = PATH, read_delimiter = NULL, rows = 100, write_2file = "") )
  })
  
  
  
  testthat::test_that("in case that the rows parameter is less than 1 it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( read_rows(input_file = PATH, read_delimiter = "\n", rows = 0, write_2file = "") )
  })
  
  
  testthat::test_that("in case that the write_2file parameter is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( read_rows(input_file = PATH, read_delimiter = "\n", rows = 1, write_2file = NULL) )
  })
  
  
  
  #-------------------------
  # read rows [ expect true]
  #-------------------------
  
  
  testthat::test_that("it returns the correct output if write_2file is an empty string", {
    
    NROW = 5
    
    res = read_rows(input_file = PATH, read_delimiter = "\n", rows = NROW, write_2file = "")
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( length(res$data) == NROW )
  })
  
  
  testthat::test_that("it returns the correct output if write_2file is a valid path to a file", {
    
    NROW = 5
    
    res = read_rows(input_file = PATH, read_delimiter = "\n", rows = NROW, write_2file = read_CHARS_ROWS)
    
    res_write = read_rows(input_file = read_CHARS_ROWS, read_delimiter = "\n", rows = NROW * 2, write_2file = "")
    
    #-------------------------------------------------------------------- debug tests
    cat("test-sparse_functions_AND_read_text.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( length(res_write$data) == NROW )
  })
  
  break    # exit loop for tests ( count iterations / tests for debugging )
}
