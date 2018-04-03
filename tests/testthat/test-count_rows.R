

if (.Platform$OS.type == "windows") {
  
  PATH = paste0(getwd(), path.expand("\\example_word_vecs.txt"))
  INVALID_PATH = paste0(getwd(), path.expand("\\INVALID\\example_word_vecs.txt"))
}

if (.Platform$OS.type == "unix") {
  
  PATH = paste0(getwd(), path.expand("/example_word_vecs.txt"))
  INVALID_PATH = paste0(getwd(), path.expand("/INVALID/example_word_vecs.txt"))
}




context('count-rows function')


#============================
# 'Count_Rows' error handling
#============================


testthat::test_that("in case that the path to a file is invalid, it returns an error", {
  
  testthat::expect_error( Count_Rows(INVALID_PATH, verbose = F) )
})


testthat::test_that("in case that the verbose parameter is not a boolean, it returns an error", {
  
  testthat::expect_error( Count_Rows(PATH, verbose = 'F') )
})



#=========================
# 'Count_Rows' expect true
#=========================


testthat::test_that("the 'Count_Rows' function returns the correct output", {
  
  res = Count_Rows(PATH, verbose = F)
  
  testthat::expect_true( is.numeric(res) && res > 0 )
})

