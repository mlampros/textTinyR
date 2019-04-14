
## textTinyR 1.1.3

* Exception which applies to *tokenize_transform_text()* and *tokenize_transform_vec_docs()* functions on all Operating Systems (Linux, Macintosh, Windows) in case of parallelization ( OpenMP ) when I additionally write data to a folder or file ( 'path_2folder' or 'vocabulary_path_file' ). Both Rcpp functions of the 'tokenize_transform_text()' and 'tokenize_transform_vec_docs()' do have an OpenMP-critical-clause which ensures that data appended to a variable are protected ( only one thread at a time will enter the section ). See the code lines 258 and 312 of the 'export_all_funcs.cpp' file. However, this must not apply (parallelization) when the 'path_2folder' or the 'vocabulary_path_file' are not equal to "" (empty string). Due to the fact that writing to the file takes place internally I can not enclose the 'save' functions to an OpenMP-crtical-clause. Therefore, whenever I save to an output file set the number of threads to 1 and print out a warning so that the user knows that parallelization is disabled [ see issue : 'https://github.com/mlampros/textTinyR/issues/8' ]
* Stop the execution of the *tokenize_transform_text()* and *tokenize_transform_vec_docs()* functions whenever the user specifies the *path_2folder* parameter (valid path to a folder) and the 'output_token_single_file.txt' file already exists ( otherwise new data will be appended at the end of the file ) [ see issue : 'https://github.com/mlampros/textTinyR/issues/8' ]
* I added a note in the vignettes about the [new version of the fastText R package](https://github.com/mlampros/fastText) ( the [old version is archived](https://github.com/mlampros/fastTextR) )
* I commented out 14 tests in the *test-utf_locale.R* file for the Debian distribution (Linux) due to an error specific to 'Latin-1 locale'. See also my comments in the *helper-function_for_tests.R* file. 


## textTinyR 1.1.2

* I modified the *porter2_stemmer.cpp* and especially the *Porter2Stemmer::stem()* function as it was initially incorrectly modified
* I attempted to fix the clang-UBSAN error, however it's not reproducible with the latest install of clang==6.0, llvm==6.0 and CRAN configuration of ASAN, UBSAN. I had to comment this particular test case ( test-tokenization_transformation.R, lines 938-963 ). *The clang-UBSAN error was the following* : 

```R

test-tokenization_transformation.R : test id 329 
/usr/local/bin/../include/c++/v1/string:2992:30: runtime error: addition of unsigned offset to 0x62500f06b1b9 overflowed to 0x62500f06b1b8 SUMMARY: UndefinedBehaviorSanitizer: undefined-behavior /usr/local/bin/../include/c++/v1/string:2992:30 in

```

## textTinyR 1.1.1

* I removed the *-lboost_system* flag from the *Makevars* file
* I modified the *text_file_parser* function (the documentation and examples too) due to an error


## textTinyR 1.1.0

* *boost-locale* is no longer a system requirement for the textTinyR package
* The *text_file_parser* function can now accept besides a valid path to a file also a vector of character strings. Moreover, besides writing to a file it can also return a vector of character strings. Furthermore, the *start_query* and *end_query* parameters can take more than one query-terms as input.
* I added utility functions for *Word Vector Representations* (i.e. *GloVe*, *fasttext*), frequently referred to as *doc2vec*, and functions for the (pairwise) calculation of text document dissimilarities.


## textTinyR 1.0.9

I added the *global_term_weights()* method in the *sparse_term_matrix R6* class


## textTinyR 1.0.8

I removed the *threads* parameter from the *term_associations* method of the *sparse_term_matrix* R6-class.
I modified the *OpenMP* clauses of the .cpp files to address the ASAN errors.


## textTinyR 1.0.7

I added the *triplet_data()* method in the *sparse_term_matrix R6* class


## textTinyR 1.0.6

I removed the *ngram_sequential* and *ngram_overlap* stemmers from the *vocabulary_parser* function.
I fixed a bug in the *char_n_grams* of the *token_stats.h* source file.


## textTinyR 1.0.5

I removed the *ngram_sequential* and *ngram_overlap* stemmers from the *sparse_term_matrix* and *tokenize_transform_vec_docs* functions. I overlooked the fact that the n-gram stemming is based on the whole corpus and not on
each vector of the document(s), which is the case for the *sparse_term_matrix* and *tokenize_transform_vec_docs* functions. 
I added a *zzz.R* file with a *packageStartupMessage* to inform the users about the previous change in n-gram stemming.
I also updated the package documentation and Vignette.
I modified the *secondary_n_grams* of the *tokenization.h* source file due to a bug.
I've used the *enc2utf8* function) to encode (utf-8) the terms of the sparse matrix.


## textTinyR 1.0.4

I modified the *res_token_vector()*, *res_token_list()* [ *export_all_funcs.cpp* file ] and *append_2file()* [ *tokenization.h* file ] functions, because the *tokenize_transform_vec_docs()* function returned an incorrect output in case that the *path_2folder* parameter was not the empty string.


## textTinyR 1.0.3

I corrected the UBSAN-memory errors, which occured in the *adj_Sparsity()* function of the *term_matrix.h* header file (the errors happen, when passing empty vectors to the armadillo *batch_insertion()* function)


## textTinyR 1.0.2

I included detailed installation instructions for the Macintosh OSx
I modified the source code to correct the boost-locale errors, which occurred during testing on Macintosh OSx


## textTinyR 1.0.1

I added the following system-flag in the Makevars.in file to avoid linking errors for the Mac OS: -lboost_system
I modified the *term_associations* and *Term_Matrix_Adjust* methods to avoid indexing errors
I corrected mistakes in the Vignette


## textTinyR 1.0.0




