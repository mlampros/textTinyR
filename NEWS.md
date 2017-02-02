
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




