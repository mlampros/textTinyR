#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _textTinyR_Adj_Sparsity(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_Associations_Cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_batch_2file(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_big_parser(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_big_splitter_bytes(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_big_tokenize(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_Collocations_ngrams(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_convert_bytes(SEXP, SEXP);
extern SEXP _textTinyR_Cosine_dist(SEXP, SEXP, SEXP);
extern SEXP _textTinyR_Count_characters(SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_dense_2sparse_mat(SEXP);
extern SEXP _textTinyR_Dice_similarity(SEXP, SEXP, SEXP);
extern SEXP _textTinyR_Dissimilarity_mat(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_file_parser(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_Frequency_distribution(SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_Levenshtein_dist(SEXP, SEXP);
extern SEXP _textTinyR_load_sparse_(SEXP);
extern SEXP _textTinyR_Look_up_tbl(SEXP, SEXP);
extern SEXP _textTinyR_Most_Freq_Terms(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_Path_2vector(SEXP, SEXP);
extern SEXP _textTinyR_read_CHARS(SEXP, SEXP, SEXP);
extern SEXP _textTinyR_read_ROWS(SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_res_term_matrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_res_token(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_res_token_list(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_res_token_vector(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_save_sparse_(SEXP, SEXP);
extern SEXP _textTinyR_sparsity_float(SEXP);
extern SEXP _textTinyR_sp_means(SEXP, SEXP);
extern SEXP _textTinyR_sp_sums(SEXP, SEXP);
extern SEXP _textTinyR_tf_idf_exclude(SEXP, SEXP);
extern SEXP _textTinyR_vocabulary_counts(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _textTinyR_vocabulary_counts_big_tokenize(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_textTinyR_Adj_Sparsity",                   (DL_FUNC) &_textTinyR_Adj_Sparsity,                    5},
    {"_textTinyR_Associations_Cpp",               (DL_FUNC) &_textTinyR_Associations_Cpp,               11},
    {"_textTinyR_batch_2file",                    (DL_FUNC) &_textTinyR_batch_2file,                    33},
    {"_textTinyR_big_parser",                     (DL_FUNC) &_textTinyR_big_parser,                      7},
    {"_textTinyR_big_splitter_bytes",             (DL_FUNC) &_textTinyR_big_splitter_bytes,              6},
    {"_textTinyR_big_tokenize",                   (DL_FUNC) &_textTinyR_big_tokenize,                   35},
    {"_textTinyR_Collocations_ngrams",            (DL_FUNC) &_textTinyR_Collocations_ngrams,             5},
    {"_textTinyR_convert_bytes",                  (DL_FUNC) &_textTinyR_convert_bytes,                   2},
    {"_textTinyR_Cosine_dist",                    (DL_FUNC) &_textTinyR_Cosine_dist,                     3},
    {"_textTinyR_Count_characters",               (DL_FUNC) &_textTinyR_Count_characters,                4},
    {"_textTinyR_dense_2sparse_mat",              (DL_FUNC) &_textTinyR_dense_2sparse_mat,               1},
    {"_textTinyR_Dice_similarity",                (DL_FUNC) &_textTinyR_Dice_similarity,                 3},
    {"_textTinyR_Dissimilarity_mat",              (DL_FUNC) &_textTinyR_Dissimilarity_mat,               8},
    {"_textTinyR_file_parser",                    (DL_FUNC) &_textTinyR_file_parser,                     7},
    {"_textTinyR_Frequency_distribution",         (DL_FUNC) &_textTinyR_Frequency_distribution,          4},
    {"_textTinyR_Levenshtein_dist",               (DL_FUNC) &_textTinyR_Levenshtein_dist,                2},
    {"_textTinyR_load_sparse_",                   (DL_FUNC) &_textTinyR_load_sparse_,                    1},
    {"_textTinyR_Look_up_tbl",                    (DL_FUNC) &_textTinyR_Look_up_tbl,                     2},
    {"_textTinyR_Most_Freq_Terms",                (DL_FUNC) &_textTinyR_Most_Freq_Terms,                 6},
    {"_textTinyR_Path_2vector",                   (DL_FUNC) &_textTinyR_Path_2vector,                    2},
    {"_textTinyR_read_CHARS",                     (DL_FUNC) &_textTinyR_read_CHARS,                      3},
    {"_textTinyR_read_ROWS",                      (DL_FUNC) &_textTinyR_read_ROWS,                       4},
    {"_textTinyR_res_term_matrix",                (DL_FUNC) &_textTinyR_res_term_matrix,                34},
    {"_textTinyR_res_token",                      (DL_FUNC) &_textTinyR_res_token,                      35},
    {"_textTinyR_res_token_list",                 (DL_FUNC) &_textTinyR_res_token_list,                 31},
    {"_textTinyR_res_token_vector",               (DL_FUNC) &_textTinyR_res_token_vector,               31},
    {"_textTinyR_save_sparse_",                   (DL_FUNC) &_textTinyR_save_sparse_,                    2},
    {"_textTinyR_sparsity_float",                 (DL_FUNC) &_textTinyR_sparsity_float,                  1},
    {"_textTinyR_sp_means",                       (DL_FUNC) &_textTinyR_sp_means,                        2},
    {"_textTinyR_sp_sums",                        (DL_FUNC) &_textTinyR_sp_sums,                         2},
    {"_textTinyR_tf_idf_exclude",                 (DL_FUNC) &_textTinyR_tf_idf_exclude,                  2},
    {"_textTinyR_vocabulary_counts",              (DL_FUNC) &_textTinyR_vocabulary_counts,              30},
    {"_textTinyR_vocabulary_counts_big_tokenize", (DL_FUNC) &_textTinyR_vocabulary_counts_big_tokenize,  4},
    {NULL, NULL, 0}
};

void R_init_textTinyR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}