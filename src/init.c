#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP textTinyR_Adj_Sparsity(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_Associations_Cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_batch_2file(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_big_parser(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_big_splitter_bytes(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_big_tokenize(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_Collocations_ngrams(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_convert_bytes(SEXP, SEXP);
extern SEXP textTinyR_Cosine_dist(SEXP, SEXP, SEXP);
extern SEXP textTinyR_Count_characters(SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_dense_2sparse_mat(SEXP);
extern SEXP textTinyR_Dice_similarity(SEXP, SEXP, SEXP);
extern SEXP textTinyR_Dissimilarity_mat(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_file_parser(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_Frequency_distribution(SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_Levenshtein_dist(SEXP, SEXP);
extern SEXP textTinyR_load_sparse_(SEXP);
extern SEXP textTinyR_Look_up_tbl(SEXP, SEXP);
extern SEXP textTinyR_Most_Freq_Terms(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_Path_2vector(SEXP, SEXP);
extern SEXP textTinyR_read_CHARS(SEXP, SEXP, SEXP);
extern SEXP textTinyR_read_ROWS(SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_res_term_matrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_res_token(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_res_token_list(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_res_token_vector(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_save_sparse_(SEXP, SEXP);
extern SEXP textTinyR_sp_means(SEXP, SEXP);
extern SEXP textTinyR_sp_sums(SEXP, SEXP);
extern SEXP textTinyR_sparsity_float(SEXP);
extern SEXP textTinyR_tf_idf_exclude(SEXP, SEXP);
extern SEXP textTinyR_vocabulary_counts(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP textTinyR_vocabulary_counts_big_tokenize(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"textTinyR_Adj_Sparsity",                   (DL_FUNC) &textTinyR_Adj_Sparsity,                    5},
    {"textTinyR_Associations_Cpp",               (DL_FUNC) &textTinyR_Associations_Cpp,               12},
    {"textTinyR_batch_2file",                    (DL_FUNC) &textTinyR_batch_2file,                    33},
    {"textTinyR_big_parser",                     (DL_FUNC) &textTinyR_big_parser,                      7},
    {"textTinyR_big_splitter_bytes",             (DL_FUNC) &textTinyR_big_splitter_bytes,              6},
    {"textTinyR_big_tokenize",                   (DL_FUNC) &textTinyR_big_tokenize,                   35},
    {"textTinyR_Collocations_ngrams",            (DL_FUNC) &textTinyR_Collocations_ngrams,             5},
    {"textTinyR_convert_bytes",                  (DL_FUNC) &textTinyR_convert_bytes,                   2},
    {"textTinyR_Cosine_dist",                    (DL_FUNC) &textTinyR_Cosine_dist,                     3},
    {"textTinyR_Count_characters",               (DL_FUNC) &textTinyR_Count_characters,                4},
    {"textTinyR_dense_2sparse_mat",              (DL_FUNC) &textTinyR_dense_2sparse_mat,               1},
    {"textTinyR_Dice_similarity",                (DL_FUNC) &textTinyR_Dice_similarity,                 3},
    {"textTinyR_Dissimilarity_mat",              (DL_FUNC) &textTinyR_Dissimilarity_mat,               8},
    {"textTinyR_file_parser",                    (DL_FUNC) &textTinyR_file_parser,                     7},
    {"textTinyR_Frequency_distribution",         (DL_FUNC) &textTinyR_Frequency_distribution,          4},
    {"textTinyR_Levenshtein_dist",               (DL_FUNC) &textTinyR_Levenshtein_dist,                2},
    {"textTinyR_load_sparse_",                   (DL_FUNC) &textTinyR_load_sparse_,                    1},
    {"textTinyR_Look_up_tbl",                    (DL_FUNC) &textTinyR_Look_up_tbl,                     2},
    {"textTinyR_Most_Freq_Terms",                (DL_FUNC) &textTinyR_Most_Freq_Terms,                 6},
    {"textTinyR_Path_2vector",                   (DL_FUNC) &textTinyR_Path_2vector,                    2},
    {"textTinyR_read_CHARS",                     (DL_FUNC) &textTinyR_read_CHARS,                      3},
    {"textTinyR_read_ROWS",                      (DL_FUNC) &textTinyR_read_ROWS,                       4},
    {"textTinyR_res_term_matrix",                (DL_FUNC) &textTinyR_res_term_matrix,                34},
    {"textTinyR_res_token",                      (DL_FUNC) &textTinyR_res_token,                      35},
    {"textTinyR_res_token_list",                 (DL_FUNC) &textTinyR_res_token_list,                 31},
    {"textTinyR_res_token_vector",               (DL_FUNC) &textTinyR_res_token_vector,               31},
    {"textTinyR_save_sparse_",                   (DL_FUNC) &textTinyR_save_sparse_,                    2},
    {"textTinyR_sp_means",                       (DL_FUNC) &textTinyR_sp_means,                        2},
    {"textTinyR_sp_sums",                        (DL_FUNC) &textTinyR_sp_sums,                         2},
    {"textTinyR_sparsity_float",                 (DL_FUNC) &textTinyR_sparsity_float,                  1},
    {"textTinyR_tf_idf_exclude",                 (DL_FUNC) &textTinyR_tf_idf_exclude,                  2},
    {"textTinyR_vocabulary_counts",              (DL_FUNC) &textTinyR_vocabulary_counts,              34},
    {"textTinyR_vocabulary_counts_big_tokenize", (DL_FUNC) &textTinyR_vocabulary_counts_big_tokenize,  4},
    {NULL, NULL, 0}
};

void R_init_textTinyR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

