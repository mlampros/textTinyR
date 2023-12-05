#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _textTinyR_Adj_Sparsity(void *, void *, void *, void *, void *);
extern SEXP _textTinyR_append_data(void *, void *);
extern SEXP _textTinyR_Associations_Cpp(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_batch_2file(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_batch_calculation(void *, void *);
extern SEXP _textTinyR_big_parser(void *, void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_big_splitter_bytes(void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_big_tokenize(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_Collocations_ngrams(void *, void *, void *, void *, void *);
extern SEXP _textTinyR_convert_bytes(void *, void *);
extern SEXP _textTinyR_COR_MATR(void *, void *, void *);
extern SEXP _textTinyR_COS(void *, void *, void *, void *);
extern SEXP _textTinyR_cosine_dist(void *, void *, void *);
extern SEXP _textTinyR_Cosine_dist(void *, void *, void *);
extern SEXP _textTinyR_Count_characters(void *, void *, void *, void *);
extern SEXP _textTinyR_count_rows(void *, void *);
extern SEXP _textTinyR_COUNTS_INTERSECT(void *, void *, void *, void *);
extern SEXP _textTinyR_dense_2sparse_mat(void *);
extern SEXP _textTinyR_DICE(void *, void *);
extern SEXP _textTinyR_Dice_similarity(void *, void *, void *);
extern SEXP _textTinyR_Dissimilarity_mat(void *, void *, void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_DIST(void *, void *, void *, void *, void *);
extern SEXP _textTinyR_DISTINCT_WORD_INTERSECT(void *, void *);
extern SEXP _textTinyR_file_parser(void *, void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_Frequency_distribution(void *, void *, void *, void *);
extern SEXP _textTinyR_idf_global_term_weights(void *, void *);
extern SEXP _textTinyR_inner_cm(void *, void *, void *);
extern SEXP _textTinyR_inner_jd(void *, void *, void *, void *);
extern SEXP _textTinyR_inner_reduce_dims(void *, void *, void *, void *);
extern SEXP _textTinyR_INTERSECT(void *, void *);
extern SEXP _textTinyR_JACCARD(void *, void *);
extern SEXP _textTinyR_jaccard_dice(void *, void *, void *, void *);
extern SEXP _textTinyR_keep_idxs(void *, void *);
extern SEXP _textTinyR_Levenshtein_dist(void *, void *);
extern SEXP _textTinyR_load_sparse_(void *);
extern SEXP _textTinyR_Look_up_tbl(void *, void *);
extern SEXP _textTinyR_modulus(void *, void *);
extern SEXP _textTinyR_Most_Freq_Terms(void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_Not_Duplicated(void *);
extern SEXP _textTinyR_NUM_LETTERS_DISTINCT(void *);
extern SEXP _textTinyR_Path_2vector(void *, void *);
extern SEXP _textTinyR_RATIO_DISTINCT(void *, void *, void *, void *);
extern SEXP _textTinyR_read_CHARS(void *, void *, void *);
extern SEXP _textTinyR_read_ROWS(void *, void *, void *, void *);
extern SEXP _textTinyR_read_ROWS_wv(void *, void *);
extern SEXP _textTinyR_reduce_dims_with_correlation(void *, void *, void *, void *, void *);
extern SEXP _textTinyR_reduced_word_vectors(void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_res_term_matrix(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_res_token(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_res_token_list(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_res_token_vector(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_save_sparse_(void *, void *);
extern SEXP _textTinyR_sp_means(void *, void *);
extern SEXP _textTinyR_sp_sums(void *, void *);
extern SEXP _textTinyR_sparsity_float(void *);
extern SEXP _textTinyR_sublist(void *, void *);
extern SEXP _textTinyR_tf_idf_exclude(void *, void *);
extern SEXP _textTinyR_UNION(void *, void *);
extern SEXP _textTinyR_UNIQUE(void *);
extern SEXP _textTinyR_vec_parser(void *, void *, void *, void *, void *);
extern SEXP _textTinyR_vocabulary_counts(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern SEXP _textTinyR_vocabulary_counts_big_tokenize(void *, void *, void *, void *);
extern SEXP _textTinyR_word_vectors_methods(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CallMethodDef CallEntries[] = {
    {"_textTinyR_Adj_Sparsity",                   (DL_FUNC) &_textTinyR_Adj_Sparsity,                    5},
    {"_textTinyR_append_data",                    (DL_FUNC) &_textTinyR_append_data,                     2},
    {"_textTinyR_Associations_Cpp",               (DL_FUNC) &_textTinyR_Associations_Cpp,               11},
    {"_textTinyR_batch_2file",                    (DL_FUNC) &_textTinyR_batch_2file,                    33},
    {"_textTinyR_batch_calculation",              (DL_FUNC) &_textTinyR_batch_calculation,               2},
    {"_textTinyR_big_parser",                     (DL_FUNC) &_textTinyR_big_parser,                      7},
    {"_textTinyR_big_splitter_bytes",             (DL_FUNC) &_textTinyR_big_splitter_bytes,              6},
    {"_textTinyR_big_tokenize",                   (DL_FUNC) &_textTinyR_big_tokenize,                   35},
    {"_textTinyR_Collocations_ngrams",            (DL_FUNC) &_textTinyR_Collocations_ngrams,             5},
    {"_textTinyR_convert_bytes",                  (DL_FUNC) &_textTinyR_convert_bytes,                   2},
    {"_textTinyR_COR_MATR",                       (DL_FUNC) &_textTinyR_COR_MATR,                        3},
    {"_textTinyR_COS",                            (DL_FUNC) &_textTinyR_COS,                             4},
    {"_textTinyR_cosine_dist",                    (DL_FUNC) &_textTinyR_cosine_dist,                     3},
    {"_textTinyR_Cosine_dist",                    (DL_FUNC) &_textTinyR_Cosine_dist,                     3},
    {"_textTinyR_Count_characters",               (DL_FUNC) &_textTinyR_Count_characters,                4},
    {"_textTinyR_count_rows",                     (DL_FUNC) &_textTinyR_count_rows,                      2},
    {"_textTinyR_COUNTS_INTERSECT",               (DL_FUNC) &_textTinyR_COUNTS_INTERSECT,                4},
    {"_textTinyR_dense_2sparse_mat",              (DL_FUNC) &_textTinyR_dense_2sparse_mat,               1},
    {"_textTinyR_DICE",                           (DL_FUNC) &_textTinyR_DICE,                            2},
    {"_textTinyR_Dice_similarity",                (DL_FUNC) &_textTinyR_Dice_similarity,                 3},
    {"_textTinyR_Dissimilarity_mat",              (DL_FUNC) &_textTinyR_Dissimilarity_mat,               8},
    {"_textTinyR_DIST",                           (DL_FUNC) &_textTinyR_DIST,                            5},
    {"_textTinyR_DISTINCT_WORD_INTERSECT",        (DL_FUNC) &_textTinyR_DISTINCT_WORD_INTERSECT,         2},
    {"_textTinyR_file_parser",                    (DL_FUNC) &_textTinyR_file_parser,                     7},
    {"_textTinyR_Frequency_distribution",         (DL_FUNC) &_textTinyR_Frequency_distribution,          4},
    {"_textTinyR_idf_global_term_weights",        (DL_FUNC) &_textTinyR_idf_global_term_weights,         2},
    {"_textTinyR_inner_cm",                       (DL_FUNC) &_textTinyR_inner_cm,                        3},
    {"_textTinyR_inner_jd",                       (DL_FUNC) &_textTinyR_inner_jd,                        4},
    {"_textTinyR_inner_reduce_dims",              (DL_FUNC) &_textTinyR_inner_reduce_dims,               4},
    {"_textTinyR_INTERSECT",                      (DL_FUNC) &_textTinyR_INTERSECT,                       2},
    {"_textTinyR_JACCARD",                        (DL_FUNC) &_textTinyR_JACCARD,                         2},
    {"_textTinyR_jaccard_dice",                   (DL_FUNC) &_textTinyR_jaccard_dice,                    4},
    {"_textTinyR_keep_idxs",                      (DL_FUNC) &_textTinyR_keep_idxs,                       2},
    {"_textTinyR_Levenshtein_dist",               (DL_FUNC) &_textTinyR_Levenshtein_dist,                2},
    {"_textTinyR_load_sparse_",                   (DL_FUNC) &_textTinyR_load_sparse_,                    1},
    {"_textTinyR_Look_up_tbl",                    (DL_FUNC) &_textTinyR_Look_up_tbl,                     2},
    {"_textTinyR_modulus",                        (DL_FUNC) &_textTinyR_modulus,                         2},
    {"_textTinyR_Most_Freq_Terms",                (DL_FUNC) &_textTinyR_Most_Freq_Terms,                 6},
    {"_textTinyR_Not_Duplicated",                 (DL_FUNC) &_textTinyR_Not_Duplicated,                  1},
    {"_textTinyR_NUM_LETTERS_DISTINCT",           (DL_FUNC) &_textTinyR_NUM_LETTERS_DISTINCT,            1},
    {"_textTinyR_Path_2vector",                   (DL_FUNC) &_textTinyR_Path_2vector,                    2},
    {"_textTinyR_RATIO_DISTINCT",                 (DL_FUNC) &_textTinyR_RATIO_DISTINCT,                  4},
    {"_textTinyR_read_CHARS",                     (DL_FUNC) &_textTinyR_read_CHARS,                      3},
    {"_textTinyR_read_ROWS",                      (DL_FUNC) &_textTinyR_read_ROWS,                       4},
    {"_textTinyR_read_ROWS_wv",                   (DL_FUNC) &_textTinyR_read_ROWS_wv,                    2},
    {"_textTinyR_reduce_dims_with_correlation",   (DL_FUNC) &_textTinyR_reduce_dims_with_correlation,    5},
    {"_textTinyR_reduced_word_vectors",           (DL_FUNC) &_textTinyR_reduced_word_vectors,            6},
    {"_textTinyR_res_term_matrix",                (DL_FUNC) &_textTinyR_res_term_matrix,                34},
    {"_textTinyR_res_token",                      (DL_FUNC) &_textTinyR_res_token,                      35},
    {"_textTinyR_res_token_list",                 (DL_FUNC) &_textTinyR_res_token_list,                 31},
    {"_textTinyR_res_token_vector",               (DL_FUNC) &_textTinyR_res_token_vector,               31},
    {"_textTinyR_save_sparse_",                   (DL_FUNC) &_textTinyR_save_sparse_,                    2},
    {"_textTinyR_sp_means",                       (DL_FUNC) &_textTinyR_sp_means,                        2},
    {"_textTinyR_sp_sums",                        (DL_FUNC) &_textTinyR_sp_sums,                         2},
    {"_textTinyR_sparsity_float",                 (DL_FUNC) &_textTinyR_sparsity_float,                  1},
    {"_textTinyR_sublist",                        (DL_FUNC) &_textTinyR_sublist,                         2},
    {"_textTinyR_tf_idf_exclude",                 (DL_FUNC) &_textTinyR_tf_idf_exclude,                  2},
    {"_textTinyR_UNION",                          (DL_FUNC) &_textTinyR_UNION,                           2},
    {"_textTinyR_UNIQUE",                         (DL_FUNC) &_textTinyR_UNIQUE,                          1},
    {"_textTinyR_vec_parser",                     (DL_FUNC) &_textTinyR_vec_parser,                      5},
    {"_textTinyR_vocabulary_counts",              (DL_FUNC) &_textTinyR_vocabulary_counts,              30},
    {"_textTinyR_vocabulary_counts_big_tokenize", (DL_FUNC) &_textTinyR_vocabulary_counts_big_tokenize,  4},
    {"_textTinyR_word_vectors_methods",           (DL_FUNC) &_textTinyR_word_vectors_methods,           12},
    {NULL, NULL, 0}
};

void R_init_textTinyR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
