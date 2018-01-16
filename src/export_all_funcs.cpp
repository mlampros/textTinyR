# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(BH)]]


#include <boost/algorithm/string/join.hpp>
#include <boost/range/algorithm_ext/erase.hpp>
#include <boost/range/adaptor/map.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/algorithm/string.hpp>

                                      // _WIN32 :  Both 32 bit and 64 bit [ http://stackoverflow.com/questions/142508/how-do-i-check-os-with-a-preprocessor-directive ]
#ifndef _WIN32                        // the boost-locale header does not work in windows [ gcc (v. 4.6.3) in RTools (v. 3.3) is probably not built with locale support ], see http://stackoverflow.com/questions/31670839/how-do-i-read-a-windows-1252-file-using-rcpp
  #include <boost/locale.hpp>
#endif

#include <dirent.h>
#include <sys/types.h>
#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <streambuf>
#include <limits>
#include <chrono>
#include <algorithm>
#include <map>
#include <unordered_map>

#ifdef _OPENMP
#include <omp.h>
#endif

//-------------------------
// include dependencies:

#include "porter2_stemmer.h"
#include "tokenization.h"
#include "batch_tokenization.h"
#include "ngram_stemmer.h"
#include "token_stats.h"
#include "token_big_files.h"
#include "term_matrix.h"
#include "term_associations.h"
#include "sort_2dim_vecs.h"

//-------------------------



//--------------------------------------
// functions for the class TOKEN_stats:
//--------------------------------------


// returns the tokens of a folder or file to a vector
//

// [[Rcpp::export]]
std::vector<std::string> Path_2vector(std::string path_2folder = "", std::string path_2file = "") {

  TOKEN_stats tok;

  return tok.path_2vector(path_2folder, path_2file);
}


// frequency distribution [ using EITHER a folder of files, a file OR a character vector of strings ]
// x can't be initialized (exported) to an empty vector and this should be done in the R session
//

// [[Rcpp::export]]
std::unordered_map<std::string, int> Frequency_distribution(std::vector<std::string> &x, std::string path_2folder = "",

                                                            std::string path_2file = "", char file_delimiter = '\n') {

  TOKEN_stats tok;

  return tok.frequency_distribution(x, path_2folder, path_2file, file_delimiter);
}


// count number of characters [ using EITHER a folder of files, a file OR a character vector of strings ]
// x can't be initialized (exported) to an empty vector and this should be done in the R session
//

// [[Rcpp::export]]
std::unordered_map<int, std::vector<std::string>> Count_characters(std::vector<std::string> x, std::string path_2folder = "",

                                                                   std::string path_2file = "", char file_delimiter = '\n') {

  TOKEN_stats tok;

  return tok.count_characters(x, path_2folder, path_2file, file_delimiter);
}


// collocations of words [ using EITHER a folder of files, a file OR a character vector of strings ]
// x can't be initialized (exported) to an empty vector and this should be done in the R session
//

// [[Rcpp::export]]
std::unordered_map<std::string, std::unordered_map<std::string, int> > Collocations_ngrams(std::vector<std::string> x, std::string path_2folder = "",

                                                                                           std::string path_2file = "", char file_delimiter = '\n',

                                                                                           std::string n_gram_delimiter = "_") {
  TOKEN_stats tok;

  return tok.collocations_ngrams(x, path_2folder, path_2file, file_delimiter, n_gram_delimiter);
}


// dice similarity for n-grams
//

// [[Rcpp::export]]
double Dice_similarity(std::string x, std::string y, int n_grams) {

  TOKEN_stats tok;

  return tok.dice_similarity(x, y, n_grams);
}


// levenshtein distance for words
//

// [[Rcpp::export]]
double Levenshtein_dist(std::string &s, std::string &t) {

  TOKEN_stats tok;

  return tok.levenshtein_dist(s, t);
}


// cosine similarity for strings which include more than 1 words
//

// [[Rcpp::export]]
double Cosine_dist(std::string &x, std::string &y, std::string split_separator = " ") {

  TOKEN_stats tok;

  return tok.cosine_dist(x, y, split_separator);
}


// dissimilarity matrix for the methods c('dice', 'levenshtein', 'cosine')
//

// [[Rcpp::export]]
arma::mat Dissimilarity_mat(std::vector<std::string> words, int dice_n_gram = 2, std::string method = "dice", std::string split_separator = " ",

                            double dice_thresh = 0.3, bool upper = true, bool diagonal = true, int threads = 1) {

  TOKEN_stats tok;

  return tok.dissimilarity_mat(words, method, split_separator, dice_n_gram, dice_thresh, upper, diagonal, threads);
}


// look-up table using n-grams for a vector of strings
//

// [[Rcpp::export]]
std::unordered_map<std::string, std::vector<std::string>> Look_up_tbl(std::vector<std::string> VEC, int n_grams) {

  TOKEN_stats tok;

  return tok.look_up_tbl(VEC, n_grams);
}



//--------------------------------------------------------------------
// function for the tokenization header (input is a character string):
//--------------------------------------------------------------------


// [[Rcpp::export]]
std::vector<std::string> res_token(std::string x, std::vector<std::string> language, std::string language_spec, std::string LOCALE_UTF, bool FLAG_path, char read_file_delimiter, int max_num_char,

                                   std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false, bool cpp_remove_punctuation = false,

                                   bool remove_punctuation_vector = false, bool cpp_remove_numbers = false, bool cpp_trim_token = false, bool cpp_tokenization_function = false,

                                   std::string cpp_string_separator = "-*", bool cpp_remove_stopwords = false, int min_num_char = 1, std::string stemmer = "NULL", int min_n_gram = 1,

                                   int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0, std::string n_gram_delimiter = " ", std::string concat_delimiter = "NULL",

                                   std::string path_2file = "", int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1, int threads = 1,

                                   bool verbose = false, bool save_2single_file = false, std::string path_extend = "output_token.txt", std::string vocabulary_path = "") {


  big_files bgf;

  return bgf.res_TOKEN(x, language, language_spec, LOCALE_UTF, FLAG_path, read_file_delimiter, max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation,

                       remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords,

                       min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, concat_delimiter, path_2file, stemmer_ngram,

                       stemmer_gamma, stemmer_truncate, stemmer_batches, threads, verbose, save_2single_file, path_extend, vocabulary_path);
}




//--------------------------------------------------------------------------------------------
// tokenization and transformation for a vector of documents ( a vector of character strings )
//--------------------------------------------------------------------------------------------

// [[Rcpp::export]]
std::vector<std::string> res_token_vector(std::vector<std::string>& VEC, std::vector<std::string>& language, std::string& language_spec, std::string& LOCALE_UTF, int max_num_char,

                                                        std::string& remove_char, bool cpp_to_lower, bool cpp_to_upper, bool cpp_remove_punctuation,

                                                        bool remove_punctuation_vector, bool cpp_remove_numbers, bool cpp_trim_token, bool cpp_tokenization_function,

                                                        std::string& cpp_string_separator, bool cpp_remove_stopwords, int min_num_char, std::string& stemmer, int min_n_gram,

                                                        int max_n_gram, int skip_n_gram, int skip_distance, std::string& n_gram_delimiter, std::string& concat_delimiter,

                                                        std::string& path_2file, int stemmer_ngram, double stemmer_gamma, int stemmer_truncate, int stemmer_batches, int threads,

                                                        bool verbose, std::string& vocabulary_path) {
  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  big_files bgf;

  std::vector<std::string> res_vec(VEC.size());               // returns a vector of character strings

  bool FLAG_write_file = path_2file == "" ? false : true;

  unsigned long long f;
  
  #ifdef _OPENMP
  #pragma omp parallel for schedule(static) shared(VEC, bgf, vocabulary_path, FLAG_write_file, stemmer_batches, stemmer_truncate, stemmer_gamma, stemmer_ngram, path_2file, concat_delimiter, n_gram_delimiter, skip_distance, skip_n_gram, max_n_gram, min_n_gram, stemmer, min_num_char, cpp_remove_stopwords, cpp_string_separator, cpp_tokenization_function, cpp_trim_token, remove_punctuation_vector, cpp_remove_numbers, language, language_spec, LOCALE_UTF, max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation, res_vec) private(f)
  #endif
  for (f = 0; f < VEC.size(); f++) {
    
    std::string tmp_str = bgf.inner_res_tok_vec(f, VEC, language, language_spec, LOCALE_UTF, max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation,
                                                        
                                                remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords,
                                                
                                                min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, concat_delimiter, path_2file, stemmer_ngram,
                                                
                                                stemmer_gamma, stemmer_truncate, stemmer_batches, vocabulary_path, FLAG_write_file);
    
    #ifdef _OPENMP
    #pragma omp critical
    #endif
    {
      res_vec[f] = tmp_str;
    }
  }

  return res_vec;
}


//--------------------------------------------------------------------------------------------------------------------------------------
// tokenization and transformation for a vector of documents ( a list where each sublists includes a character vector of (split) words )
//--------------------------------------------------------------------------------------------------------------------------------------

// [[Rcpp::export]]
std::vector<std::vector<std::string> > res_token_list(std::vector<std::string>& VEC, std::vector<std::string>& language, std::string& language_spec, std::string& LOCALE_UTF, int max_num_char,

                                                      std::string& remove_char, bool cpp_to_lower, bool cpp_to_upper, bool cpp_remove_punctuation, bool remove_punctuation_vector, bool cpp_remove_numbers,

                                                      bool cpp_trim_token, bool cpp_tokenization_function, std::string& cpp_string_separator, bool cpp_remove_stopwords, int min_num_char,

                                                      std::string& stemmer, int min_n_gram, int max_n_gram, int skip_n_gram, int skip_distance, std::string& n_gram_delimiter, std::string& concat_delimiter,

                                                      std::string& path_2file, int stemmer_ngram, double stemmer_gamma, int stemmer_truncate, int stemmer_batches, int threads, bool verbose,

                                                      std::string& vocabulary_path) {

  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  big_files bgf;

  std::vector<std::vector<std::string> > res_vec(VEC.size());        // returns a list of character vectors

  bool FLAG_write_file = path_2file == "" ? false : true;

  unsigned long long f;

  #ifdef _OPENMP
  #pragma omp parallel for schedule(static) shared(VEC, bgf, language, language_spec, LOCALE_UTF, max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation, remove_punctuation_vector, cpp_remove_numbers, res_vec, vocabulary_path, FLAG_write_file, stemmer_batches, stemmer_truncate, stemmer_gamma, stemmer_ngram, path_2file, concat_delimiter, n_gram_delimiter, skip_distance, skip_n_gram, max_n_gram, min_n_gram, stemmer, min_num_char, cpp_remove_stopwords, cpp_string_separator, cpp_tokenization_function, cpp_trim_token) private(f)
  #endif
  for (f = 0; f < VEC.size(); f++) {
    
    std::vector<std::string> tmp_vec = bgf.inner_res_tok_list(f, VEC, language, language_spec, LOCALE_UTF, max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation,
                          
                                                              remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords,
                                                              
                                                              min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, concat_delimiter, path_2file, stemmer_ngram,
                                                              
                                                              stemmer_gamma, stemmer_truncate, stemmer_batches, vocabulary_path, FLAG_write_file);
    
    #ifdef _OPENMP
    #pragma omp critical
    #endif
    {
      res_vec[f] = tmp_vec;
    }
  }

  return res_vec;
}


// //---------------------------------------------------------------------
// // function for the tokenization header  (input is a character vector):
// //---------------------------------------------------------------------
// // this one is incorrect (as it can be used with a vector of character strings) see how the 'TOKEN_batch' works inside the 'batch_2file' function in the BATCH_TOKEN class
//
// // [[Rcpp::export]]
// std::vector<std::string> res_token_vector(std::vector<std::string> &VEC, std::vector<std::string> language, std::string language_spec, std::string LOCALE_UTF,
//
//                                           int max_num_char, std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false, bool cpp_remove_punctuation = false,
//
//                                           bool remove_punctuation_vector = false, bool cpp_remove_numbers = false, bool cpp_trim_token = false, bool cpp_tokenization_function = false,
//
//                                           std::string cpp_string_separator = "-*", bool cpp_remove_stopwords = false, int min_num_char = 1, std::string stemmer = "NULL", int min_n_gram = 1,
//
//                                           int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0, std::string n_gram_delimiter = " ", std::string concat_delimiter = "NULL",
//
//                                           std::string path_2file = "", int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1, int threads = 1,
//
//                                           bool verbose = false, bool save_2single_file = false, std::string path_extend = "output_token.txt", std::string vocabulary_path = "") {
//
//   BATCH_TOKEN btkn;
//
//   return btkn.TOKEN_batch(VEC, language, language_spec, LOCALE_UTF, max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation, remove_punctuation_vector,
//
//                           cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords, min_num_char, stemmer, min_n_gram, max_n_gram,
//
//                           skip_n_gram, skip_distance, n_gram_delimiter, stemmer_ngram, stemmer_gamma, stemmer_truncate, stemmer_batches, threads, verbose, vocabulary_path,
//
//                           concat_delimiter, save_2single_file, path_2file, path_extend);
//}


//---------------------------
// big_files class functions
//---------------------------


// split the big .xml file into smaller files so that they can be pre-processed in memory [ each resulted sub-file should be approx. ~ 800MB to 1GB ]
//


// [[Rcpp::export]]
void big_splitter_bytes(std::string input_path, int batches, std::string end_query, std::string OUTPUT_PATH, bool trimmed_line = false, bool verbose = false) {

  big_files bgf;

  bgf.bytes_splitter(input_path, batches, OUTPUT_PATH, end_query, trimmed_line, verbose);
}


// parse the sub-files by keeping only the relevant text using a start_query and an end_query
//


// [[Rcpp::export]]
void big_parser(std::string input_path_folder, std::string start_query, std::string end_query, std::string output_path_folder,

                int min_lines = 1, bool trimmed_line = false, bool verbose = false) {

  big_files bgf;

  bgf.wrapper_batches_parser(input_path_folder, start_query, end_query, output_path_folder, min_lines, trimmed_line, verbose);
}



// file parser, which uses a 'start_query' and an 'end_query' to extract information from a file
//

// [[Rcpp::export]]
void file_parser(std::string input_path_file, std::string start_query, std::string end_query, std::string output_path_file = "",

                 int min_lines = 1, bool trimmed_line = false, bool verbose = false) {

  big_files bgf;

  bgf.batch_parser(input_path_file, start_query, end_query, output_path_file, min_lines, trimmed_line, verbose);
}



// calculate the KB, MB, GB of a file
//

// [[Rcpp::export]]
double convert_bytes(std::string input_path_file, std::string unit = "GB") {

  big_files bgf;

  return bgf.bytes_converter(input_path_file, unit);
}




// tokenize-transform big data sets
//

// [[Rcpp::export]]
void big_tokenize(std::string input_path_folder, std::string output_path_folder, int batches, std::vector<std::string> language, std::string language_spec, std::string LOCALE_UTF,

                  char read_file_delimiter, int max_num_char, int increment_batch_no = 1, std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false,

                  bool cpp_remove_punctuation = false, bool remove_punctuation_vector = false, bool cpp_remove_numbers = false, bool cpp_trim_token = false,

                  bool cpp_tokenization_function = false, std::string cpp_string_separator = "-*", bool cpp_remove_stopwords = false, int min_num_char = 1,

                  std::string stemmer = "NULL", int min_n_gram = 1, int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0, std::string n_gram_delimiter = " ",

                  std::string concat_delimiter = "NULL", int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1, int threads = 1,

                  bool save_2single_file = false, std::string vocabulary_folder = "", bool verbose = false) {

  big_files bgf;

  bgf.wrapper_batch_tokenizer_bytes(input_path_folder, output_path_folder, batches, increment_batch_no, language, language_spec, LOCALE_UTF, read_file_delimiter, max_num_char, remove_char,

                                    cpp_to_lower, cpp_to_upper, cpp_remove_punctuation, remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator,

                                    cpp_remove_stopwords, min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, concat_delimiter, stemmer_ngram,

                                    stemmer_gamma, stemmer_truncate, stemmer_batches, threads, save_2single_file, vocabulary_folder, verbose);
}



// calculate the vocabulary counts from a 'clean' file (after tokenization-transformation is applied)
//

// [[Rcpp::export]]
void vocabulary_counts_big_tokenize(std::string input_path_folder, std::string output_path_file, int max_num_chars = 1000, bool verbose = false) {

  big_files bgf;

  bgf.vocabulary_counts_folder(input_path_folder, output_path_file, max_num_chars, verbose);
}



// calculate the vocabulary counts from a file
//

// [[Rcpp::export]]
void vocabulary_counts(std::string input_path_file, std::string start_query, std::string end_query, std::vector<std::string> language, std::string output_path_file = "",

                       int min_lines = 1, bool trimmed_line = false, bool query_transform = false, std::string language_spec = "english", std::string LOCALE_UTF = "",

                       long long max_num_char = 1000000000, std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false,

                       bool cpp_remove_punctuation = false, bool remove_punctuation_vector = false, bool cpp_remove_numbers = false, bool cpp_trim_token = false,

                       bool cpp_tokenization_function = false, std::string cpp_string_separator = " \r\n\t.,;:()?!//", bool cpp_remove_stopwords = false,

                       int min_num_char = 1, std::string stemmer = "NULL", int min_n_gram = 1, int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0,

                       std::string n_gram_delimiter = " ", int threads = 1, bool verbose = false) {

  big_files bgf;

  return bgf.vocabulary_count_parser(input_path_file, start_query, end_query, language, output_path_file, min_lines, trimmed_line, query_transform, language_spec, LOCALE_UTF,

                                     max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation, remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token,

                                     cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords, min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance,

                                     n_gram_delimiter, threads, verbose);
}



//--------------------------------------------
// function for the batch-tokenization header:
//--------------------------------------------

// [[Rcpp::export]]
void batch_2file(std::string INPUT_FILE, std::string OUTPUT_PATH, int batches, char read_file_delimiter, std::vector<std::string> language, std::string language_spec, std::string LOCALE_UTF,

                 int max_num_char, std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false, bool cpp_remove_punctuation = false, bool remove_punctuation_vector = false,

                 bool cpp_remove_numbers = false, bool cpp_trim_token = false, bool cpp_tokenization_function = false, std::string cpp_string_separator = "-*", bool cpp_remove_stopwords = false,

                 int min_num_char = 1, std::string stemmer = "NULL", int min_n_gram = 1, int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0, std::string n_gram_delimiter = " ",

                 int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1, int threads = 1, std::string concat_delimiter = "\n",

                 std::string vocabulary_path = "", bool verbose = false) {

  BATCH_TOKEN btkn;

  return btkn.batch_2file(INPUT_FILE, OUTPUT_PATH, batches, read_file_delimiter, language, language_spec, LOCALE_UTF, max_num_char, remove_char, cpp_to_lower, cpp_to_upper,

                          cpp_remove_punctuation, remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords,

                          min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, stemmer_ngram, stemmer_gamma, stemmer_truncate,

                          stemmer_batches, threads, concat_delimiter, verbose, vocabulary_path);
}



//----------------------------------------------------------------
// returns either a document-term-matrix OR a term-document-matrix
//----------------------------------------------------------------


// [[Rcpp::export]]
Rcpp::List res_term_matrix(std::vector<std::string> vector_corpus, std::vector<std::string> language, std::string language_spec, std::string LOCALE_UTF, long long max_num_char, bool document_term_matrix = true,

                           std::string path_2documents_file = "NULL", bool sort_columns = false, std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false,

                           bool cpp_remove_punctuation = false, bool remove_punctuation_vector = false, bool cpp_remove_numbers = false, bool cpp_trim_token = false, bool cpp_tokenization_function = false,

                           std::string cpp_string_separator = "-*", bool cpp_remove_stopwords = false, int min_num_char = 1, std::string stemmer = "NULL", int min_n_gram = 1, int max_n_gram = 1,

                           int skip_n_gram = 1, int skip_distance = 0, std::string n_gram_delimiter = " ", int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1,

                           int threads = 1, bool verbose = false, long long print_every_rows = 1000, std::string normalize_tf = "NULL",bool tf_idf = false) {

  term_matrix trmx;

  if (document_term_matrix) {

    trmx.document_term_matrix(vector_corpus, language, language_spec, LOCALE_UTF, max_num_char, path_2documents_file, sort_columns, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation, remove_punctuation_vector,

                              cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords, min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance,

                              n_gram_delimiter, stemmer_ngram, stemmer_gamma, stemmer_truncate, stemmer_batches, threads, verbose, print_every_rows, normalize_tf, tf_idf);}

  if (!document_term_matrix) {

    trmx.term_document_matrix(vector_corpus, language, language_spec, LOCALE_UTF, max_num_char, path_2documents_file, sort_columns, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation, remove_punctuation_vector,

                              cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords, min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance,

                              n_gram_delimiter, stemmer_ngram, stemmer_gamma, stemmer_truncate, stemmer_batches, threads, verbose, print_every_rows, normalize_tf, tf_idf);
  }

  return(Rcpp::List::create( Rcpp::Named("terms") = trmx.output_data().terms_out, Rcpp::Named("term_matrix") = trmx.output_data().sp_mat_out,

                             Rcpp::Named("rows") = trmx.output_data().row_idx_, Rcpp::Named("cols") = trmx.output_data().col_idx_,

                             Rcpp::Named("counts") = trmx.output_data().docs_cnt_));
}



//---------------------------------------------------------------------------------------------------------
// global term weights [ using the document-term-matrix with 'tf_idf = false' AND 'normalize_tf = "NULL"' ]
//---------------------------------------------------------------------------------------------------------

// [[Rcpp::export]]
Rcpp::List idf_global_term_weights(arma::sp_mat Tmat, std::vector<std::string> Terms) {
  
  if (Tmat.empty()) {
    
    Rcpp::stop("first run the document-term-matrix method");
  }
  
  term_matrix trmx;
  
  arma::rowvec sum_counts = trmx.Sparse_Sums(Tmat, false);
  
  long long num_docs = Tmat.n_rows;
  
  std::vector<double> idf_gtw = arma::conv_to< std::vector<double> >::from(arma::log(num_docs / (sum_counts + 1.0)));        // +1 to avoid division by zero
  
  return(Rcpp::List::create( Rcpp::Named("terms") = Terms, Rcpp::Named("Idf_global_term_weights") = idf_gtw));
}



//-----------------------------------------------------------------------
// adjust the sparsity in a document-term-matrix OR term-document-matrix
//-----------------------------------------------------------------------

// [[Rcpp::export]]
Rcpp::List Adj_Sparsity(arma::rowvec column_indices, arma::rowvec row_indices, arma::vec docs_counts, std::vector<std::string> Terms, double sparsity_thresh = 1.0) {

  term_matrix trmx;

  return trmx.adj_Sparsity(column_indices, row_indices, docs_counts, Terms, sparsity_thresh);
}




// associations for either a single variable or for a vector of variables
//

// [[Rcpp::export]]
Rcpp::List Associations_Cpp(arma::rowvec column_indices_, arma::rowvec row_indices_, arma::vec docs_counts_, long long target_size, std::vector<std::string> Terms, std::vector<int> mult_target_var,

                            long long keepTerms = 0, long long target_var = -1, std::string normalize_TF = "NULL", bool tf_IDF = false, bool verbose = false) {


  arma::wall_clock timer;

  if (verbose) { timer.tic(); Rprintf("\n"); }

  if (normalize_TF != "NULL" || tf_IDF) {

    associations_class<double> aclass(column_indices_, row_indices_, docs_counts_);

    aclass.associations_mapping();

    Rcpp::List tmp_rcpp;

    if (target_var != -1) {

      tmp_rcpp = aclass.correlation_assoc_single(target_var, target_size, Terms, keepTerms);
    }

    else {

      tmp_rcpp = aclass.correlation_assoc_multiple(mult_target_var, target_size, Terms, keepTerms, verbose);
    }

    if (verbose) { double n = timer.toc(); Rprintf("\tminutes.to.complete: %.5f", n / 60.0); }

    return tmp_rcpp;
  }

  else {

    associations_class<long long> aclass(column_indices_, row_indices_, docs_counts_);

    aclass.associations_mapping();

    Rcpp::List tmp_rcpp;

    if (target_var != -1) {

      tmp_rcpp = aclass.correlation_assoc_single(target_var, target_size, Terms, keepTerms);
    }

    else {

      tmp_rcpp = aclass.correlation_assoc_multiple(mult_target_var, target_size, Terms, keepTerms, verbose);
    }

    if (verbose) { double n = timer.toc(); Rprintf("\tminutes.to.complete: %.5f", n / 60.0); }

    return tmp_rcpp;
  }
}


//--------------------
// most frequent terms
//--------------------


// [[Rcpp::export]]
Rcpp::List Most_Freq_Terms(arma::sp_mat sparse_data, std::vector<std::string> Terms, long long keepTerms = 0, bool flag_dtm = false, int threads = 1, bool verbose = false) {

  arma::wall_clock timer;

  if (verbose) { timer.tic(); Rprintf("\n"); }

  term_matrix trmx;

  Rcpp::List tmp_lst = trmx.most_freq_terms(sparse_data, Terms, keepTerms, flag_dtm, threads);

  if (verbose) { double n = timer.toc(); Rprintf("minutes.to.complete: %.5f", n / 60.0); }

  return tmp_lst;
}



//----------------------------------------------------------
// returns the sparsity of a sparse matrix as a float number
//----------------------------------------------------------


// [[Rcpp::export]]
void sparsity_float(arma::sp_mat data) {

  term_matrix trmx;

  Rcpp::Rcout << std::trunc(10000 * trmx.sparsity(data)) / 10000 << " %" << std::endl;
}


//----------------------------------
// convert a dense matrix to sparse
//----------------------------------


// [[Rcpp::export]]
arma::sp_mat dense_2sparse_mat(arma::mat x) {

  arma::sp_mat x1(x);

  return x1;
}



//------------------------------------
// rowSums, colSums of a sparse matrix
//------------------------------------


// [[Rcpp::export]]
arma::rowvec sp_sums(arma::sp_mat sp_data, bool rowSums = false) {

  term_matrix trmx;

  return trmx.Sparse_Sums(sp_data, rowSums);
}


//----------------------------------------------------------------------------
// exclude zero-valued rows or columns (belongs to the 'Adj_Sparsity function)
//----------------------------------------------------------------------------

// [[Rcpp::export]]
arma::uvec tf_idf_exclude(arma::sp_mat tmp_mat, bool document_term_matrix = true) {

  arma::rowvec tmp_indices = sp_sums(tmp_mat, !document_term_matrix);

  arma::uvec keep_idx = arma::find(tmp_indices != 0.0);

  return keep_idx;
}


//------------------------------------
// rowMeans, colMeans of a sparse matrix
//------------------------------------


// [[Rcpp::export]]
arma::rowvec sp_means(arma::sp_mat sp_data, bool rowMeans = false) {

  arma::sp_mat norm_col_sums;

  arma::mat tmp_mat;

  if (rowMeans) {

    norm_col_sums = arma::mean(sp_data, 1);

    tmp_mat = arma::conv_to< arma::mat >::from(norm_col_sums.col(0));}

  else {

    norm_col_sums = arma::mean(sp_data, 0);

    tmp_mat = arma::conv_to< arma::mat >::from(norm_col_sums.row(0));
  }

  arma::rowvec tmp_vec = arma::conv_to< arma::rowvec >::from(tmp_mat);

  return tmp_vec;
}



//---------------------------------------
// save a sparse matrix in binary format
//---------------------------------------


// [[Rcpp::export]]
void save_sparse_(arma::sp_mat x, std::string file_name = "save_sparse.mat") {

  x.save(file_name, arma::arma_binary);
}


//---------------------------------------
// load a sparse matrix in binary format
//---------------------------------------

// [[Rcpp::export]]
arma::sp_mat load_sparse_(std::string file_name = "load_sparse.mat") {

  arma::sp_mat load_dat;

  load_dat.load(file_name, arma::arma_binary);

  return load_dat;
}


//------------------------------------------------------
// read a specific number of characters from a text file
//------------------------------------------------------

// [[Rcpp::export]]
std::string read_CHARS(std::string input_file, long long characters = 200, std::string write_2file = "") {

  long long nr_char = 1;

  char chs;

  std::string STR;

  std::fstream myfile(input_file, std::fstream::in);

  while (myfile >> std::noskipws >> chs) {

    STR += chs;

    if (nr_char >= characters) {

      break;
    }

    nr_char++;
  }

  if (write_2file != "") {

    std::ofstream out(write_2file);

    out << STR;

    STR.clear();

    STR.shrink_to_fit();

    out.close();
  }

  return STR;
}


//------------------------------------------------
// read a specific number of rows from a text file
//------------------------------------------------


// [[Rcpp::export]]
std::vector<std::string> read_ROWS(std::string input_file, std::string write_2file = "", char read_delimiter = ' ', long long rows = 200) {

  long long nr_rows = 0;

  std::string line;

  std::vector<std::string> VEC;

  std::ifstream myfile(input_file);

  while (std::getline(myfile, line, read_delimiter)) {

    if (nr_rows == rows) {

      break;
    }

    VEC.push_back(line + '\n');                 // I added new-line so that each row can be pre-processed separately

    nr_rows++;
  }

  if (write_2file != "") {

    std::ofstream out(write_2file);

    for (unsigned long long i = 0; i < VEC.size(); i++) {

      out << VEC[i];
    }

    VEC.clear();

    VEC.shrink_to_fit();

    out.close();
  }

  myfile.close();

  return VEC;
}



// function to remove duplicates
//

// [[Rcpp::export]]
Rcpp::LogicalVector Not_Duplicated(Rcpp::CharacterVector x) {
  
  Rcpp::LogicalVector out = Rcpp::duplicated(x);
  
  return !out;
}

