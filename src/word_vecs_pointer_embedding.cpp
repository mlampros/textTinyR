# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(BH)]]


#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <memory>
#include <sstream>
#include <stdlib.h>
#include <algorithm>
#include <iterator>
#include <unordered_map>
#include <map>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/range/adaptor/map.hpp>
#include <boost/range/algorithm/copy.hpp>


#ifdef _OPENMP
#include <omp.h>
#endif



//--------------------------------------------------------------------------------------------
// Here the idea is to run only once the pre-processing of the word-vectors and then to
// be able to experiment with all 3 word-vector methods without re-doing the pre-processing.
// Thus create an R6 class which first calls the "reduced_word_vectors" method
// to build the reduced word-vectors and then use a second method ( "word_vectors_methods" )
// inside the R6 class to experiment with one of the 3 methods.
// Exchange of object between R and C++ using either an external pointer or by copying data
//--------------------------------------------------------------------------------------------



// alias for the unordered map
//

using Pointer_Map = std::unordered_map<std::string, std::vector<double> >;


// struct for sorting [ 'PREPROCESS_WORD_VECS' class ]
//

struct STRUCT {

  std::string VAR1;

  int VAR2;
};


// struct for sorting [ 'embedding_word_sequences' function ]
//

struct struct_SORT {

  int index;

  std::string single_word;

  int frequency;
};


// --------------------------------------------------------------------------- class to sort words in case of clustering
//

class SORT_CLUST_DOCS {

  public:

    SORT_CLUST_DOCS() { }


    // method used for clustering documents
    //

    static bool STRUCT_SORT(const STRUCT &x, const STRUCT &y) {

      return x.VAR2 > y.VAR2;
    }


    // method used for embedding sequences [ I can do this (and the previous one) in a more efficient way by using a template, see 'textTinyR' the 'sort_2dim_vecs.h' file ]
    //

    static bool sort_frequency(const struct_SORT &a, const struct_SORT &b) {

      return a.frequency > b.frequency;
    }


    // method used for clustering documents
    //

    std::vector<STRUCT> SORT_MAP(std::unordered_map<std::string, int> DAT) {

      std::vector<STRUCT> struct_result;

      for (auto& iter : DAT) {

        STRUCT tmp_struct;

        tmp_struct.VAR1 = iter.first;

        tmp_struct.VAR2 = iter.second;

        struct_result.push_back(tmp_struct);
      }

      std::sort(struct_result.begin(), struct_result.end(), STRUCT_SORT);

      return struct_result;
    }

    ~SORT_CLUST_DOCS() { }
};



// --------------------------------------------------------------------------- class for word-vectors
//

class PREPROCESS_WORD_VECS {


  private:

    // constructor parameters
    //------------------------

    std::string FILE;

    std::vector<std::string> unique_tokens;

    int vector_dimensions;

    int print_every_rows;

    bool verbose;


    // variables for methods
    //-----------------------

    std::unordered_map<std::string, long long > indices;

    std::vector<std::string> str_vec;

    std::vector<std::string> words_intersection;


    // these variables I'll use in 'WORD_VECTORS_sqrt', 'WORD_VECTORS_norm' and 'TFIDF_W2V' methods [ use by reference ]
    //-------------------------------------------------------------------------------------------------------------------

    //std::shared_ptr<Pointer_Map> RES{ std::make_shared<Pointer_Map>() };               // pointer using std::shared_ptr AND std::make_shared

    Pointer_Map* RES = new Pointer_Map();                                                // initial pointer for the unordered map

    Pointer_Map* RES_deref;                                                              // used for the back-conversion of the Rcpp::Xptr pointer to a cpp-pointer

    std::vector<std::string> character_strings;

    arma::mat word_vec_mat;

    Pointer_Map RES_copy;                                                                // option to return the unordered-map rather than the pointer, copy_data = TRUE [ easier to debug in case of error / segfault ]

    std::vector<std::vector<double> > Vec_RES_copy;

  public:

    // constructor

    PREPROCESS_WORD_VECS(std::string INPUT_FILE, std::vector<std::string> input_unique_tokens, int input_vector_dimensions, int input_print_every_rows,
                         bool input_verbose) : FILE(INPUT_FILE), unique_tokens(input_unique_tokens), vector_dimensions(input_vector_dimensions), print_every_rows(input_print_every_rows), verbose(input_verbose) {}


    // boost-split-string
    //

    std::vector<std::string> bst_split(std::string INPUT) {

      std::vector<std::string> tmp_vec;

      boost::trim(INPUT);                                     // first trim the input string as it's possible that after splitting white-spaces might be taken as strings, causing miss-matching between split-strings and 'vector_dimensions'

      boost::split( tmp_vec, INPUT, boost::is_any_of(" "), boost::token_compress_on );

      return tmp_vec;
    }


    // returns only the character strings from the word-vectors
    //

    void character_strings_word_vector() {

      long long nr_rows = 0;

      long long tmp_print_rows = print_every_rows;

      std::string line;

      std::ifstream myfile(FILE);

      if (!myfile) {

        Rcpp::stop("error occured while opening the File!\n");
      }

      else {

        if (verbose) {

          Rcpp::Rcout << "File is successfully opened" << std::endl;
        }
      }

      while (std::getline(myfile, line)) {

        std::vector<std::string> tok = bst_split(line);

        int tk_size = tok.size();

        if (tk_size == (vector_dimensions + 1)) {                                   // keep only rows with the correct dimensions  ( the + 1 takes into account the first character string item )

          str_vec.push_back(tok[0]);                                                // this is the first word (item) of the word-vectors which corresponds to the term and all other following terms (items) are numbers

          if (verbose) {

            if (nr_rows + 1 == tmp_print_rows) {

              Rprintf("\rtotal.number.lines.processed.input: %3d", nr_rows + 1);

              tmp_print_rows += print_every_rows;
            }
          }
        }

        nr_rows++;
      }

      myfile.close();
    }


    // keep track of the indices of the unique tokens
    //

    void create_index_unique_toks() {

      int count = 0;

      for (unsigned int f = 0; f < unique_tokens.size(); f++) {

        indices[unique_tokens[f]] = count;

        count++;
      }
    }


    // intersection of token-unique-words and word-vector-character strings
    //

    std::vector<std::string> INTERSECT(std::vector<std::string>& tmp_unique_tokens, std::vector<std::string>& tmp_str_vec) {

      std::vector<std::string> tmp_words_intersection;

      std::sort(tmp_unique_tokens.begin(), tmp_unique_tokens.end());

      std::sort(tmp_str_vec.begin(), tmp_str_vec.end());

      std::set_intersection(tmp_unique_tokens.begin(), tmp_unique_tokens.end(), tmp_str_vec.begin(), tmp_str_vec.end(), std::back_inserter(tmp_words_intersection));

      return tmp_words_intersection;
    }


    // set -1 to all words that are both in the word-vectors and in the unique words (intersection)
    //

    void modify_index_unique_toks() {

      for (unsigned int k = 0; k < words_intersection.size(); k++) {

        indices[words_intersection[k]] = -1;
      }
    }


    // unordered map of the subset of unique tokens
    //

    void subset_wordvecs(bool copy_data = false) {

      long long sec_nr_rows = 0;

      long long sec_tmp_print_rows = print_every_rows;

      std::string line;

      std::ifstream sec_myfile(FILE);

      if (!sec_myfile) {

        Rcpp::stop("error occured while opening the File!\n");
      }

      else {

        if (verbose) {

          Rcpp::Rcout << "File is successfully opened" << std::endl;
        }
      }

      while (std::getline(sec_myfile, line)) {

        std::vector<std::string> tok = bst_split(line);

        if (verbose) {

          if (sec_nr_rows + 1 == sec_tmp_print_rows) {

            Rprintf("\rtotal.number.lines.processed.output: %3d", sec_nr_rows + 1);

            sec_tmp_print_rows += print_every_rows;
          }
        }

        if (indices[tok[0]] == -1) {

          std::vector<double> conv_to_double(vector_dimensions);

          for (int k = 1; k < vector_dimensions + 1; k++) {

            conv_to_double[k-1] = atof(tok[k].c_str());                               // append vector by using an index (k-1) convert to numeric values from std::string using atof()
          }

          if (copy_data) {

            RES_copy[tok[0]] = conv_to_double;
          }

          else {

            (*RES)[tok[0]] = conv_to_double;
          }
        }

        sec_nr_rows++;
      }

      sec_myfile.close();
    }


    // function which creates a public variable with the pre-processed word-vectors [ reduced size ]
    //

    void pre_proc_wordvec(bool copy_data = false) {

      if (verbose) { Rcpp::Rcout << "" << std::endl; }

      if (verbose) { Rcpp::Rcout << "pre-processing of input data starts ..." << std::endl; }

      character_strings_word_vector();

      if (verbose) { Rprintf("\n"); }

      if (verbose) { Rcpp::Rcout << "creation of index starts ..." << std::endl; }

      create_index_unique_toks();

      if (verbose) { Rcpp::Rcout << "intersection of tokens and wordvec character strings starts ..." << std::endl; }

      words_intersection = INTERSECT(unique_tokens, str_vec);

      if (verbose) { Rcpp::Rcout << "modification of indices starts ..." << std::endl; }

      modify_index_unique_toks();

      if (verbose) { Rcpp::Rcout << "final processing of data starts ..." << std::endl; }

      subset_wordvecs(copy_data);
    }


    // function which returns two  variables :
    //
    // 1st. an std::vector<std::string> with the character strings of the preprocessed (reduced) word-vectors
    // 2nd. a pointer to the reduced word-vectors OR an unordered_map if copy_data = true
    //

    Rcpp::List vec_mat(bool copy_data = false) {

      Rcpp::List temp_lst = Rcpp::List::create( Rcpp::Named("terms_reduced_wordvecs"), Rcpp::Named("reduced_wordvecs_pointer"));

      if (copy_data) {

        for (auto& it: RES_copy) {

          character_strings.push_back(it.first);

          Vec_RES_copy.push_back(it.second);
        }

        temp_lst["reduced_wordvecs_pointer"] = Vec_RES_copy;                                    // return unordered-map if "copy_data = true"
      }

      else {

        for (auto& it: (*RES)) {                                                                // de-reference of the unordered map

          character_strings.push_back(it.first);
        }

        Rcpp::XPtr< Pointer_Map > pntr(RES, true);

        temp_lst["reduced_wordvecs_pointer"] = pntr;                                             // return pointer if "copy_data = false"
      }

      temp_lst["terms_reduced_wordvecs"] = character_strings;                                    // in any case (copy_data = true or false) append character strings to the Rcpp::List

      return temp_lst;
    }


    // Rcpp::Xptr TO cpp-pointer AND back-conversion  [ Here I use the Rcpp::Xptr to move a cpp-object from R to the C++ and back, avoiding in that way to copy data ]
    //
    // references:
    // https://stat.ethz.ch/pipermail/r-devel/2012-December/065487.html
    // https://www.r-bloggers.com/external-pointers-with-rcpp/
    // http://lists.r-forge.r-project.org/pipermail/rcpp-devel/2015-June/008821.html ]
    //
    // [ when copy_data = FALSE ]

    void de_reference(Rcpp::List rcpp_list) {

      character_strings = Rcpp::as<std::vector<std::string> >(rcpp_list[0]);        // extract the character-vector-strings from the Rcpp::List [ first item ]

      SEXP pntr = Rcpp::as< SEXP > (rcpp_list[1]);                                  // extract the pointer from the Rcpp::List [ second item ]

      Rcpp::XPtr<Pointer_Map> RES(pntr);                                            // Rcpp::Xptr

      RES_deref = RES ;                                                             // see first web-link on how this works
    }


    // overwrite key-values of unordered map [ when copy_data = TRUE ]
    //

    void overwrite_unord_map(std::vector<std::string>& str_x, std::unordered_map<std::string, std::vector<double> >& map_y) {

      character_strings = str_x;

      RES_copy = map_y;
    }



    // inner function for the "WORD_VECTORS_sqrt" function [ due to ASAN errors ]
    //

    arma::rowvec inner_wv_sqrt(std::vector<std::vector<std::string> >& INPUT_list, unsigned int j, bool copy_data) {

      std::vector<std::string> string_vec = INPUT_list[j];                                                            // 'string_vec' is a vector of tokens

      arma::rowvec num_vec(vector_dimensions, arma::fill::zeros);

      if (!string_vec.empty()) {

        for (unsigned int f = 0; f < string_vec.size(); f++) {                                                        // iterate over the 'string_vec'

          std::string tmp_str = string_vec[f];

          arma::rowvec tmp_vec;

          if (copy_data) {

            tmp_vec = arma::conv_to< arma::rowvec >::from(RES_copy[tmp_str]);
          }

          else {

            tmp_vec = arma::conv_to< arma::rowvec >::from((*RES_deref)[tmp_str]);                                     // de-referencing of the unordered-map
          }

          if (tmp_vec.n_elem > 0) {                                                                                   // exception for the case where 'tmp_str' does not exist in word vectors

            num_vec += tmp_vec;                                                                                       // add the vectors of the character words in the vector ('string_vec')
          }
        }
      }

      if (arma::accu(num_vec) != 0.0) {

        double tmp_denom = arma::accu(arma::pow(num_vec, 2.0));

        double denom = std::sqrt((double) tmp_denom);                                                                 // the warning disappears if result is a 'float'

        //float denom = std::sqrt( (float) tmp_denom);

        num_vec /= denom;
      }

      return num_vec;
    }



    // word-vectors calculation using power2 and sqrt, SEE,  https://www.linkedin.com/pulse/duplicate-quora-question-abhishek-thakur [ without normalization of word-vectors ]
    //

    arma::mat WORD_VECTORS_sqrt(std::vector<std::vector<std::string> >& INPUT_list, int threads, bool copy_data = false) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      arma::mat res_out(INPUT_list.size(), vector_dimensions, arma::fill::zeros);

      unsigned int j;

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static) shared(INPUT_list, res_out, copy_data) private(j)                       // public variables 'vector_dimensions', 'RES_deref' by default shared
      #endif
      for (j = 0; j < INPUT_list.size(); j++) {                                                                         // iterate over the list of lists of word-tokens

        arma::rowvec num_vec = inner_wv_sqrt(INPUT_list, j, copy_data);

        for (unsigned int k = 0; k < num_vec.n_elem; k++) {

          #ifdef _OPENMP
          #pragma omp atomic write
          #endif
          res_out(j, k) = num_vec(k);
        }
      }

      return res_out;
    }


    // inner function for the "WORD_VECTORS_norm" function [ due to ASAN errors ]
    //

    arma::rowvec inner_wv_norm(std::vector<std::vector<std::string> >& INPUT_list, unsigned int j, bool copy_data) {

      std::vector<std::string> string_vec = INPUT_list[j];                                                            // 'string_vec' is a vector of tokens

      arma::rowvec num_vec(vector_dimensions, arma::fill::zeros);

      if (!string_vec.empty()) {

        for (unsigned int f = 0; f < string_vec.size(); f++) {                                                        // iterate over the 'string_vec'

          std::string tmp_str = string_vec[f];                                                                        // take the vector of each word from the 'RES_copy' or 'RES_deref' unordered-map

          arma::rowvec INNER_VEC;

          if (copy_data) {

            INNER_VEC = arma::conv_to< arma::rowvec >::from(RES_copy[tmp_str]);
          }

          else {

            INNER_VEC = arma::conv_to< arma::rowvec >::from((*RES_deref)[tmp_str]);
          }

          if (INNER_VEC.n_elem > 0) {                                                                                 // exception for the case where 'tmp_str' does not exist in word vectors

            INNER_VEC -= arma::as_scalar(arma::min(INNER_VEC));

            double tmp_min_max = arma::as_scalar(arma::max(INNER_VEC)) - arma::as_scalar(arma::min(INNER_VEC));       // min-max normalizing for positive and negative values

            INNER_VEC /= tmp_min_max;

            double SUM = arma::as_scalar(arma::accu(INNER_VEC));                                                      // normalizing so that values lie between 0 and 1

            INNER_VEC /= SUM;

            num_vec += INNER_VEC;                                                                                     // add the vectors of the character words in the vector ('string_vec')
          }
        }
      }

      return num_vec;
    }


    // word-vectors calculation using min-max-normalization
    //

    arma::mat WORD_VECTORS_norm(std::vector<std::vector<std::string> >& INPUT_list, int threads, bool copy_data = false) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      arma::mat res_out(INPUT_list.size(), vector_dimensions, arma::fill::zeros);

      unsigned int j;

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static) shared(INPUT_list, res_out, copy_data) private(j)                       // public variables 'vector_dimensions', 'RES_copy' or 'RES_deref' by default shared
      #endif
      for (j = 0; j < INPUT_list.size(); j++) {                                                                         // iterate over the list of lists of word-tokens

        arma::rowvec num_vec = inner_wv_norm(INPUT_list, j, copy_data);

        for (unsigned int k = 0; k < num_vec.n_elem; k++) {

          #ifdef _OPENMP
          #pragma omp atomic write
          #endif
          res_out(j, k) = num_vec(k);
        }
      }

      return res_out;
    }



    // inner function for the "TFIDF_W2V" function [ due to ASAN errors ]
    //

    arma::rowvec inner_wv_tfidf(std::vector<std::vector<std::string> >& INPUT_list, unsigned int m, bool copy_data, std::unordered_map<std::string, double>& gtw_unordmap_reduced) {

      std::vector<std::string> inner_lst = INPUT_list[m];

      unsigned int inner_SIZE = inner_lst.size();

      arma::rowvec outer_mean;

      if (inner_SIZE > 0) {                                                                                           // if the sublist to pre-process is empty then do nothing as the output-matrix is first filled with zero values

        arma::mat inner_mat(inner_SIZE, vector_dimensions, arma::fill::zeros);                                        // by default fill with 0's   [ SEE what is the most proper value in idf for a non-existing value ]

        for (unsigned int s = 0; s < inner_SIZE; s++) {

          std::vector<double> tmp_wv_inner;

          if (copy_data) {

            tmp_wv_inner = RES_copy[inner_lst[s]];
          }

          else {

            tmp_wv_inner = (*RES_deref)[inner_lst[s]];                                                                // de-reference here otherwise segfault
          }

          int tmp_wv_size = tmp_wv_inner.size();

          if ((tmp_wv_size ==  vector_dimensions) && gtw_unordmap_reduced.count(inner_lst[s])) {              // continue only if the word appears in both the 'RES_deref' and 'gtw_unordmap_reduced' objects

            arma::rowvec wv_inner = arma::conv_to< arma::rowvec >::from(tmp_wv_inner);

            double idf_inner = gtw_unordmap_reduced[inner_lst[s]];

            wv_inner *= idf_inner;

            inner_mat.row(s) = wv_inner;
          }
        }

        outer_mean = arma::mean(inner_mat, 0);                                                              // by default get the column-mean of the matrix [ can be changed ]
      }

      return outer_mean;
    }


    //----------------------------------------------------------------------------------------------------------------------------------------------------
    // word-vector calculation using the idf of the global-term-weights [ 'http://www.erogol.com/duplicate-question-detection-deep-learning/' ]
    //
    // intersection of reduced word-vector-terms with the global-weights-terms :
    //--------------------------------------------------------------------------
    //
    // The word-vector of a term will be multiplied with the corresponding idf of the global-weights-term, if the word-vector for the specific term does
    // not exist ( all-zeros-vector ) then the resulted multiplication is also an all-zeros-vector thus calculate the intersection between the
    // word-vector-terms and the idf-global-terms and NOT the intersection of the unique-document-terms with the global-weights-terms
    //
    // difference between 'local' and 'global' terms
    //----------------------------------------------
    //
    // 'local'  : frequency of a term in a document
    // 'global' : ( how many documents does a term appear in ) + ( total number of documents )
    //
    // In summary, 'local' does TF (term-frequency) and 'global' does IDF (inverse document frequency). Together they do : TF-IDF
    //----------------------------------------------------------------------------------------------------------------------------------------------------
    //

    arma::mat TFIDF_W2V(std::vector<std::vector<std::string> >& INPUT_list, std::vector<std::string>& gtw_terms, std::vector<double>& gtw_weights, int threads, bool copy_data = false) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      //-----------------------------------------------------------------------------

      // it might be possible to improve the efficiency of this code snippet

      std::unordered_map<std::string, double> gtw_unordmap, gtw_unordmap_reduced;                          // initialize two unordered-maps (one for all terms and one for the reduced ones)

      for (unsigned int t = 0; t < gtw_terms.size(); t++) {

        gtw_unordmap[gtw_terms[t]] = gtw_weights[t];
      }

      std::vector<std::string> tmp_intersect = INTERSECT(character_strings, gtw_terms);

      for (unsigned long long n = 0; n < tmp_intersect.size(); n++) {

        gtw_unordmap_reduced[tmp_intersect[n]] = gtw_unordmap[tmp_intersect[n]];
      }

      std::unordered_map<std::string, double>().swap(gtw_unordmap);                                                     // release memory as I don't need the first unordered-map [ 'gtw_unordmap_reduced' is the one to use]

      //-----------------------------------------------------------------------------

      unsigned long long lst_SIZE = INPUT_list.size();

      arma::mat outer_mat(lst_SIZE, vector_dimensions, arma::fill::zeros);                                              // by default fill with 0's   [ SEE what is the proper value of 'idf' for a non-existing term ]

      unsigned long long m;

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static) shared(INPUT_list, lst_SIZE, gtw_unordmap_reduced, outer_mat, copy_data) private(m)                // public variables 'vector_dimensions', 'RES' by default shared
      #endif
      for (m = 0; m < lst_SIZE; m++) {

        arma::rowvec outer_mean = inner_wv_tfidf(INPUT_list, m, copy_data, gtw_unordmap_reduced);

        if (!outer_mean.is_empty()) {

          for (unsigned int k = 0; k < outer_mean.n_elem; k++) {

            #ifdef _OPENMP
            #pragma omp atomic write
            #endif
            outer_mat(m, k) = outer_mean(k);
          }
        }
      }

      return outer_mat;
    }


    ~PREPROCESS_WORD_VECS() { }
};




//====================
// functions to export
//====================


// returns an Rcpp::List which includes : 1st. the terms of the reduced word-vectors
//                                        2nd. an external pointer OR an unordered_map (if copy_data = true) of the reduced word-vectors needed for the word-vector-methods
//

// [[Rcpp::export]]
Rcpp::List reduced_word_vectors(std::string FILE, std::vector<std::string> unique_tokens, int vector_dimensions,

                                int print_every_rows = 10000, bool verbose = false, bool copy_data = false) {

  PREPROCESS_WORD_VECS ppwv(FILE, unique_tokens, vector_dimensions, print_every_rows, verbose);

  ppwv.pre_proc_wordvec(copy_data);                                                   // pre-process input-word-vector-embeddings to reduce the dimensions

  return ppwv.vec_mat(copy_data);                                                     // return two different objects : an std::vector of string-terms and a numeric-matrix of the word-vectors
}



// word-vector-methods : 'sum_sqrt', 'min_max_norm' or 'idf'
//
// In case that NO global-term-weights are used then both 'gtw_terms' and
// 'gtw_weights' should take an emtpy vector as input from inside R
//

// [[Rcpp::export]]
arma::mat word_vectors_methods(Rcpp::List rcpp_list, std::vector<std::vector<std::string> >& INPUT_list, std::string FILE, std::string method,

                               std::vector<std::string> unique_tokens, int vector_dimensions, std::vector<std::string>& gtw_terms,

                               std::vector<double>& gtw_weights, int print_every_rows = 10000, bool verbose = false, int threads = 1,

                               bool copy_data = false) {

  arma::wall_clock timer;

  if (verbose) {

    timer.tic();
  }

  PREPROCESS_WORD_VECS ppwv_deref(FILE, unique_tokens, vector_dimensions, print_every_rows, verbose);

  if (copy_data) {

    std::vector<std::string> character_strings = Rcpp::as<std::vector<std::string> >(rcpp_list[0]);                             // first item of the input-list

    std::vector<std::vector<double> > tmp_lst_map = Rcpp::as< std::vector<std::vector<double> > > (rcpp_list[1]);               // second item of the input-list

    std::unordered_map<std::string, std::vector<double> > INP_map;                                                              // build the unordered-map [ when copy_data = TRUE ]

    for (unsigned int f = 0; f < tmp_lst_map.size(); f++) {

      INP_map[character_strings[f]] = tmp_lst_map[f];
    }

    ppwv_deref.overwrite_unord_map(character_strings, INP_map);                                               // overwrite the character-string-vector and the unordered-map to use in the word-vector-methods
  }

  else {

    ppwv_deref.de_reference(rcpp_list);
  }

  arma::mat outp_wv;                                                                                         // output-word-vector-matrix

  if (verbose) { Rcpp::Rcout << "\n" << std::endl; }

  if (method == "sum_sqrt") {

    if (verbose) { Rcpp::Rcout << "The word-vector 'sum_sqrt' method starts ..." << std::endl; }

    outp_wv = ppwv_deref.WORD_VECTORS_sqrt(INPUT_list, threads, copy_data);}

  else if (method == "min_max_norm") {

    if (verbose) { Rcpp::Rcout << "The word-vector 'min_max_norm' method starts ..." << std::endl; }

    outp_wv = ppwv_deref.WORD_VECTORS_norm(INPUT_list, threads, copy_data);}

  else if (method == "idf") {

    if (verbose) { Rcpp::Rcout << "The word-vector 'idf' method starts ..." << std::endl; }

    outp_wv = ppwv_deref.TFIDF_W2V(INPUT_list, gtw_terms, gtw_weights, threads, copy_data);
  }

  else {

    Rcpp::stop("valid methods are : 'sum_sqrt', 'min_max_norm' or 'idf'");
  }

  if (verbose) {

    double n = timer.toc();

    Rprintf("\n");

    Rprintf("\rminutes.to.complete: %.5f", n / 60.0);
  }

  return outp_wv;
}



// takes a list of tokenized documents and clusterings (hard cluster values) and returns the most frequent terms for each cluster (sorted)
//

// [[Rcpp::export]]
std::unordered_map<int, Rcpp::List > append_data(Rcpp::List x, std::vector<int> y) {

  SORT_CLUST_DOCS sort_docs;

  std::unordered_map<int, std::vector<std::string> > out;

  for (unsigned int i = 0; i < x.size(); i++) {

    std::vector<std::string> tmp_inner = Rcpp::as<std::vector<std::string> >(x[i]);

    for (unsigned int j = 0; j < tmp_inner.size(); j++) {

      out[y[i]].push_back(tmp_inner[j]);
    }
  }

  std::unordered_map<int, Rcpp::List > out_map;

  for (auto& iter : out) {

    std::unordered_map<std::string, int> tmp_in;

    std::vector<std::string> tmp_v = iter.second;

    for (unsigned int g = 0; g < tmp_v.size(); g++) {

      tmp_in[tmp_v[g]] += 1;
    }

    std::vector<STRUCT> tmp_sort_struct = sort_docs.SORT_MAP(tmp_in);

    std::vector<std::string> inner_str;

    std::vector<int> inner_int;

    for (auto& ITER : tmp_sort_struct) {

      inner_str.push_back(ITER.VAR1);

      inner_int.push_back(ITER.VAR2);
    }

    out_map[iter.first] = Rcpp::List::create( Rcpp::Named("words") = inner_str, Rcpp::Named("counts") = inner_int);
  }

  return out_map;
}

