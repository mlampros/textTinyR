# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(BH)]]

#include <string>
#include <vector>
#include <iostream>
#include <cmath>
#include <map>
#include <unordered_map>

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/range/algorithm_ext/erase.hpp>
#include <boost/range/adaptor/map.hpp>
#include <boost/range/algorithm/copy.hpp>

#ifdef _OPENMP
#include <omp.h>
#endif


// distance metrics
//

double METHODS_dist(std::string& method, arma::mat& MATRIX_1st, arma::mat& MATRIX_2nd, unsigned int j, double eps = 1.0e-6) {
  
  double tmp_idx = 0.0;
  
  if (method == "euclidean") {
    
    tmp_idx = std::sqrt(arma::as_scalar(arma::accu(arma::square((MATRIX_1st.row(j) - MATRIX_2nd.row(j))))));
  }
  
  else if (method == "manhattan") {
    
    tmp_idx = arma::as_scalar(arma::accu(arma::abs((MATRIX_1st.row(j) - MATRIX_2nd.row(j)))));
  }
  
  else if (method == "chebyshev") {
    
    tmp_idx = arma::as_scalar(max(arma::abs((MATRIX_1st.row(j) - MATRIX_2nd.row(j)))));
  }
  
  else if (method == "canberra") {
    
    tmp_idx = arma::as_scalar(arma::accu(arma::abs((MATRIX_1st.row(j) - MATRIX_2nd.row(j)) + eps)/(arma::abs(MATRIX_1st.row(j)) + arma::abs(MATRIX_2nd.row(j)) + eps)));                 // added 1.0e-6 otherwise rstudio crashes
  }
  
  else if (method == "braycurtis") {
    
    tmp_idx = arma::as_scalar(arma::accu(arma::abs((MATRIX_1st.row(j) - MATRIX_2nd.row(j))))/(arma::accu(arma::abs(MATRIX_1st.row(j))) + arma::accu(arma::abs(MATRIX_2nd.row(j)))));
  }
  
  else if (method == "pearson_correlation") {
    
    tmp_idx = arma::as_scalar(1.0 - arma::cor(MATRIX_1st.row(j), MATRIX_2nd.row(j)));
  }
  
  else if (method == "simple_matching_coefficient") {
    
    double a = eps;
    double d = eps;
    
    for (unsigned int t = 0; t < MATRIX_1st.row(j).n_elem; t++) {
      
      if (MATRIX_1st.row(j)(t) == 1 && MATRIX_2nd.row(j)(t) == 1) {
        
        a += 1.0;}
      
      if (MATRIX_1st.row(j)(t) == 0 && MATRIX_2nd.row(j)(t) == 0) {
        
        d += 1.0;
      }
    }
    
    tmp_idx = 1.0 - ((a + d) / MATRIX_1st.row(j).n_elem);
  }
  
  else if (method == "cosine") {                   // https://github.com/scipy/scipy/blob/v0.19.0/scipy/spatial/distance.py#L476-L506
    
    tmp_idx = arma::dot(MATRIX_1st.row(j), MATRIX_2nd.row(j)) / (arma::norm(MATRIX_1st.row(j)) * arma::norm(MATRIX_2nd.row(j)));                   // NORMALLY, here I subtract 1.0 to get the distance
  }
  
  else if (method == "hamming") {                                                                                     // for binary data
    
    tmp_idx = arma::as_scalar(accu(MATRIX_1st.row(j) != MATRIX_2nd.row(j))/(MATRIX_1st.row(j).n_elem * 1.0));
  }
  
  else if (method == "jaccard_coefficient") {                                                                                     // for binary data
    
    double a = eps;
    double b = eps;
    double c = eps;
    
    for (unsigned int t = 0; t < MATRIX_1st.row(j).n_elem; t++) {
      
      if (MATRIX_1st.row(j)(t) == 1 && MATRIX_2nd.row(j)(t) == 1) {
        
        a += 1.0;}
      
      if (MATRIX_1st.row(j)(t) == 1 && MATRIX_2nd.row(j)(t) == 0) {
        
        b += 1.0;}
      
      if (MATRIX_1st.row(j)(t) == 0 && MATRIX_2nd.row(j)(t) == 1) {
        
        c += 1.0;
      }
    }
    
    tmp_idx = 1.0 - (a / (a + b + c));
  }
  
  else if (method == "Rao_coefficient") {                                                                                     // for binary data
    
    double a = eps;
    
    for (unsigned int t = 0; t < MATRIX_1st.row(j).n_elem; t++) {
      
      if (MATRIX_1st.row(j)(t) == 1 && MATRIX_2nd.row(j)(t) == 1) {
        
        a += 1.0;
      }
    }
    
    tmp_idx = 1.0 - (a / MATRIX_1st.row(j).n_elem);
  }
  
  else {
    
    tmp_idx = 0.0;                                             // default = 0; create exceptions in R, so that tmp_idx is never 0;
  }
  
  if ( tmp_idx != tmp_idx ) {                                 // handling of NAs (in case that minimum distance is optimal), if NaN then distance 1.0 [  NaN will compare false to everything, including itself ], http://stackoverflow.com/questions/11569337/using-an-if-statement-to-switch-nan-values-in-an-array-to-0-0]
    
    tmp_idx = 1.0;
  }
  
  return tmp_idx;
}


// calculate the distance between rows of two different matrices
// [ a modification of the dissimilarity functions described in : https://www.linkedin.com/pulse/duplicate-quora-question-abhishek-thakur ]

// [[Rcpp::export]]
arma::rowvec DIST(arma::mat& MATRIX_1st, arma::mat& MATRIX_2nd, std::string& method, int threads, double eps = 1.0e-6) {

  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  arma::rowvec tmp_out = arma::zeros<arma::rowvec>(MATRIX_1st.n_rows);

  unsigned int j;

  #ifdef _OPENMP
  #pragma omp parallel for schedule(static) shared(MATRIX_1st, MATRIX_2nd, method, eps, tmp_out) private(j)
  #endif
  for (j = 0; j < MATRIX_1st.n_rows; j++) {

    double tmp_val = METHODS_dist(method, MATRIX_1st, MATRIX_2nd, j, eps);
    
    #ifdef _OPENMP
    #pragma omp atomic write
    #endif
    tmp_out(j) = tmp_val;
  }

  return tmp_out;
}



// cosine distance for character strings  [ I could use "#pragma section" to parallelize independent code snippets --- however the COS function utilizes also openmp ]
//

// [[Rcpp::export]]
double cosine_dist(std::string& x, std::string& y, std::string& separator) {
  
  std::unordered_map<std::string, int> res_x;
  
  std::vector<std::string> tmp_vec_x;
  
  boost::split( tmp_vec_x, x, boost::is_any_of(separator), boost::token_compress_on );
  
  for (unsigned int i = 0; i < tmp_vec_x.size(); i++) {
    
    res_x[tmp_vec_x[i]] += 1;
  }
  
  std::unordered_map<std::string, int> res_y;
  
  std::vector<std::string> tmp_vec_y;
  
  boost::split( tmp_vec_y, y, boost::is_any_of(separator), boost::token_compress_on );
  
  for (unsigned int i = 0; i < tmp_vec_y.size(); i++) {
    
    res_y[tmp_vec_y[i]] += 1;
  }
  
  std::vector<std::string> keys_x;
  keys_x.reserve(res_x.size());
  
  for(auto kv : res_x) {
    
    keys_x.push_back(kv.first);
  }
  
  std::vector<std::string> keys_y;
  keys_y.reserve(res_y.size());
  
  for(auto kv : res_y) {
    
    keys_y.push_back(kv.first);
  }
  
  std::vector<std::string> intersec;
  
  std::sort(keys_x.begin(), keys_x.end());
  std::sort(keys_y.begin(), keys_y.end());
  
  std::set_intersection(keys_x.begin(), keys_x.end(), keys_y.begin(), keys_y.end(), std::back_inserter(intersec));
  
  double numerator = 0.0;
  
  for (unsigned int i = 0; i < intersec.size(); i++) {
    
    numerator += res_x[intersec[i]] * res_y[intersec[i]];
  }
  
  double sum_x = 0.0;
  
  for(auto kv_x : res_x) {
    
    sum_x += std::pow(kv_x.second, 2.0);
  }
  
  double sum_y = 0.0;
  
  for(auto kv_y : res_y) {
    
    sum_y += std::pow(kv_y.second, 2.0);
  }
  
  double denominator = std::sqrt(sum_x) * std::sqrt(sum_y);
  
  double res_out = 0.0;
  
  if (denominator > 0.0) {
    
    res_out = numerator / denominator;
  }
  
  return res_out;
}



// // inner function for 'COS'
// //
// 
// // [[Rcpp::export]]
// double inner_cos(std::vector<std::string>& TEXT_SEQ1, std::vector<std::string>& TEXT_SEQ2, std::string& separator, unsigned int j) {
//   
//   std::string tmp_first = TEXT_SEQ1[j];
//   
//   std::string tmp_second = TEXT_SEQ2[j];
//   
//   double tmp = cosine_dist(tmp_first, tmp_second, separator);
//   
//   return tmp;
// }



// cosine distance for two lists (std-vectors) of same length
//

// [[Rcpp::export]]
arma::rowvec COS(std::vector<std::string>& TEXT_SEQ1, std::vector<std::string>& TEXT_SEQ2, int threads, std::string& separator) {

  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  arma::rowvec tmp_out = arma::zeros<arma::rowvec>(TEXT_SEQ1.size());

  unsigned int j;

  #ifdef _OPENMP
  #pragma omp parallel for schedule(static) shared(TEXT_SEQ1, TEXT_SEQ2, separator, tmp_out) private(j)
  #endif
  for (j = 0; j < TEXT_SEQ1.size(); j++) {

    double tmp_cos = cosine_dist(TEXT_SEQ1[j], TEXT_SEQ2[j], separator);

    #ifdef _OPENMP
    #pragma omp atomic write
    #endif
    tmp_out(j) = tmp_cos;
  }

  return tmp_out;
}


// unique values
//

// [[Rcpp::export]]
std::vector<std::string> UNIQUE(std::vector<std::string> x) {

  std::set<std::string> s(x.begin(), x.end());

  x.assign(s.begin(), s.end());

  return x;
}



// intersection of vectors
//

// [[Rcpp::export]]
std::vector<std::string> INTERSECT(std::vector<std::string> &v1, std::vector<std::string> &v2) {

  std::vector<std::string> v3;

  std::sort(v1.begin(), v1.end());
  std::sort(v2.begin(), v2.end());

  std::set_intersection(v1.begin(),v1.end(),v2.begin(),v2.end(), std::back_inserter(v3));

  return v3;
}


// union of vectors
//

// [[Rcpp::export]]
std::vector<std::string> UNION(std::vector<std::string> &v1, std::vector<std::string> &v2) {

  std::vector<std::string> v3;

  std::sort(v1.begin(), v1.end());
  std::sort(v2.begin(), v2.end());

  std::set_union(v1.begin(),v1.end(),v2.begin(),v2.end(), std::back_inserter(v3));

  return v3;
}



// the JACCARD function is a modification of the corresponding python function of the first place solution of the "search results" kaggle competition
//

// [[Rcpp::export]]
double JACCARD(std::vector<std::string> &vec1, std::vector<std::string> &vec2) {

  std::vector<std::string> SET1 = UNIQUE(vec1);

  std::vector<std::string> SET2 = UNIQUE(vec2);

  std::vector<std::string> INTSCT = INTERSECT(SET1, SET2);

  std::vector<std::string> UNI = UNION(SET1, SET2);

  double res = 0.0;

  if (UNI.size() > 0) {

    res = INTSCT.size() / ( UNI.size() * 1.0 );
  }

  return res;
}


// the DICE function is a modification of the corresponding python function of the first place solution of the "search results" kaggle competition
//

// [[Rcpp::export]]
double DICE(std::vector<std::string> &vec1, std::vector<std::string> &vec2) {

 std::vector<std::string> SET1 = UNIQUE(vec1);

  std::vector<std::string> SET2 = UNIQUE(vec2);

  std::vector<std::string> INTSCT = INTERSECT(SET1, SET2);

  double res = 0.0;

  double LEN = SET1.size() + SET2.size();

  if (LEN > 0) {

    res = ( 2.0 * INTSCT.size() ) / LEN;
  }

  return res;
}


// inner function for 'jaccard_dice' function
//

// [[Rcpp::export]]
double inner_jd(std::vector<std::vector<std::string>>& VEC1, std::vector<std::vector<std::string>>& VEC2, std::string& method, unsigned int j) {
  
  double tmp = 0.0;
  
  if (method == "jaccard") {
    
    tmp = JACCARD(VEC1[j], VEC2[j]);}
  
  if (method == "dice") {
    
    tmp = DICE(VEC1[j], VEC2[j]);
  }
  
  return tmp;
}


// compute the jaccard and dice similarity for two lists (std-vectors) of same length
//

// [[Rcpp::export]]
arma::rowvec jaccard_dice(std::vector<std::vector<std::string>>& VEC1, std::vector<std::vector<std::string>>& VEC2, std::string& method, int threads = 1) {

  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  arma::rowvec tmp_out = arma::zeros<arma::rowvec>(VEC1.size());

  unsigned int j;

  #ifdef _OPENMP
  #pragma omp parallel for schedule(static) shared(VEC1, VEC2, method, tmp_out) private(j)
  #endif
  for (j = 0; j < VEC1.size(); j++) {

    double tmp_val = inner_jd(VEC1, VEC2, method, j);

    #ifdef _OPENMP
    #pragma omp atomic write
    #endif
    tmp_out(j) = tmp_val;
  }

  return tmp_out;
}


//=============================================
// functions for 'reduce_dims_with_correlation' 
//=============================================


// secondary function for the 'COR_MATR' function
//

// [[Rcpp::export]]
double inner_cm(arma::mat& x, arma::rowvec& y, unsigned int i) {
  
  arma::rowvec tmp_out = arma::cor(arma::conv_to< arma::rowvec >::from(x.col(i)), y);
  
  double res_out = 0.0;                        // in case that 'tmp_out' is NA or +/- Inf return a correlation of 0.0
  
  if (tmp_out.is_finite()) {
    
    res_out = arma::as_scalar(tmp_out);
  }
  
  return res_out;
}



// function to calculate correlations between response and predictors [ column-wise ]
//

// [[Rcpp::export]]
arma::rowvec COR_MATR(arma::mat& x, arma::rowvec& y, int threads) {

  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  arma::rowvec out(x.n_cols);

  unsigned int i;

  #ifdef _OPENMP
  #pragma omp parallel for schedule(static) shared(x, y, out) private(i)
  #endif
  for(i = 0; i < x.n_cols; i++) {

    #ifdef _OPENMP
    #pragma omp atomic write
    #endif
    out(i) = inner_cm(x, y, i);
  }

  return arma::abs(out);
}



// secodary function for 'IDX_COR'
//

// [[Rcpp::export]]
arma::rowvec keep_idxs(arma::uvec& x, unsigned int exclude_idx) {        // 'x' vector is modified in every iteration
  
  arma::rowvec out(x.n_elem - 1, arma::fill::zeros);
  
  int iter = 0;
  
  for (unsigned int i = 0; i < x.n_elem; i++) {
    
    if (i != exclude_idx) {
      
      out(iter) = x(i);
      
      iter++;
    }
  }
  
  return out;
}



// inner function for the "reduce_dims_with_correlation" function [ due to ASAN errors ]
//

// [[Rcpp::export]]
double inner_reduce_dims(arma::mat& x, const arma::rowvec& ALL_OTHER_IDXs, unsigned int i, arma::rowvec& current_col) {
  
  arma::rowvec tmp_other_col = arma::conv_to< arma::rowvec >::from(x.col(ALL_OTHER_IDXs(i)));
  
  double tmp_scalar = arma::as_scalar(cor(current_col, tmp_other_col));
  
  return tmp_scalar;
}



// select predictors using the correlation coefficient as indicator
//

// [[Rcpp::export]]
arma::uvec reduce_dims_with_correlation(arma::mat& x, arma::rowvec& y, double response_lower_thresh = 0.2,
                                          
                                          double predictors_upper_thresh = 0.65, int threads = 1) {
  
  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif
  
  if (response_lower_thresh == 0.0 && predictors_upper_thresh == 1.0) {                                 // return all indices in case that 'response_thresh' = 0.0 AND 'predictors_thresh' = 1.0
    
    arma::uvec out(x.n_cols, arma::fill::zeros);
    
    for (unsigned int f = 0; f < x.n_cols; f++) {
      
      out(f) = f;
    }
    
    return out + 1;                                                                                       // adjust indexing ( + 1 )
  }
  
  else {
    
    arma::rowvec cor_with_y = COR_MATR(x, y, threads);                                                    // calculation of correlations between response and predictors
  
    arma::uvec sorted_idx = arma::sort_index(cor_with_y, "descend");                                      // sort index of the correlations
    
    int current_idx = 0;                                                                                  // 'current_idx' equals to 0 for all cases
  
    arma::uvec KEEP_INDICES;
    
    int ITER = 0;
    
    while(true) {                                                                                         // continue till condition is fulfilled
      
      if (sorted_idx.n_elem < 2) {                                                                        // at least 2 columns needed for the while loop, else break the while-loop
        
        break;
      }
      
      KEEP_INDICES.resize(ITER + 1);
      
      KEEP_INDICES(ITER) = sorted_idx(current_idx);
      
      int CURRENT_IDX = sorted_idx(current_idx);                                                          // index of current column to use
      
      arma::rowvec ALL_OTHER_IDXs = keep_idxs(sorted_idx, current_idx);                                   // exclude the current index
      
      arma::rowvec current_col = arma::conv_to< arma::rowvec >::from(x.col(CURRENT_IDX));                 // take the relevant column (according to index)
      
      arma::rowvec inner_calc_cor(ALL_OTHER_IDXs.n_elem, arma::fill::zeros);                              // initialize a rowvector to save the correlations [ inside the while loop ]
      
      unsigned int i;
      
      #ifdef _OPENMP
      #pragma omp parallel for schedule(static) shared(ALL_OTHER_IDXs, x, current_col, inner_calc_cor) private(i)
      #endif
      for (i = 0; i < ALL_OTHER_IDXs.n_elem; i++) {
        
        double tmp_val = inner_reduce_dims(x, ALL_OTHER_IDXs, i, current_col);
        
        #ifdef _OPENMP
        #pragma omp atomic write
        #endif
        inner_calc_cor(i) = tmp_val;
      }
      
      inner_calc_cor = arma::abs(inner_calc_cor);                                                         // use absolute [ so that negative (-0.5) and positive (0.5) correlations are equal ] otherwise the correlation threshold does not have an effect
  
      arma::uvec find_and_keep = arma::find(inner_calc_cor < predictors_upper_thresh);                    // use this query to keep the relevant indices using the correlation threshold
      
      if (find_and_keep.n_elem > 0) {
        
        sorted_idx = arma::conv_to< arma::uvec >::from(ALL_OTHER_IDXs(find_and_keep));
      }
      
      else {
        
        break;                                                                                            // if length of 'find_and_keep' vector < 1, break the while-loop
      }
      
      ITER++;
    }
    
    arma::rowvec tmp_thresh_cor = arma::conv_to< arma::rowvec >::from(cor_with_y(KEEP_INDICES));
  
    arma::uvec find_and_keep_y = arma::find(tmp_thresh_cor >= response_lower_thresh);                     // find indices less than the 'response_lower_thresh'
  
    if (find_and_keep_y.n_elem > 0) {
  
      return arma::sort(KEEP_INDICES(find_and_keep_y) + 1);                                               // use +1 to adjust for the difference in indexing between C++ and R
    }
  
    else {
  
      return arma::sort(KEEP_INDICES + 1);
    }
  }
}



//=========================================


// remainder for long-long-int
//

// [[Rcpp::export]]
long long modulus (long long a, int b) {

  return(a % b);
}


// ordered-map to calculate the boundaries of the batches [ the remainder goes to the last batch ] for long-long-int
// [ it is meant to be used directly in R, thus indexing beginns from 1 ]
//

// [[Rcpp::export]]
std::map<int, std::vector<long long>> batch_calculation(long long nr_rows, int batches) {

  std::map<int, std::vector<long long>> vec;

  long long remainder = modulus(nr_rows, batches);

  long long round_rows = std::floor(nr_rows / batches);

  for (int i = 0; i < batches; i++) {

    if (i == 0) {

      vec[i] = {1, round_rows};}

    else if (i == batches - 1) {

      vec[i] = {(round_rows * i) + 1, round_rows * (i + 1) + remainder};}

    else {

      vec[i] = {(round_rows * i) + 1, round_rows * (i + 1)};
    }
  }

  return vec;
}



//---------------------------------------------------------------------------------------------
// feature creation from text-vectors as described in :
// https://www.kaggle.com/c/home-depot-product-search-relevance/discussion/20427 by Igor Buinyi
//---------------------------------------------------------------------------------------------

// intersection of distinct words
//

// [[Rcpp::export]]
std::vector<std::string> DISTINCT_WORD_INTERSECT(std::vector<std::string> VEC1, std::vector<std::string> VEC2) {

  std::vector<std::string> tmp_inters = INTERSECT(VEC1, VEC2);         // see previously created INTERSECT() function

  tmp_inters = UNIQUE(tmp_inters);

  return tmp_inters;
}


// intersection of distinct letters
//

// [[Rcpp::export]]
arma::rowvec NUM_LETTERS_DISTINCT(std::vector<std::string> VEC) {

  arma::rowvec res(VEC.size(), arma::fill::zeros);

  for (unsigned int i = 0; i < VEC.size(); i++) {

    res(i) = VEC[i].length();
  }

  return res;
}


// counts for intersection of either words or letters for two lists of same length
//

// [[Rcpp::export]]
arma::rowvec COUNTS_INTERSECT(std::vector<std::vector<std::string>> SUBL1, std::vector<std::vector<std::string>> SUBL2,

                              bool distinct = true, bool num_letters = false) {

  arma::rowvec RES(SUBL1.size(), arma::fill::zeros);

  for (unsigned int i = 0; i < SUBL1.size(); i++) {

    std::vector<std::string> tmp;

    if (distinct) {

      tmp = DISTINCT_WORD_INTERSECT(SUBL1[i], SUBL2[i]);
    }

    else {

      tmp = INTERSECT(SUBL1[i], SUBL2[i]);
    }

    if (tmp.empty()) {

      RES(i) = 0;}

    else {

      if (num_letters) {

        RES(i) = arma::as_scalar(arma::accu(NUM_LETTERS_DISTINCT(tmp)));}        // accumulate the number of letters in the vector

      else {

        RES(i) = tmp.size();
      }
    }
  }

  return RES;
}


// ratio for intersection of either words or letters for two lists of same length
//

// [[Rcpp::export]]
arma::rowvec RATIO_DISTINCT(std::vector<std::vector<std::string>> SUBL1, std::vector<std::vector<std::string>> SUBL2, bool distinct = true, bool num_letters = false) {

  arma::rowvec RES(SUBL1.size(), arma::fill::zeros);

  for (unsigned int i = 0; i < SUBL1.size(); i++) {

    std::vector<std::string> tmp;

    std::vector<std::string> tmp_u = UNION(SUBL1[i], SUBL2[i]);

    if (distinct) {

      tmp = DISTINCT_WORD_INTERSECT(SUBL1[i], SUBL2[i]);
    }

    else {

      tmp = INTERSECT(SUBL1[i], SUBL2[i]);
    }

    if (tmp.empty() || tmp_u.empty()) {

      RES(i) = 0.0;}

    else {

      if (num_letters) {

        RES(i) = arma::as_scalar(arma::accu(NUM_LETTERS_DISTINCT(tmp))) / ( arma::as_scalar(arma::accu(NUM_LETTERS_DISTINCT(tmp_u))) * 1.0);}        // accumulate the number of letters in the vector

      else {

        RES(i) = tmp.size() / ( tmp_u.size() * 1.0 );
      }
    }
  }

  return RES;
}



// read-rows used in the 'dims_of_word_vecs' function
//

// [[Rcpp::export]]
std::string read_ROWS_wv(std::string input_file, char read_delimiter = ' ') {

  int nr_rows = 0;

  std::string line;

  std::ifstream myfile(input_file);

  while (std::getline(myfile, line, read_delimiter)) {

    if (nr_rows == 1) {                                      // read only first row to estimate the dimensions of the word vectors

      break;
    }

    nr_rows++;
  }

  myfile.close();

  boost::trim(line);

  return line;
}



// count the number of rows of a file (path to a file is required)
// by default the delimiter is the newline character (  " \ n " )
//

// [[Rcpp::export]]
long long count_rows(std::string FILE, bool verbose = false) {
  
  arma::wall_clock timer;
  
  if (verbose) {
    
    timer.tic();
  }
  
  long long nr_rows = 0;
  
  std::string line;
  
  std::ifstream myfile(FILE);
  
  while (std::getline(myfile, line)) {
    
    nr_rows++;
  }
  
  if (verbose) { 
    
    double n = timer.toc(); 
    
    Rprintf("\n"); 
    
    Rprintf("\tminutes.to.complete: %.5f", n / 60.0);
  }
  
  return nr_rows;
}


