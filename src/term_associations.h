
/**
 * Copyright (C) 2016 Lampros Mouselimis
 *
 * @file term_associations.h
 *
 * @author Lampros Mouselimis
 *
 * @date October - December 2016
 *
 * @Notes: association statistics using either a document-term-matrix or a term-document-matrix (sparse format)
 *
 * @last_modified: October 2017
 *
 **/



#ifndef __termassociations__
#define __termassociations__



template<class T>
struct associations {

  long long row_assoc;

  T count_assoc;
};



template<class T>
class associations_class {


  private:

    std::map<long long, std::vector<associations<T>>> assoc_map_;

    arma::rowvec column_indices_;

    arma::rowvec row_indices_;

    arma::vec docs_counts_;


  public:


    // initialize object member variables in the constructor

    associations_class(arma::rowvec &col_idx, arma::rowvec &row_idx, arma::vec &doc_cnt) : column_indices_(col_idx), row_indices_(row_idx), docs_counts_(doc_cnt) { }


    // mapping for columns, rows, counts [ associations ]
    //

    void associations_mapping() {

      for (unsigned long long iter = 0; iter < column_indices_.size(); iter++) {

        associations<T> asct;

        asct.row_assoc = row_indices_[iter];

        asct.count_assoc = docs_counts_[iter];

        assoc_map_[column_indices_[iter]].push_back(asct);
      }
    }


    // associations between two variables
    //

    double correlation_assoc(long long var1, long long var2, long long target_size) {

      arma::rowvec res_vec1(target_size, arma::fill::zeros);

      arma::rowvec res_vec2(target_size, arma::fill::zeros);

      for (auto& it : assoc_map_[var1]) {

        res_vec1(it.row_assoc) = it.count_assoc;
      }

      for (auto& it1 : assoc_map_[var2]) {

        res_vec2(it1.row_assoc) = it1.count_assoc;
      }

      return arma::as_scalar(arma::cor(res_vec1, res_vec2));
    }


    // struct for the 'correlation_assoc_mult' function
    //

    struct struct_map_assoc {

      long long index;

      double correlation;
    };


    // secondary function to sort the counts in the 'struct_map_assoc'
    //

    static bool sort_by_correlation(const struct_map_assoc &a, const struct_map_assoc &b) {

      return a.correlation > b.correlation;
    }


    // associations for a single target-variable
    //

    Rcpp::List correlation_assoc_single(long long target_var, long long target_size, std::vector<std::string> Terms, long long keepTerms = 0) {

      std::map<long long, std::vector<associations<T>>> copy_assoc = assoc_map_;

      arma::rowvec res_vec1(target_size, arma::fill::zeros);

      for (auto& it : copy_assoc[target_var]) {

        res_vec1(it.row_assoc) = it.count_assoc;
      }

      if (arma::accu(res_vec1) == 0.0) {

        Rcpp::Rcout << "" << std::endl;

        Rcpp::Rcout << "warning: the '" << Terms[target_var - 1] << "' variable sums to zero" << std::endl;
      }

      copy_assoc.erase(target_var);

      std::vector<long long> copy_keys;

      boost::copy(copy_assoc | boost::adaptors::map_keys, std::back_inserter(copy_keys));

      std::vector<struct_map_assoc> cor_reslt(copy_keys.size());

      for (unsigned long long f = 0; f < copy_keys.size(); f++) {

        arma::rowvec res_vec2(target_size, arma::fill::zeros);

        std::vector<associations<T>> tmp_v_struct = copy_assoc[copy_keys[f]];

        for (unsigned long long g = 0; g < tmp_v_struct.size(); g++) {

          res_vec2(tmp_v_struct[g].row_assoc) = tmp_v_struct[g].count_assoc;
        }

        struct_map_assoc tmp_cor;

        tmp_cor.index = copy_keys[f];

        tmp_cor.correlation = arma::as_scalar(arma::cor(res_vec1, res_vec2));

        cor_reslt[f] = tmp_cor;
      }

      std::sort(cor_reslt.begin(), cor_reslt.end(), sort_by_correlation);

      long long cor_reslt_size = cor_reslt.size();

      long long kt_iter = keepTerms == 0 ? cor_reslt_size : keepTerms;

      if (keepTerms > cor_reslt_size) {

        kt_iter = cor_reslt_size;
      }

      std::vector<std::string> sorted_index(kt_iter);

      arma::rowvec sorted_correlation(kt_iter);

      for (long long ITER = 0; ITER < kt_iter; ITER++) {

        sorted_index[ITER] = Terms[cor_reslt[ITER].index - 1];

        sorted_correlation(ITER) = cor_reslt[ITER].correlation;
      }

      return(Rcpp::List::create( Rcpp::Named("term") = sorted_index, Rcpp::Named("correlation") = sorted_correlation));
    }


    // associations for multiple target-variables
    //

    Rcpp::List correlation_assoc_multiple(std::vector<int> targ_vars, long long target_size, std::vector<std::string> Terms, long long keepTerms = 0, bool verbose = false) {

      Rcpp::List assoc_lst(targ_vars.size());

      for (unsigned int count_var = 0; count_var < targ_vars.size(); count_var++) {

        if (verbose) { Rprintf("\rtotal.number.variables.processed: %3d", count_var + 1); }

        assoc_lst[count_var] = correlation_assoc_single(targ_vars[count_var], target_size, Terms, keepTerms);
      }

      return assoc_lst;
    }

    ~associations_class() { }
};



#endif

