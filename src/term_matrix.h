
/**
 * Copyright (C) 2016 Lampros Mouselimis
 *
 * @file term_matrix.h
 *
 * @author Lampros Mouselimis
 *
 * @date October - December 2016
 *
 * @Notes: document-term-matrix or term-document-matrix in sparse format
 *
 * @last_modified: October 2017
 *
 **/



#ifndef __termmatrix__
#define __termmatrix__


//-------------------------
// include dependencies:

#include "token_big_files.h"
#include "sort_2dim_vecs.h"

//-------------------------


#ifdef _OPENMP
#include <omp.h>
#endif


class term_matrix {

  private:

    std::vector<std::string> terms;

    arma::sp_mat tmat;

    arma::rowvec column_indices_;

    arma::rowvec row_indices_;

    arma::vec docs_counts_;

    SORT_2DIMENSIONAL_VEC<std::string, long long> s2dv;

    big_files bgf;

  public:

    term_matrix() { }


    // tokenization function used in 'document_term_matrix'
    //

    std::vector<std::string> dtm_token(std::string x, std::vector<std::string> language, std::string language_spec, std::string LOCALE_UTF, bool FLAG_path, char read_file_delimiter,

                                       long long max_num_char, std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false, bool cpp_remove_punctuation = false,

                                       bool remove_punctuation_vector = false, bool cpp_remove_numbers = false, bool cpp_trim_token = false, bool cpp_tokenization_function = false,

                                       std::string cpp_string_separator = "-*", bool cpp_remove_stopwords = false, int min_num_char = 1, std::string stemmer = "NULL", int min_n_gram = 1,

                                       int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0, std::string n_gram_delimiter = " ", std::string concat_delimiter = "NULL",

                                       std::string path_2file = "", int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1, int threads = 1,

                                       bool verbose = false, bool save_2single_file = false, std::string path_extend = "output_token.txt", std::string vocabulary_path = "") {


      return bgf.res_TOKEN(x, language, language_spec, LOCALE_UTF, false, read_file_delimiter, max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation,

                           remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords,

                           min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, concat_delimiter, path_2file,

                           stemmer_ngram, stemmer_gamma, stemmer_truncate, stemmer_batches, threads, verbose, false, path_extend, "");
    }



    // batch-insertion function for sparse matrices
    //

    arma::sp_mat batch_insertion(arma::rowvec ROWS, arma::rowvec COLS, arma::vec values) {

      arma::mat locations_tmp(2, ROWS.n_elem, arma::fill::zeros);

      locations_tmp.row(0) = ROWS;

      locations_tmp.row(1) = COLS;

      arma::umat locations = arma::conv_to< arma::umat >::from(locations_tmp);

      arma::sp_mat convert_2sparse(locations, values);

      return convert_2sparse;
    }



    // struct for 'l1', 'l2' normalization
    //

    template<class T>
    struct keep_idx {

      long long idx;

      T count;
    };


    // sort function for 'l1', 'l2' normalization

    static bool sort_by_norm(const keep_idx<double> &a, const keep_idx<double> &b) {

      return a.idx < b.idx;
    }


    // main function for 'l1', 'l2' normalization [ it will be used only in tf_idf with 'double' as data type ]
    //

    template<class T>
    arma::vec l1_l2_norm(arma::rowvec row_docs, std::vector<T> count_or_tfidf, std::string normalization = "l1") {

      std::map<long long, std::vector<keep_idx<T>>> norm_map;              // mapping of rows [ each row is a different document ]

      for (unsigned long long i = 0; i < row_docs.n_elem; i++) {

        keep_idx<T> kdx;

        kdx.idx = i;

        kdx.count = count_or_tfidf[i];

        norm_map[row_docs[i]].push_back(kdx);
      }

      std::vector<std::vector<keep_idx<T>>> copy_norm;

      boost::copy(norm_map | boost::adaptors::map_values, std::back_inserter(copy_norm));

      std::vector<keep_idx<T>> RES_struct;

      long long count_resize = 0;

      for (unsigned long long j = 0; j < copy_norm.size(); j++) {

        std::vector<keep_idx<T>> tmp_VEC = copy_norm[j];

        std::vector<long long> tmp_idx(tmp_VEC.size());

        arma::rowvec tmp_counts(tmp_VEC.size());

        double SUM = 0.0;

        for (unsigned long long k = 0; k < tmp_VEC.size(); k++) {

          tmp_idx[k] = tmp_VEC[k].idx;

          tmp_counts(k) = tmp_VEC[k].count;

          if (normalization == "l1") {

            SUM += std::abs(tmp_VEC[k].count);}

          if (normalization == "l2") {

            SUM += std::pow(tmp_VEC[k].count, 2.0);
          }
        }

        for (unsigned long long f = 0; f < tmp_counts.n_elem; f++) {

          keep_idx<T> kdx1;

          if (tmp_counts[f] > 0.0) {

            if (normalization == "l1") {

              kdx1.count = tmp_counts[f] / SUM;}

            if (normalization == "l2") {

              double tmp_val = tmp_counts[f] / std::sqrt(SUM);

              //kdx1.count = std::pow(tmp_val, 2.0);

              kdx1.count = tmp_val;
            }
          }

          else {

            kdx1.count = 0.0;
          }

          kdx1.idx = tmp_idx[f];

          RES_struct.resize(count_resize + 1);

          RES_struct[count_resize] = kdx1;

          count_resize++;
        }
      }

      std::sort(RES_struct.begin(), RES_struct.end(), sort_by_norm);

      arma::vec out_res(RES_struct.size());

      long long ITER1 = 0;

      for (auto& it : RES_struct) {

        out_res[ITER1] = it.count;

        ITER1++;
      }

      return out_res;
    }



    // document-term-matrix
    // http://blog.christianperone.com/2011/10/machine-learning-text-feature-extraction-tf-idf-part-ii/ --> for 'l1', 'l2' normalization
    //

    void document_term_matrix(std::vector<std::string> vector_corpus, std::vector<std::string> language, std::string language_spec, std::string LOCALE_UTF, long long max_num_char, std::string path_2documents_file = "NULL",

                              bool sort_columns = false, std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false, bool cpp_remove_punctuation = false, bool remove_punctuation_vector = false,

                              bool cpp_remove_numbers = false, bool cpp_trim_token = false, bool cpp_tokenization_function = false, std::string cpp_string_separator = "-*", bool cpp_remove_stopwords = false,

                              int min_num_char = 1, std::string stemmer = "NULL", int min_n_gram = 1, int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0, std::string n_gram_delimiter = " ",

                              int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1, int threads = 1, bool verbose = false, long long print_every_rows = 1000,

                              std::string normalize_tf = "NULL", bool tf_idf = false) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      arma::wall_clock timer, timer_all;

      if (verbose) { timer.tic(); timer_all.tic(); Rprintf("\n"); }

      std::vector<long long> row_indices_docs;

      std::vector<long long> docs_counts;

      std::vector<double> docs_counts_idf;

      std::vector<std::string> docs_words;

      std::unordered_map<std::string, long long> unique_words;                      // unique_words is (key, value) = (unique_word, unique_index)

      long long count = 0;

      long long tmp_print_rows = print_every_rows;                                  // calling the Rprintf() function with the '\r' argument very often, slows down the function

      if (path_2documents_file != "NULL") {                                         //  option 1 : read data from file

        std::ifstream myfile(path_2documents_file);

        std::string line;

        while (std::getline(myfile, line)) {

          std::map<std::string, long long> tmp_docs_counts_words;

          std::map<std::string, double> tmp_docs_idf;

          std::map<std::string, double> tmp_normalize_tfidf;

          if (line.length() == 0) {

            if (normalize_tf != "NULL" && !tf_idf) {

              tmp_docs_idf[""] += 1.0;}

            if (normalize_tf == "NULL" && !tf_idf) {

              tmp_docs_counts_words[""] += 1;}                                        // assign "" to empty lines

            if (tf_idf) {

              tmp_normalize_tfidf[""] += 1.0;
            }
          }

          else {

            // disable threads here as I do have to do with sentences (small-to-medium length of documents), rather than with documents of thousands of words (more than 1 threads increase the system time)

            std::vector<std::string> tmp_vec = dtm_token(line, language, language_spec, LOCALE_UTF, false, '\t', max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation, remove_punctuation_vector,

                                                         cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords, min_num_char, stemmer, min_n_gram, max_n_gram,

                                                         skip_n_gram, skip_distance, n_gram_delimiter, "NULL", "", stemmer_ngram, stemmer_gamma, stemmer_truncate, stemmer_batches, 1, false, false, "output_token.txt", "");
            if (tmp_vec.size() == 0) {

              if (normalize_tf != "NULL" && !tf_idf) {

                tmp_docs_idf[""] += 1.0;}

              if (normalize_tf == "NULL" && !tf_idf) {

                tmp_docs_counts_words[""] += 1;}                                                                            // assign "" to empty vectors

              if (tf_idf) {

                tmp_normalize_tfidf[""] += 1.0;
              }
            }

            else {

              for (unsigned long long i = 0; i < tmp_vec.size(); i++) {

                if (normalize_tf != "NULL" && !tf_idf) {

                  tmp_docs_idf[tmp_vec[i]] += 1.0;}

                if (normalize_tf == "NULL" && !tf_idf) {

                  tmp_docs_counts_words[tmp_vec[i]] += 1;}                                                                  // temporary words-counts of tokenize-transform

                if (tf_idf) {

                  tmp_normalize_tfidf[tmp_vec[i]] += 1.0;
                }
              }
            }
          }

          if (normalize_tf != "NULL" && !tf_idf) {

            for (auto& iter_unq : tmp_docs_idf) { unique_words[iter_unq.first] += 1; }}

          if (normalize_tf == "NULL" && !tf_idf) {

            for (auto& iter_unq : tmp_docs_counts_words) { unique_words[iter_unq.first] += 1; }}

          if (tf_idf) {

           for (auto& iter_unq : tmp_normalize_tfidf) { unique_words[iter_unq.first] += 1; }
          }

          if ((normalize_tf != "NULL" && !tf_idf) && tmp_docs_idf.size() > 1) {

            double sum_tfidf_elems = 0.0;

            if (normalize_tf == "l1") {

              for (auto& tf_n : tmp_docs_idf) { sum_tfidf_elems += std::abs(tf_n.second); }

              for (auto& it: tmp_docs_idf) { it.second /= sum_tfidf_elems; }
            }

            if (normalize_tf == "l2") {

              for (auto& tf_n : tmp_docs_idf) { sum_tfidf_elems += std::pow(tf_n.second, 2.0); }

              for (auto& it: tmp_docs_idf) {

                it.second /= std::sqrt(sum_tfidf_elems);

                //it.second *= it.second;
              }
            }
          }

          if (tf_idf && tmp_normalize_tfidf.size() > 1) {

            double sum_tfidf_elems = 0.0;

            for (auto& tf_n : tmp_normalize_tfidf) { sum_tfidf_elems += tf_n.second; }

            for (auto& it: tmp_normalize_tfidf) { it.second /= sum_tfidf_elems; }
          }

          long long tmp_size_vec = 0;

          if (normalize_tf != "NULL" && !tf_idf) {

            tmp_size_vec = tmp_docs_idf.size();}

          if (normalize_tf == "NULL" && !tf_idf) {

            tmp_size_vec = tmp_docs_counts_words.size();}

          if (tf_idf) {

            tmp_size_vec = tmp_normalize_tfidf.size();
          }

          std::vector<long long> tmp_idx_docs(tmp_size_vec);

          std::fill(tmp_idx_docs.begin(), tmp_idx_docs.end(), count);                                                     // sequence indices of the current vector

          row_indices_docs.insert(std::end(row_indices_docs), std::begin(tmp_idx_docs), std::end(tmp_idx_docs));          // (whole) sequence of indices

          std::vector<long long> docs_values;

          std::vector<double> docs_values_idf;

          std::vector<std::string> docs_keys;

          if (normalize_tf != "NULL" && !tf_idf) {

            boost::copy(tmp_docs_idf | boost::adaptors::map_values, std::back_inserter(docs_values_idf));                 // copy doc-values

            docs_counts_idf.insert(std::end(docs_counts_idf), std::begin(docs_values_idf), std::end(docs_values_idf));    // (whole) sequence of doc-values

            boost::copy(tmp_docs_idf | boost::adaptors::map_keys, std::back_inserter(docs_keys));                         // copy doc-keys

            docs_words.insert(std::end(docs_words), std::begin(docs_keys), std::end(docs_keys));}                         // (whole) sequence of docs-keys

          if (normalize_tf == "NULL" && !tf_idf) {

            boost::copy(tmp_docs_counts_words | boost::adaptors::map_values, std::back_inserter(docs_values));

            docs_counts.insert(std::end(docs_counts), std::begin(docs_values), std::end(docs_values));

            boost::copy(tmp_docs_counts_words | boost::adaptors::map_keys, std::back_inserter(docs_keys));

            docs_words.insert(std::end(docs_words), std::begin(docs_keys), std::end(docs_keys));}

          if (tf_idf) {

            boost::copy(tmp_normalize_tfidf | boost::adaptors::map_values, std::back_inserter(docs_values_idf));

            docs_counts_idf.insert(std::end(docs_counts_idf), std::begin(docs_values_idf), std::end(docs_values_idf));

            boost::copy(tmp_normalize_tfidf | boost::adaptors::map_keys, std::back_inserter(docs_keys));

            docs_words.insert(std::end(docs_words), std::begin(docs_keys), std::end(docs_keys));
          }

          if (verbose) {

            if (count + 1 == tmp_print_rows) {

              Rprintf("\rtotal.number.lines.processed.tokenization: %3d", count + 1);

              tmp_print_rows += print_every_rows;
            }
          }

          count++;
        }
      }

      else {                                                                                        //  option 2 : read data from a vector (each subvector is a character string i.e. sentence)

        for (unsigned int subvec = 0; subvec < vector_corpus.size(); subvec++) {

          std::map<std::string, long long> tmp_docs_counts_words;

          std::map<std::string, double> tmp_docs_idf;

          std::map<std::string, double> tmp_normalize_tfidf;

          if (vector_corpus[subvec].size() == 0) {

            if (normalize_tf != "NULL" && !tf_idf) {

              tmp_docs_idf[""] += 1.0;}

            if (normalize_tf == "NULL" && !tf_idf) {

              tmp_docs_counts_words[""] += 1;}                                        // assign "" to empty lines

            if (tf_idf) {

              tmp_normalize_tfidf[""] += 1.0;
            }
          }

          else {

            // disable threads here as I do have to do with sentences (small-to-medium length of documents), rather than with documents of thousands of words (more than 1 threads increase the system time)

            std::vector<std::string> tmp_vec = dtm_token(vector_corpus[subvec], language, language_spec, LOCALE_UTF, false, '\t', max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation,

                                                         remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords, min_num_char, stemmer,

                                                         min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, "NULL", "", stemmer_ngram, stemmer_gamma, stemmer_truncate, stemmer_batches, 1,

                                                         false, false, "output_token.txt", "");
            if (tmp_vec.size() == 0) {

              if (normalize_tf != "NULL" && !tf_idf) {

                tmp_docs_idf[""] += 1.0;}

              if (normalize_tf == "NULL" && !tf_idf) {

                tmp_docs_counts_words[""] += 1;}                                                                            // assign "" to empty vectors

              if (tf_idf) {

                tmp_normalize_tfidf[""] += 1.0;
              }
            }

            else {

              for (unsigned long long i = 0; i < tmp_vec.size(); i++) {

                if (normalize_tf != "NULL" && !tf_idf) {

                  tmp_docs_idf[tmp_vec[i]] += 1.0;}

                if (normalize_tf == "NULL" && !tf_idf) {

                  tmp_docs_counts_words[tmp_vec[i]] += 1;}                                                                  // temporary words-counts of tokenize-transform

                if (tf_idf) {

                  tmp_normalize_tfidf[tmp_vec[i]] += 1.0;
                }
              }
            }
          }

          if (normalize_tf != "NULL" && !tf_idf) {

            for (auto& iter_unq : tmp_docs_idf) { unique_words[iter_unq.first] += 1; }}

          if (normalize_tf == "NULL" && !tf_idf) {

            for (auto& iter_unq : tmp_docs_counts_words) { unique_words[iter_unq.first] += 1; }}

          if (tf_idf) {

            for (auto& iter_unq : tmp_normalize_tfidf) { unique_words[iter_unq.first] += 1; }
          }

          if ((normalize_tf != "NULL" && !tf_idf) && tmp_docs_idf.size() > 1) {

            double sum_tfidf_elems = 0.0;

            if (normalize_tf == "l1") {

              for (auto& tf_n : tmp_docs_idf) { sum_tfidf_elems += std::abs(tf_n.second); }

              for (auto& it: tmp_docs_idf) { it.second /= sum_tfidf_elems; }
            }

            if (normalize_tf == "l2") {

              for (auto& tf_n : tmp_docs_idf) { sum_tfidf_elems += std::pow(tf_n.second, 2.0); }

              for (auto& it: tmp_docs_idf) {

                it.second /= std::sqrt(sum_tfidf_elems);

                //it.second *= it.second;
              }
            }
          }

          if (tf_idf && tmp_normalize_tfidf.size() > 1) {

            double sum_tfidf_elems = 0.0;

            for (auto& tf_n : tmp_normalize_tfidf) { sum_tfidf_elems += tf_n.second; }

            for (auto& it: tmp_normalize_tfidf) { it.second /= sum_tfidf_elems; }
          }

          long long tmp_size_vec = 0;

          if (normalize_tf != "NULL" && !tf_idf) {

            tmp_size_vec = tmp_docs_idf.size();}

          if (normalize_tf == "NULL" && !tf_idf) {

            tmp_size_vec = tmp_docs_counts_words.size();}

          if (tf_idf) {

            tmp_size_vec = tmp_normalize_tfidf.size();
          }

          std::vector<long long> tmp_idx_docs(tmp_size_vec);

          std::fill(tmp_idx_docs.begin(), tmp_idx_docs.end(), count);                                                     // sequence indices of the current vector

          row_indices_docs.insert(std::end(row_indices_docs), std::begin(tmp_idx_docs), std::end(tmp_idx_docs));          // (whole) sequence of indices

          std::vector<long long> docs_values;

          std::vector<double> docs_values_idf;

          std::vector<std::string> docs_keys;

          if (normalize_tf != "NULL" && !tf_idf) {

            boost::copy(tmp_docs_idf | boost::adaptors::map_values, std::back_inserter(docs_values_idf));                 // copy doc-values

            docs_counts_idf.insert(std::end(docs_counts_idf), std::begin(docs_values_idf), std::end(docs_values_idf));    // (whole) sequence of doc-values

            boost::copy(tmp_docs_idf | boost::adaptors::map_keys, std::back_inserter(docs_keys));                         // copy doc-keys

            docs_words.insert(std::end(docs_words), std::begin(docs_keys), std::end(docs_keys));}                         // (whole) sequence of docs-keys

          if (normalize_tf == "NULL" && !tf_idf) {

            boost::copy(tmp_docs_counts_words | boost::adaptors::map_values, std::back_inserter(docs_values));

            docs_counts.insert(std::end(docs_counts), std::begin(docs_values), std::end(docs_values));

            boost::copy(tmp_docs_counts_words | boost::adaptors::map_keys, std::back_inserter(docs_keys));

            docs_words.insert(std::end(docs_words), std::begin(docs_keys), std::end(docs_keys));}

          if (tf_idf) {

            boost::copy(tmp_normalize_tfidf | boost::adaptors::map_values, std::back_inserter(docs_values_idf));

            docs_counts_idf.insert(std::end(docs_counts_idf), std::begin(docs_values_idf), std::end(docs_values_idf));

            boost::copy(tmp_normalize_tfidf | boost::adaptors::map_keys, std::back_inserter(docs_keys));

            docs_words.insert(std::end(docs_words), std::begin(docs_keys), std::end(docs_keys));
          }

          if (verbose) {

            if (count + 1 == tmp_print_rows) {

              Rprintf("\rtotal.number.lines.processed.tokenization: %3d", count + 1);

              tmp_print_rows += print_every_rows;
            }
          }

          count++;
        }
      }

      long long add_iter = 0;

      std::unordered_map<std::string, long long> unique_words_copy;

      if (tf_idf) { unique_words_copy = unique_words; }                                      // copy the unique words in case of tf-idf

      std::vector<std::string> return_unq_words(unique_words.size());                        // vector of unique words

      std::map<std::string, long long> unique_words_sorted;

      for (auto& it: unique_words) {

        if (sort_columns) {

          unique_words_sorted[it.first] = 0;}

        else {

          it.second = add_iter;                                                              // overwrite document-term-counts with indices of unique words

          return_unq_words[add_iter] = it.first;
        }

        add_iter++;
      }

      long long add_iter1 = 0;

      if (sort_columns) {

        for (auto& it1 : unique_words_sorted) {

          it1.second = add_iter1;

          return_unq_words[add_iter1] = it1.first;

          add_iter1++;
        }
      }

      terms = return_unq_words;

      return_unq_words.shrink_to_fit();

      if (verbose) {

        double n = timer.toc();

        Rprintf("\tminutes.to.tokenize.transform.data: %.5f", n / 60.0);
      }

      arma::rowvec column_indices_docs(docs_words.size());

      arma::vec tfidf_docs(docs_words.size());

      unsigned long long j;
      
      #ifdef _OPENMP
      #pragma omp parallel for schedule(static) shared(docs_words, sort_columns, column_indices_docs, unique_words_sorted, unique_words, tf_idf, tfidf_docs, docs_counts_idf, count, unique_words_copy) private(j)
      #endif
      for (j = 0; j < docs_words.size(); j++) {

        if (sort_columns) {

          #ifdef _OPENMP
          #pragma omp atomic write
          #endif
          column_indices_docs(j) = unique_words_sorted[docs_words[j]];}

        else {

          #ifdef _OPENMP
          #pragma omp atomic write
          #endif
          column_indices_docs(j) = unique_words[docs_words[j]];                                                                // match docs-words with unique words to get the column indices
        }

        if (tf_idf) {

          #ifdef _OPENMP
          #pragma omp atomic write
          #endif
          tfidf_docs(j) = docs_counts_idf[j] * std::log( (count * 1.0) /  (1.0 + unique_words_copy[docs_words[j]]) );          // tf-idf : by default natural logarithm AND add 1.0 to the denominator to avoid zero division
        }
      }

      arma::rowvec row_indices_docs_arma = arma::conv_to< arma::rowvec >::from(row_indices_docs);

      row_indices_ = row_indices_docs_arma;

      column_indices_ = column_indices_docs;      // save row, column indices for associations

      if (normalize_tf == "NULL" && !tf_idf) {

        arma::vec docs_counts_arma = arma::conv_to< arma::vec >::from(docs_counts);

        docs_counts_ = docs_counts_arma;

        tmat = batch_insertion(row_indices_docs_arma, column_indices_docs, docs_counts_arma);
      }

      if (normalize_tf != "NULL" && !tf_idf) {

        arma::vec docs_counts_arma = arma::conv_to< arma::vec >::from(docs_counts_idf);

        docs_counts_ = docs_counts_arma;

        tmat = batch_insertion(row_indices_docs_arma, column_indices_docs, docs_counts_arma);
      }

      if (tf_idf) {

        if (normalize_tf == "l2") {

          std::vector<double> TMP_std = arma::conv_to< std::vector<double> >::from(tfidf_docs);

          arma::vec tmp_tfidf_docs = l1_l2_norm(row_indices_docs_arma, TMP_std, normalize_tf);     // template of data type 'double' for l2 normalization

          docs_counts_ = tmp_tfidf_docs;

          tmat = batch_insertion(row_indices_docs_arma, column_indices_docs, tmp_tfidf_docs);}

        else {

          docs_counts_ = tfidf_docs;

          tmat = batch_insertion(row_indices_docs_arma, column_indices_docs, tfidf_docs);
        }
      }

      if (verbose) {

        double n1 = timer_all.toc();

        Rprintf("\ttotal.time: %.5f", n1 / 60.0);
      }
    }


    // term-document-matrix
    //

    void term_document_matrix(std::vector<std::string> vector_corpus, std::vector<std::string> language, std::string language_spec, std::string LOCALE_UTF, long long max_num_char, std::string path_2documents_file = "NULL",

                              bool sort_columns = false, std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false, bool cpp_remove_punctuation = false, bool remove_punctuation_vector = false,

                              bool cpp_remove_numbers = false, bool cpp_trim_token = false, bool cpp_tokenization_function = false, std::string cpp_string_separator = "-*", bool cpp_remove_stopwords = false,

                              int min_num_char = 1, std::string stemmer = "NULL", int min_n_gram = 1, int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0, std::string n_gram_delimiter = " ",

                              int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1, int threads = 1, bool verbose = false, long long print_every_rows = 1000,

                              std::string normalize_tf = "NULL",bool tf_idf = false) {


      document_term_matrix(vector_corpus, language, language_spec, LOCALE_UTF, max_num_char, path_2documents_file, sort_columns, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation, remove_punctuation_vector,

                           cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords, min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance,

                           n_gram_delimiter, stemmer_ngram, stemmer_gamma, stemmer_truncate, stemmer_batches, threads, verbose, print_every_rows, normalize_tf, tf_idf);

      tmat = tmat.t();
    }



    // returns the sparsity-percentage of the sparse matrix
    //

    double sparsity(arma::sp_mat data) {

      arma::vec non_sparse = arma::nonzeros(data);

      double tot_num_dims = data.n_cols * data.n_rows;

      double sparsity_tmp = 1.0 - (non_sparse.n_elem / tot_num_dims);

      ngram_stemmer ngram;

      double res_perc = ngram.round_rcpp(sparsity_tmp, 6) * 100.0;

      return res_perc;
    }



    // adjust the sparsity of a term-matrix
    //

    Rcpp::List adj_Sparsity(arma::rowvec column_indices, arma::rowvec row_indices, arma::vec docs_counts, std::vector<std::string> Terms, double sparsity_thresh = 1.0) {

      std::unordered_map<long long, long long> map_col_indices;

      for (unsigned long long i = 0; i < column_indices.n_elem; i++) {

        map_col_indices[column_indices[i]]++;
      }

      arma::uvec unq_idx = arma::find_unique(row_indices);

      double unq_docs = unq_idx.n_elem;

      std::vector<long long> all_indices;

      std::vector<long long> new_col_indices;

      long long increment = 0;

      std::vector<std::string> adj_new_terms;

      for (auto &kv : map_col_indices) {

        double sparsity = 1.0 - kv.second / unq_docs;

        if (sparsity < sparsity_thresh) {                     // the 'sparsity_thresh' excludes terms with sparsity equal to the value of sparsity_thresh

          arma::uvec all_tmp_idx = arma::find(column_indices == kv.first);

          adj_new_terms.resize(increment + 1);

          adj_new_terms[increment] = Terms[kv.first];         // update the terms using the kv.first as index

          arma::rowvec tmp_new_vec(all_tmp_idx.n_elem);

          tmp_new_vec.fill(increment);

          std::vector<long long> tmp_vec = arma::conv_to< std::vector<long long> >::from(all_tmp_idx);

          std::vector<long long> new_vec = arma::conv_to< std::vector<long long> >::from(tmp_new_vec);

          all_indices.insert(std::end(all_indices), std::begin(tmp_vec), std::end(tmp_vec));

          new_col_indices.insert(std::end(new_col_indices), std::begin(new_vec), std::end(new_vec));

          increment++;
        }
      }

      if (all_indices.empty() || new_col_indices.empty()) {

        return(Rcpp::List::create( Rcpp::Named("empty_indices") = arma::datum::inf));}

      else {

        arma::rowvec new_COLS = arma::conv_to< arma::rowvec >::from(new_col_indices);

        arma::rowvec row1(all_indices.size());

        arma::vec cnt1(all_indices.size());

        for (unsigned long long j = 0; j < all_indices.size(); j++) {

          row1(j) = row_indices(all_indices[j]);

          cnt1(j) = docs_counts(all_indices[j]);
        }

        arma::sp_mat sp_out = batch_insertion(row1, new_COLS, cnt1);

        return(Rcpp::List::create( Rcpp::Named("sparse_matrix") = sp_out, Rcpp::Named("terms") = adj_new_terms, Rcpp::Named("update_cols") = new_COLS,

                                   Rcpp::Named("update_rows") = row1, Rcpp::Named("update_counts") = cnt1));
      }
    }



    // rowSums, colSums of a sparse matrix
    //

    arma::rowvec Sparse_Sums(arma::sp_mat sp_data, bool rowSums = false) {

      arma::sp_mat norm_col_sums;

      arma::mat tmp_mat;

      if (rowSums) {

        norm_col_sums = arma::sum(sp_data, 1);

        tmp_mat = arma::conv_to< arma::mat >::from(norm_col_sums.col(0));}

      else {

        norm_col_sums = arma::sum(sp_data, 0);

        tmp_mat = arma::conv_to< arma::mat >::from(norm_col_sums.row(0));
      }

      arma::rowvec tmp_vec = arma::conv_to< arma::rowvec >::from(tmp_mat);

      return tmp_vec;
    }



    // most frequent terms
    //

    Rcpp::List most_freq_terms(arma::sp_mat sparse_data, std::vector<std::string> Terms, long long keepTerms = 0, bool flag_dtm = false, int threads = 1) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      arma::rowvec sps;

      if (flag_dtm) {                // assume document-term-matrix and counts [ not tf-normalized OR tf-idf ]

        sps = Sparse_Sums(sparse_data, false);}

      else {                        // assume term-document-matrix  [ not tf-normalized OR tf-idf ]

        sps = Sparse_Sums(sparse_data, true);
      }

      std::vector<long long> sps1 = arma::conv_to< std::vector<long long> >::from(sps);

      std::vector<STRUCT<std::string, long long>> vec_freq = s2dv.inner_sort_func_VEC(Terms, sps1, false, false);

      long long kt_iter = keepTerms == 0 ? vec_freq.size() : keepTerms;

      long long tmp_vec_freq_size = vec_freq.size();

      if (keepTerms > tmp_vec_freq_size) {

        kt_iter = tmp_vec_freq_size;
      }

      std::vector<std::string> sorted_terms(kt_iter);

      arma::rowvec sorted_frequency(kt_iter);

      long long ITER;
      
      #ifdef _OPENMP
      #pragma omp parallel for schedule(static) shared(kt_iter, vec_freq, sorted_terms, sorted_frequency) private(ITER)
      #endif
      for (ITER = 0; ITER < kt_iter; ITER++) {

        #ifdef _OPENMP
        #pragma omp critical
        #endif
        {
          sorted_terms[ITER] = vec_freq[ITER].VAR1;

          sorted_frequency(ITER) = vec_freq[ITER].VAR2;
        }
      }

      return(Rcpp::List::create( Rcpp::Named("term") = sorted_terms, Rcpp::Named("frequency") = sorted_frequency));
    }


    // struct to return the terms, sparse-matrix
    //

    struct struct_term_matrix {

      std::vector<std::string> terms_out;

      arma::sp_mat sp_mat_out;

      arma::rowvec col_idx_;

      arma::rowvec row_idx_;

      arma::vec docs_cnt_;
    };



    // return the data
    //

    struct_term_matrix output_data() {

      return {terms, tmat, column_indices_, row_indices_, docs_counts_};
    }


    ~term_matrix() { }
};



#endif

