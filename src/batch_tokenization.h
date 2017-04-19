/**
 * Copyright (C) 2016 Lampros Mouselimis
 *
 * @file batch_tokenization.h
 *
 * @author Lampros Mouselimis
 *
 * @date October - December 2016
 *
 * @Notes: tokenization and transformation of text files in batches
 *
 * @last_modified: December 2016
 *
 **/

#ifndef __batchtokenization__
#define __batchtokenization__


//-------------------------
// include dependencies:

#include "porter2_stemmer.h"
#include "ngram_stemmer.h"
#include "tokenization.h"

//-------------------------


class BATCH_TOKEN {

  public:

    BATCH_TOKEN() { }


    // count the number of rows of a file [ it is the main difference between the BATCH_TOKEN and the big_files class, as it counts rows rather than bytes ]
    //

    long long count_rows(std::string FILE) {

      long long nr_rows = 0;

      std::string line;

      std::ifstream myfile(FILE);

      while (std::getline(myfile, line)) {

        nr_rows++;
      }

      return nr_rows;
    }


    // save a string to a file
    //

    void save_string(std::string x, std::string file = "output.txt") {

      std::ofstream out(file);

      out << x;

      out.close();
    }


    // remainder for long-long-int
    //

    long long modulus (long long a, int b) {

      return(a % b);
    }


    // ordered-map to calculate the boundaries of the batches [ the remainder goes to the last batch ] for long-long-int
    //

    std::map<int, std::vector<long long>> batch_calculation(long long nr_rows, int batches) {

      std::map<int, std::vector<long long>> vec;

      long long remainder = modulus(nr_rows, batches);

      long long round_rows = std::floor(nr_rows / batches);

      for (int i = 0; i < batches; i++) {

        if (i == 0) {

          vec[i] = {0, round_rows - 1};}

        else if (i == batches - 1) {

          vec[i] = {(round_rows * i), round_rows * (i + 1) + remainder - 1};}

        else {

          vec[i] = {(round_rows * i), round_rows * (i + 1) - 1};
        }
      }

      return vec;
    }


    // secondary function based on class token
    //

    std::vector<std::string> TOKEN_batch(std::vector<std::string> &VEC, std::vector<std::string> language, std::string language_spec, std::string LOCALE_UTF, int max_num_char,

                                         std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false, bool cpp_remove_punctuation = false,

                                         bool remove_punctuation_vector = false, bool cpp_remove_numbers = false, bool cpp_trim_token = false,

                                         bool cpp_tokenization_function = false, std::string cpp_string_separator = " \r\n\t.,;:()?!//", bool cpp_remove_stopwords = false,

                                         int min_num_char = 1, std::string stemmer = "NULL", int min_n_gram = 1, int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0,

                                         std::string n_gram_delimiter = " ", int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1,

                                         int threads = 1, bool verbose = false, std::string vocabulary_path = "", std::string concat_delimiter = "NULL", bool save_2single_file = false,

                                         std::string path_2file = "", std::string path_extend = "output_token.txt") {

      TOKEN t(VEC);

      if (verbose) { Rcpp::Rcout << "" << std::endl; }

      if (cpp_to_lower) {

        if (verbose) { Rcpp::Rcout << "conversion to lower case starts ..." << std::endl; }

        t.conv_to_lower(LOCALE_UTF);}

      if (cpp_to_upper) {

        if (verbose) { Rcpp::Rcout << "conversion to upper case starts ..." << std::endl; }

        t.conv_to_upper(LOCALE_UTF);}

      if (remove_char != "") {

        if (verbose) { Rcpp::Rcout << "the removal of specific characters starts ..." << std::endl; }

        t.remove_all(remove_char);}

      if (cpp_remove_punctuation) {

        if (verbose) { Rcpp::Rcout << "removal of punctuation in the character string starts ..." << std::endl; }

        t.remove_punctuation();}

      if (cpp_remove_numbers) {

        if (verbose) { Rcpp::Rcout << "removal of numeric values starts ..." << std::endl; }

        t.remove_numbers();}

      if (cpp_trim_token) {

        if (verbose) { Rcpp::Rcout << "the string-trim starts ..." << std::endl; }

        t.trim_token();}

      if (cpp_tokenization_function) {

        if (verbose) {

          if (!remove_punctuation_vector) {

            Rcpp::Rcout << "the split of the character string starts ..." << std::endl; }

          else {

            Rcpp::Rcout << "the split of the character string and simultaneously the removal of the punctuation in the vector starts ..." << std::endl;
          }
        }

        t.TOKENIZER(cpp_string_separator, remove_punctuation_vector, threads);
      }

      if (cpp_remove_stopwords) {

        if (verbose) { Rcpp::Rcout << "stop words of the " << language_spec << " language will be used" << std::endl; }

        t.read_stopwords(language);

        if (verbose) { Rcpp::Rcout << "the removal of stop-words starts ..." << std::endl; }

        t.remove_stopwords(threads);}

      if (min_num_char > 1 || max_num_char < 1000000000) {

        bool max_len_flag = max_num_char < 1000000000 ? true : false;

        if (verbose) {

          if (max_len_flag) {

            Rcpp::Rcout << "character strings with more than or equal to " << min_num_char << " and less than " << max_num_char << " characters will be kept ..." << std::endl; }

          else {

            Rcpp::Rcout << "character strings with more than or equal to  " << min_num_char << " and less than 1000000000 characters will be kept ..." << std::endl;
          }
        }

        t.keep_n_char(max_num_char, min_num_char, threads);}

      if (stemmer != "NULL") {

        if (stemmer == "porter2_stemmer") {

          if (verbose) { Rcpp::Rcout << stemmer << " starts ..." << std::endl; }

          t.porter2_stemmer(threads);}

        else if (stemmer == "ngram_sequential") {

          if (verbose) { Rcpp::Rcout << stemmer << " stemming starts ..." << std::endl; }

          t.NGRAM_SEQ(stemmer_ngram, stemmer_gamma, stemmer_truncate, stemmer_batches, threads, verbose);
        }

        else if (stemmer == "ngram_overlap") {

          if (verbose) { Rcpp::Rcout << stemmer << " stemming starts ..." << std::endl; }

          t.NGRAM_OVERLAP(stemmer_ngram, verbose);
        }

        else {

          Rcpp::stop("invalid stemmer type");
        }
      }

      if (min_n_gram > 1 || max_n_gram > 1) {

        if (verbose) { Rcpp::Rcout << "n-gram creation with min_n_gram : " << min_n_gram << " and max_n_gram : " << max_n_gram << " starts ..." << std::endl; }

        t.build_n_grams(min_n_gram, max_n_gram, n_gram_delimiter, threads);
      }

      if (skip_n_gram > 1) {

        if (verbose) { Rcpp::Rcout << "skip-n-gram creation with skip_n_gram : " << skip_n_gram << " and skip-distance : " << skip_distance << " starts ..." << std::endl; }

        t.skip_n_grams(skip_n_gram, skip_distance, n_gram_delimiter, threads);
      }

      if (vocabulary_path != "") {

        if (verbose) { Rcpp::Rcout << "the vocabulary counts will be saved in: " << vocabulary_path << std::endl; }

        t.vocab_counts_save(vocabulary_path);      // it doesn't exactly save but append to file, thus multiple runs of the same function without deleting the previous file will add the output at the end of the file
      }

      if (concat_delimiter != "NULL") {

        if (verbose) { Rcpp::Rcout << "the concatenation of the string vector starts ..." << std::endl; }

        t.concatenate(concat_delimiter);}

      if (save_2single_file) {

        if (verbose) { Rcpp::Rcout << "the pre-processed data will be saved in a single file in: " << path_2file << std::endl; }

        std::string tmp_concat = concat_delimiter == "NULL" ? " " : concat_delimiter;

        t.concatenate(tmp_concat);

        // if (concat_delimiter == "NULL") {
        //
        //   t.concatenate(" ");}
        //
        // else {
        //
        //   t.concatenate(concat_delimiter);
        // }

        t.append_2file(path_2file, tmp_concat);
      }

      else {

        if (path_2file != "") {

          if (verbose) { Rcpp::Rcout << "the pre-processed data will be saved in : " << path_2file << std::endl; }

          if (concat_delimiter == "NULL") {

            t.concatenate(" ");}

          else {

            t.concatenate(concat_delimiter);
          }

          t.save_2file(path_2file, path_extend);
        }
      }

      return t._object_vector();
    }



    // function which splits a text "FILE" into batches [ if the rows of the file are less than the batch-size then save those rows to a file, otherwise split the file to batches ]
    // the 'OUTPUT_PATH' should be a path-to-a-folder like "/home/myfolder/"
    //

    void batch_2file(std::string INPUT_FILE, std::string OUTPUT_PATH, int batches, char read_file_delimiter, std::vector<std::string> language, std::string language_spec, std::string LOCALE_UTF,

                     int max_num_char, std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false, bool cpp_remove_punctuation = false, bool remove_punctuation_vector = false,

                     bool cpp_remove_numbers = false, bool cpp_trim_token = false, bool cpp_tokenization_function = false, std::string cpp_string_separator = "-*", bool cpp_remove_stopwords = false,

                     int min_num_char = 1, std::string stemmer = "NULL", int min_n_gram = 1, int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0, std::string n_gram_delimiter = " ",

                     int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1, int threads = 1, std::string concat_delimiter = "\n",

                     bool verbose = false, std::string vocabulary_path = "") {


      long long nr_rows = count_rows(INPUT_FILE);          // meant for small to medium data sets ( counting rows for big data sets is infeasible, use the 'big_text_tokenizer' function instead )

      std::ifstream myfile(INPUT_FILE);

      if (batches > nr_rows) {

        if (verbose) { Rcpp::Rcout << "" << std::endl; }

        if (verbose) {Rcpp::Rcout << "the input-file has a single line, thus only a single output-file will be returned" << std::endl; }

        std::string line;

        std::vector<std::string> myLines;

        while (std::getline(myfile, line, read_file_delimiter)) {

          myLines.push_back(line);
        }

        std::vector<std::string> tmp_batch_vec = TOKEN_batch(myLines, language, language_spec, LOCALE_UTF, max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation,

                                                             remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords,

                                                             min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, stemmer_ngram,

                                                             stemmer_gamma, stemmer_truncate, stemmer_batches, threads, verbose, vocabulary_path, "NULL", false, "", "output_token.txt");

        std::string tmp_str = boost::algorithm::join(tmp_batch_vec, concat_delimiter);

        std::string tmp_num = OUTPUT_PATH + "batch.txt";

        save_string(tmp_str, tmp_num);}

      else {

        std::map<int, std::vector<long long>> calc_batches = batch_calculation(nr_rows, batches);

        std::string line;

        std::vector<std::string> myLines;

        long long count_ROWS = 0;

        int count_BATCHES = 0;

        while (std::getline(myfile, line, read_file_delimiter)) {

          myLines.push_back(line);

          std::vector<long long> tmp_batch = calc_batches[count_BATCHES];

          if (count_BATCHES == batches) { break; }

          if (count_ROWS == tmp_batch[1]) {

            if (verbose) { Rcpp::Rcout << "" << std::endl; }

            if (verbose) { Rcpp::Rcout << "==============================" << std::endl; }

            if (verbose) { Rcpp::Rcout << "batch " << count_BATCHES + 1 << " will be pre-processed" << std::endl; }

            if (verbose) { Rcpp::Rcout << "==============================" << std::endl; }

            std::vector<std::string> tmp_batch_vec = TOKEN_batch(myLines, language, language_spec, LOCALE_UTF,max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation,

                                                                 remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords,

                                                                 min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, stemmer_ngram,

                                                                 stemmer_gamma, stemmer_truncate, stemmer_batches, threads, verbose, vocabulary_path, "NULL", false, "", "output_token.txt");

            std::string tmp_str = boost::algorithm::join(tmp_batch_vec, concat_delimiter);

            std::string tmp_num = OUTPUT_PATH + "batch" + std::to_string(count_BATCHES + 1) + ".txt";

            save_string(tmp_str, tmp_num);

            line.clear();

            myLines.clear();

            count_BATCHES++;
          }

          count_ROWS++;
        }
      }

      myfile.close();
    }

    ~BATCH_TOKEN() { }
};


#endif

