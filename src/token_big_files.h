/**
 * Copyright (C) 2016 Lampros Mouselimis
 *
 * @file token_big_files.h
 *
 * @author Lampros Mouselimis
 *
 * @date October - December 2016
 *
 * @Notes: Big text file tokenization and transformation
 *
 * @last-modified: January 2017
 *
 **/



#ifndef __bigtokenfiles__
#define __bigtokenfiles__

//-------------------------
// include dependencies:

#include "tokenization.h"
#include "batch_tokenization.h"
#include "token_stats.h"
#include "sort_2dim_vecs.h"

//-------------------------


class big_files {

  private:

    TOKEN_stats tkst;

    BATCH_TOKEN btk;

    SORT_2DIMENSIONAL_VEC<std::string, long long> s2dv;

  public:

    big_files() { }

    // returns the total size of a file (in bytes)
    //

    long long MEM_splitter(std::string input_path) {

      std::ifstream in(input_path);

      in.seekg(0, std::ios::end);                   // it deletes the object (don't call it inside the function)

      long long total_byte_size = in.tellg();

      return total_byte_size;
    }



    // use a path to a file to calculate either KB, MB or GB,
    //

    double bytes_converter(std::string input_path_file, std::string unit = "GB") {

      long long tmp_spl = MEM_splitter(input_path_file);

      double calc = 0.0;

      if (unit == "KB") {

        calc = tmp_spl / 1024.0;}

      if (unit == "MB") {

        calc = tmp_spl / 1048576.0;}

      if (unit == "GB") {

        calc = tmp_spl / 1073741824.0;
      }

      return calc;
    }


    // save character string to a file
    //

    void SAVE_string(std::string x, std::string file = "output.txt") {

      btk.save_string(x, file);
    }


    // ordered-map to calculate the boundaries of the batches [ the remainder goes to the last batch ]
    //

    std::map<int, std::vector<long long>> Batch_calculation(long long nr_rows, int batches) {

      return btk.batch_calculation(nr_rows, batches);
    }


    // secondary function to convert calculation of batches in form of an ordered-map to a form of a numeric vector (for long long)
    //

    std::vector<long long> batch_num(long long nr_rows, int batches) {

      std::map<int, std::vector<long long>> tmp = Batch_calculation(nr_rows, batches);

      std::vector<long long> out(tmp.size());

      int count = 0;

      for (auto kv: tmp) {

        out[count] = kv.second[1];

        count++;
      }

      return out;
    }


    // splits the data in batches using the number of bytes of the file
    // 'trimmed_line = TRUE' means that each line in the text file does not include any white spaces (both sides - right or left). It is already trimmed
    // If 'trimmed_line' = FALSE then the text will be trimmed both sides. It isn't trimmed
    //

    void bytes_splitter(std::string input_path, int batches, std::string OUTPUT_PATH, std::string end_query = "NULL", bool trimmed_line = false, bool verbose = false) {

      arma::wall_clock timer;

      if (verbose) { timer.tic();

        Rcpp::Rcout << "" << std::endl;
      }

      double verbose_print = 6.0;

      std::string line;

      std::string myLines;

      long long copy_size = 0;

      std::ifstream in(input_path);

      long long total_byte_size = MEM_splitter(input_path);

      std::vector<long long> btch = batch_num(total_byte_size, batches);

      int count_BATCHES = 0;

      bool flag_ = false;

      while (getline(in,line)) {

        copy_size += line.size();

        if (!trimmed_line) {

          boost::trim(line);
        }

        myLines += line + "\n";

        if (verbose) {

          int tmp_mem = ((copy_size * 1.0) / total_byte_size) * 100.0;

          if (verbose_print <= tmp_mem) {

            Rcpp::Rcout << "approx. "<< tmp_mem + 2 << " % of data pre-processed" << std::endl;

            verbose_print += 10;
          }
        }

        if (count_BATCHES == batches) { break; }

        else if (count_BATCHES < batches) {

          if (btch[count_BATCHES] < copy_size) {

            std::string end;

            if (end_query != "NULL") {

              if (end_query.length() <= line.length()) {

                end = line.substr(line.length() - end_query.length(), line.length());
              }

              if (end == end_query) {

                std::string tmp_num = OUTPUT_PATH + "batch" + std::to_string(count_BATCHES + 1) + ".txt";

                SAVE_string(myLines, tmp_num);

                line.clear();

                myLines.clear();

                count_BATCHES++;
              }
            }

            else {

              std::string tmp_num = OUTPUT_PATH + "batch" + std::to_string(count_BATCHES + 1) + ".txt";

              SAVE_string(myLines, tmp_num);

              line.clear();

              myLines.clear();

              count_BATCHES++;
            }
          }
        }

        flag_ = true;
      }

      if (flag_) {

        std::string tmp_num = OUTPUT_PATH + "batch" + std::to_string(count_BATCHES + 1) + ".txt";

        SAVE_string(myLines, tmp_num);

        line.clear();

        myLines.clear();
      }

      // time for pre-processing

      if (verbose) {

        Rcpp::Rcout << "" << std::endl;

        double n = timer.toc();

        Rcpp::Rcout << "It took " << n / 60.0 << " minutes to complete the splitting" << std::endl;
      }

      in.close();
    }


    // this function will be called from the wrapper_batches_parser
    //

    void batch_parser(std::string input_path_file, std::string start_query, std::string end_query, std::string output_path_file = "",

                      int min_lines = 1, bool trimmed_line = false, bool verbose = false) {

      arma::wall_clock timer;

      if (verbose) { timer.tic(); }

      long long total_byte_size = MEM_splitter(input_path_file);

      std::string END_str;

      std::string line;

      std::vector<std::string> myLines;

      std::ifstream in(input_path_file);

      double verbose_print = 6.0;

      bool flag_write = false;

      long long track_GB = 0;

      if (verbose) { Rcpp::Rcout << "" << std::endl; }

      while (getline(in,line)) {

        track_GB += line.size();

        if (!trimmed_line) {

          boost::trim(line);
        }

        if (start_query.length() <= line.length()) {

          std::string begin_str = line.substr(0, start_query.length());

          if (begin_str == start_query) {

            flag_write = true;

            line = line.substr(start_query.length(), line.length());                         // exclude the 'start_query' string from the line
          }
        }

        std::string end_str;

        if (end_query.length() <= line.length()) {

          end_str = line.substr(line.length() - end_query.length(), line.length());          // exclude the 'end_query' string from the line

          if (end_str == end_query) {

            line = line.substr(0, line.length() - end_query.length());
          }
        }

        if (flag_write) {

          myLines.push_back(line);
        }

        if (end_str == end_query) {

          flag_write = false;
        }

        if (verbose) {

          int tmp_mem = ((track_GB * 1.0) / total_byte_size) * 100.0;

          if (verbose_print <= tmp_mem) {

            Rcpp::Rcout << "approx. "<< tmp_mem + 2 << " % of data pre-processed" << std::endl;

            verbose_print += 10;
          }
        }

        if (!flag_write && !myLines.empty()) {

          long long myLines_size = myLines.size();

          if (myLines_size >= min_lines) {

            std::string tmp_str = boost::algorithm::join(myLines, "\n");

            END_str += tmp_str + "\n";
          }

          line.clear();

          myLines.clear();
        }
      }

      // time for pre-processing

      if (verbose) {

        Rcpp::Rcout << "" << std::endl;

        double n = timer.toc();

        Rcpp::Rcout << "It took " << n / 60.0 << " minutes to complete the preprocessing" << std::endl;
      }

      // time to save the output data

      arma::wall_clock timer1;

      if (verbose) { timer1.tic(); }

      std::string tmp_nam;

      if (output_path_file == "") {

        tmp_nam = "output_batch_parser.txt";}

      else {

        tmp_nam = output_path_file;
      }

      SAVE_string(END_str, tmp_nam);

      if (verbose) {

        Rcpp::Rcout << "" << std::endl;

        double n1 = timer1.toc();

        Rcpp::Rcout << "It took " << n1 / 60.0 << " minutes to save the pre-processed data" << std::endl;
      }

      in.close();

      END_str.shrink_to_fit();
    }


    // the wrapper_batches_parser function takes the xml-files from an input folder and returns only the text [ using the queries ] to the output-folder
    //

    void wrapper_batches_parser(std::string input_path_folder, std::string start_query, std::string end_query, std::string output_path_folder,

                                int min_lines = 1, bool trimmed_line = false, bool verbose = false) {

      arma::wall_clock timer;

      if (verbose) { timer.tic(); }

      std::vector<std::string> all_files = tkst.list_files(input_path_folder, true);

      for (unsigned int i = 0; i < all_files.size(); i++) {

        std::string tmp_nam = output_path_folder + "batch" + std::to_string(i + 1) + ".txt";

        if (verbose) {

          Rcpp::Rcout << "" << std::endl;

          Rcpp::Rcout << "====================" << std::endl;
        }

        if (verbose) { Rcpp::Rcout << "batch " << i + 1 << " begins ..." << std::endl; }

        if (verbose) { Rcpp::Rcout << "====================" << std::endl; }

        batch_parser(all_files[i], start_query, end_query, tmp_nam, min_lines, trimmed_line, verbose);
      }

      if (verbose) {

        Rcpp::Rcout << "" << std::endl;

        double n = timer.toc();

        Rcpp::Rcout << "It took " << n / 60.0 << " minutes to complete the parsing" << std::endl;
      }
    }


    // the whole token-transformation-process from the class TOKEN
    //

    std::vector<std::string> res_TOKEN(std::string x, std::vector<std::string> language, std::string language_spec, std::string LOCALE_UTF, bool FLAG_path, char read_file_delimiter, long long max_num_char,

                                       std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false, bool cpp_remove_punctuation = false,

                                       bool remove_punctuation_vector = false, bool cpp_remove_numbers = false, bool cpp_trim_token = false, bool cpp_tokenization_function = false,

                                       std::string cpp_string_separator = "-*", bool cpp_remove_stopwords = false, int min_num_char = 1, std::string stemmer = "NULL", int min_n_gram = 1,

                                       int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0, std::string n_gram_delimiter = " ", std::string concat_delimiter = "NULL",

                                       std::string path_2file = "", int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1, int threads = 1,

                                       bool verbose = false, bool save_2single_file = false, std::string path_extend = "output_token.txt", std::string vocabulary_path = "", bool tokenize_vector = false) {
      TOKEN t(x);

      if (verbose) { Rcpp::Rcout << "" << std::endl; }

      if (verbose) { Rcpp::Rcout << "input of the data starts ..." << std::endl; }

      t.read_file(read_file_delimiter, FLAG_path);

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

        t.append_2file(path_2file, tmp_concat, tokenize_vector);
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



    // parameter definitions for the fscanf() function : https://www.tutorialspoint.com/c_standard_library/c_function_fscanf.htm
    // batch-vocabulary-counts: loads the batch vocabulary files from a folder and stores a file with the merged-sorted vocabulary using a maximum number of characters for words as a parameter
    // vocabulary only for single word with it's count, for instance: 'word' 1 (a word followed by a count separated by space -- two or more words lead to error)

    void vocabulary_counts_folder(std::string input_path_folder, std::string output_path_file, int max_num_chars = 1000, bool verbose = false) {

      arma::wall_clock timer;

      if (verbose) { timer.tic(); }

      std::vector<std::string> all_files = tkst.list_files(input_path_folder, true);

      std::unordered_map<std::string, long long> myLines;

      Rprintf("\n");

      for (unsigned int i = 0; i < all_files.size(); i++) {

        if (verbose) {

          Rprintf("\rvocabulary.of.batch %d will.be.merged ...", i + 1); }

        const char* tmp_input_file = all_files[i].c_str();

        FILE* f = std::fopen(tmp_input_file, "r");

        int items_read;

        std::string postfix = "s %d\n";

        std::string str = "%" + std::to_string(max_num_chars) + postfix;

        const char* c_postfix = str.c_str();

        char *word = new char[max_num_chars + 1];

        int number;

        while(true) {

          items_read = fscanf(f, c_postfix, word, &number);

          if( items_read < 1 ) break;                               // break at the end of the file

          if (items_read == 1) {

            continue;                                               // exclude empty words from file
          }

          else {

            myLines[word] += number;
          }
        }

        delete[] word;

        fclose (f);
      }

      std::vector<STRUCT<std::string, long long>> myLines_out = s2dv.inner_sort_func_MAP(myLines, false, false);

      std::unordered_map<std::string, long long>().swap(myLines);

      std::ofstream ofs(output_path_file);

      if (verbose) {

        double n = timer.toc();

        Rprintf("\tminutes.to.merge.sort.batches: %.5f", n / 60.0);
      }

      arma::wall_clock timer1;

      if (verbose) { timer1.tic(); }

      for (auto& it : myLines_out) {

        ofs << it.VAR1 << "\t" << it.VAR2 << "\n";
      }

      myLines_out.shrink_to_fit();

      ofs.close();

      if (verbose) {

        Rcpp::Rcout << "" << std::endl;

        double n1 = timer1.toc();

        Rprintf("\tminutes.to.save.data: %.5f", n1 / 60.0);
      }
    }


    // vocabulary-count-parser  [ for small to medium data sets ]
    //

    void vocabulary_count_parser(std::string input_path_file, std::string start_query, std::string end_query,  std::vector<std::string> language, std::string output_path_file = "",

                                 int min_lines = 1, bool trimmed_line = false, bool query_transform = false, std::string language_spec = "english", std::string LOCALE_UTF = "",

                                 long long max_num_char = 1000000000, std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false,

                                 bool cpp_remove_punctuation = false, bool remove_punctuation_vector = false, bool cpp_remove_numbers = false, bool cpp_trim_token = false,

                                 bool cpp_tokenization_function = false, std::string cpp_string_separator = " \r\n\t.,;:()?!//", bool cpp_remove_stopwords = false,

                                 int min_num_char = 1, std::string stemmer = "NULL", int min_n_gram = 1, int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0,

                                 std::string n_gram_delimiter = " ", int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1,

                                 int threads = 1, bool verbose = false) {

      arma::wall_clock timer;

      if (verbose) { timer.tic(); }

      long long total_byte_size = MEM_splitter(input_path_file);

      std::string line;

      std::unordered_map<std::string, long long> myLines;

      std::ifstream in(input_path_file);

      double verbose_print = 5.0;

      bool flag_write = false;

      long long track_GB = 0;

      long long Lines = 0;

      Rprintf("\n");

      bool flag_peek = false;

      while (getline(in,line)) {

        if (in.peek() == std::ifstream::traits_type::eof()) { flag_peek = true; }

        track_GB += line.size();

        Lines++;

        if (!trimmed_line) {

          boost::trim(line);
        }

        if (start_query.length() <= line.length()) {

          std::string begin_str = line.substr(0, start_query.length());

          if (begin_str == start_query) {

            flag_write = true;

            line = line.substr(start_query.length(), line.length());                         // exclude the 'start_query' string from the line
          }
        }

        std::string end_str;

        if (end_query.length() <= line.length()) {

          end_str = line.substr(line.length() - end_query.length(), line.length());          // exclude the 'end_query' string from the line

          if (end_str == end_query) {

            line = line.substr(0, line.length() - end_query.length());
          }
        }

        if (flag_write) {

          if (query_transform) {

            std::vector<std::string> tmp_vec = res_TOKEN(line, language, language_spec, LOCALE_UTF, false, '\t', max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation,

                                               remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords,

                                               min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, "NULL", "", stemmer_ngram,

                                               stemmer_gamma, stemmer_truncate, stemmer_batches, threads, false, false, "output_token.txt", "");

            for (unsigned int k = 0; k < tmp_vec.size(); k++) {

              myLines[tmp_vec[k]] += 1;
            }
          }

          else {

            myLines[line] += 1;
          }
        }

        if (end_str == end_query) {

          flag_write = false;
        }

        if (verbose) {

          double tmp_mem = ((track_GB * 1.0) / total_byte_size) * 100.0;

          if (flag_peek) { tmp_mem = 100.0; }

          if (verbose_print <= tmp_mem || flag_peek) {

            Rprintf("\rtotal.number.lines.processed: %3d", Lines);

            Rprintf("\tdata.processed.approx.: %.1f %%", tmp_mem);

            verbose_print += 5;
          }
        }
      }

      // sort the counts

      std::vector<STRUCT<std::string, long long>> myLines_out = s2dv.inner_sort_func_MAP(myLines, false, false);

      // time for pre-processing

      if (verbose) {

        double n = timer.toc();

        Rprintf("\t\tminutes.to.process.data: %.5f", n / 60.0);
      }

      // time to save the output data

      arma::wall_clock timer1;

      if (verbose) { timer1.tic(); }

      std::string tmp_nam;

      if (output_path_file == "") {

        tmp_nam = "output_batch_parser.txt";}

      else {

        tmp_nam = output_path_file;
      }

      std::ofstream ofs(output_path_file);

      for (auto& it : myLines_out) {

        ofs << it.VAR1 << "\t" << it.VAR2 << "\n";
      }

      if (verbose) {

        Rcpp::Rcout << "" << std::endl;

        double n1 = timer1.toc();

        Rprintf("\tminutes.to.save.data: %.5f", n1 / 60.0);
      }

      in.close();

      ofs.close();
    }



    // batch token-transformation using the previous res_TOKEN function
    //

    void batch_tokenizer_bytes(std::string input_path, std::string output_path_folder, int batches, int increment_batch_no, std::vector<std::string> language, std::string language_spec,

                               std::string LOCALE_UTF, char read_file_delimiter, int max_num_char, std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false,

                               bool cpp_remove_punctuation = false, bool remove_punctuation_vector = false, bool cpp_remove_numbers = false, bool cpp_trim_token = false,

                               bool cpp_tokenization_function = false, std::string cpp_string_separator = "-*", bool cpp_remove_stopwords = false, int min_num_char = 1,

                               std::string stemmer = "NULL", int min_n_gram = 1, int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0, std::string n_gram_delimiter = " ",

                               std::string concat_delimiter = "NULL", int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1,

                               int threads = 1, bool save_2single_file = false, bool verbose = false, std::string vocabulary_folder = "") {

      std::string line;

      std::string myLines;

      int tmp_increment = increment_batch_no;

      long long copy_size = 0;

      std::ifstream in(input_path);

      long long total_byte_size = MEM_splitter(input_path);

      std::vector<long long> btch = batch_num(total_byte_size, batches);

      int count_BATCHES = 0;

      bool flag_ = false;

      while (getline(in,line)) {

        myLines += line + "\n";

        copy_size += line.size();

        if (count_BATCHES == batches) { break; }

        else if (count_BATCHES < batches) {

          if (btch[count_BATCHES] < copy_size) {

            if (verbose) {

              Rcpp::Rcout << "" << std::endl;

              Rcpp::Rcout << "-------------------" << std::endl;

              Rcpp::Rcout << "batch " << count_BATCHES + 1 << " begins ..." << std::endl;

              Rcpp::Rcout << "-------------------" << std::endl;
            }

            std::string path_extend = "batch" + std::to_string(tmp_increment) + ".txt";

            std::vector<std::string> tmp_batch_vec = res_TOKEN(myLines, language, language_spec, LOCALE_UTF, false, read_file_delimiter, max_num_char, remove_char, cpp_to_lower, cpp_to_upper,

                                                               cpp_remove_punctuation, remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator,

                                                               cpp_remove_stopwords, min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, concat_delimiter,

                                                               output_path_folder, stemmer_ngram, stemmer_gamma, stemmer_truncate, stemmer_batches, threads, verbose, save_2single_file, path_extend,

                                                               vocabulary_folder);
            line.clear();

            myLines.clear();

            count_BATCHES++;

            tmp_increment++;
          }
        }

        flag_ = true;
      }

      if (flag_) {

        if (verbose) {

          Rcpp::Rcout << "" << std::endl;

          Rcpp::Rcout << "-------------------" << std::endl;

          Rcpp::Rcout << "batch " << count_BATCHES + 1 << " begins ..." << std::endl;

          Rcpp::Rcout << "-------------------" << std::endl;
        }

        std::string path_extend = "batch" + std::to_string(tmp_increment) + ".txt";

        std::vector<std::string> tmp_batch_vec = res_TOKEN(myLines, language, language_spec, LOCALE_UTF, false, read_file_delimiter, max_num_char, remove_char, cpp_to_lower, cpp_to_upper, cpp_remove_punctuation,

                                                           remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator, cpp_remove_stopwords,

                                                           min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, concat_delimiter, output_path_folder,

                                                           stemmer_ngram, stemmer_gamma, stemmer_truncate, stemmer_batches, threads, verbose, save_2single_file, path_extend, vocabulary_folder);
        line.clear();

        myLines.clear();
      }

      in.close();
    }


    // wrapper for the batch_tokenizer_bytes function
    //

    void wrapper_batch_tokenizer_bytes(std::string input_path_folder, std::string output_path_folder, int batches, int increment_batch_no, std::vector<std::string> language, std::string language_spec,

                                       std::string LOCALE_UTF, char read_file_delimiter, int max_num_char, std::string remove_char = "", bool cpp_to_lower = false, bool cpp_to_upper = false,

                                       bool cpp_remove_punctuation = false, bool remove_punctuation_vector = false, bool cpp_remove_numbers = false, bool cpp_trim_token = false,

                                       bool cpp_tokenization_function = false, std::string cpp_string_separator = "-*", bool cpp_remove_stopwords = false, int min_num_char = 1,

                                       std::string stemmer = "NULL", int min_n_gram = 1, int max_n_gram = 1, int skip_n_gram = 1, int skip_distance = 0, std::string n_gram_delimiter = " ",

                                       std::string concat_delimiter = "NULL", int stemmer_ngram = 4, double stemmer_gamma = 0.0, int stemmer_truncate = 3, int stemmer_batches = 1, int threads = 1,

                                       bool save_2single_file = false, std::string vocabulary_folder = "", bool verbose = false) {

      arma::wall_clock timer;

      if (verbose) { timer.tic(); }

      int new_increment_no = increment_batch_no;

      std::vector<std::string> all_files = tkst.list_files(input_path_folder, true);

      for (unsigned int i = 0; i < all_files.size(); i++) {

        if (verbose) {

          Rcpp::Rcout << "" << std::endl;

          Rcpp::Rcout << "====================================" << std::endl;

          Rcpp::Rcout << "transformation of file " << i + 1 << " starts ..." << std::endl;

          Rcpp::Rcout << "====================================" << std::endl;

        }

        std::string tmp_path_vocab;

        if (vocabulary_folder != "") {

          tmp_path_vocab = vocabulary_folder + "batch" + std::to_string(i + 1) + ".txt";}

        else {

          tmp_path_vocab = "";
        }

        batch_tokenizer_bytes(all_files[i], output_path_folder, batches, new_increment_no, language, language_spec, LOCALE_UTF, read_file_delimiter, max_num_char, remove_char, cpp_to_lower,

                              cpp_to_upper, cpp_remove_punctuation, remove_punctuation_vector, cpp_remove_numbers, cpp_trim_token, cpp_tokenization_function, cpp_string_separator,

                              cpp_remove_stopwords, min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, concat_delimiter, stemmer_ngram,

                              stemmer_gamma, stemmer_truncate, stemmer_batches, threads, save_2single_file, verbose, tmp_path_vocab);

        new_increment_no += batches;
      }

      if (verbose) {

        Rcpp::Rcout << "" << std::endl;

        double n = timer.toc();

        Rcpp::Rcout << "It took " << n / 60.0 << " minutes to complete tokenization" << std::endl;
      }
    }

    ~big_files() { }
};


#endif

