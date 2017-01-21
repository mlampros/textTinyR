
/**
 * Copyright (C) 2016 Lampros Mouselimis
 *
 * @file tokenization.h
 *
 * @author Lampros Mouselimis
 *
 * @date October - December 2016
 *
 * @Notes: the main class for tokenization and transformation of text files
 *
 * @last_modified: December 2016
 *
 **/



#ifndef __tokenization__
#define __tokenization__

//-------------------------
// include dependencies:

#include "ngram_stemmer.h"

//-------------------------



// class to modify strings, string-vectors
//


class TOKEN {

  private:

    std::string x;

    std::vector<std::string> v;

    std::vector<std::string> stop_words;

    ngram_stemmer nst;

  public:

    TOKEN(std::string &string_) : x(string_) { }                   // in most cases input is a string

    TOKEN(std::vector<std::string> &vector_) :  v(vector_) { }     // in batch_tokenization is a vector


    // read either a file or a character string into c++
    //

    void read_file(char read_file_delimiter = '\t', bool FLAG_path = true) {

      if (FLAG_path) {

        std::ifstream myfile(x);

        std::string line;

        std::vector<std::string> tmp_v;

        while (std::getline(myfile, line, read_file_delimiter)) {        // file split in chunks  using the tab-delimiter

          tmp_v.push_back(line);
        }

        v = tmp_v;

        tmp_v.shrink_to_fit();
      }

      else {

        v.resize(1);

        v[0] = x;

        x.shrink_to_fit();
      }
    }


    // secondary function in case of conversion to lower or upper case if language is not english
    //

    #ifndef _WIN32
    std::string LOCALE_FUNCTION(std::string x, bool TO_lower = false, std::string LOCALE_UTF = "") {

      boost::locale::generator gen;

      std::locale loc = gen(LOCALE_UTF);

      std::locale::global(loc);                         // Create system default locale

      //std::cout.imbue(loc);                           // gives an error when I do cran-checking due to std::cout

      if (TO_lower) {

        return boost::locale::to_lower(x);}             // Make it system global

      else {

        return boost::locale::to_upper(x);
      }
    }
    #endif
    
    
    // work-around for the boost-locale header in windows [ gcc (v. 4.6.3) in RTools (v. 3.3) is probably not built with locale support ]
    // see http://stackoverflow.com/questions/31670839/how-do-i-read-a-windows-1252-file-using-rcpp
    //
    
    std::string BASE_STRING_win(std::string y, bool TO_lower = false) {
      
      SEXP x = Rcpp::wrap(y);
      
      y.shrink_to_fit();
      
      Rcpp::Environment base_env("package:base");
      
      if (TO_lower) {
        
        Rcpp::Function conv_str_lower = base_env["tolower"];
        
        return Rcpp::as<std::string>(conv_str_lower(x));}
      
      else {
        
        Rcpp::Function conv_str_upper = base_env["toupper"];
        
        return Rcpp::as<std::string>(conv_str_upper(x));
      }
    }

    
    // exceptions for specific languages ( greek, russian, bulgarian, armenian ) due to error in Macintosh OSx [ use the windows function ]
    
    bool is_language(std::string tmp_locale) {
      
      std::vector<std::string> trg_vec = { "el_GR", "ru_RU", "bg_BG", "Hy-AM" };
      
      std::vector<std::string> tmp_vec;
      
      boost::split( tmp_vec, tmp_locale, boost::is_any_of("."), boost::token_compress_on );
      
      std::string current_lang = tmp_vec[0];
      
      bool flg_out = false;
      
      for (unsigned int i = 0; i < trg_vec.size(); i++) {
        
        if (current_lang == trg_vec[i]) {
          
          flg_out = true;
        }
      }
      
      return flg_out;
    }
    
    
    // secondary function for the 'conv_to_lower' and 'conv_to_upper' functions
    // it's possible that the locale for the english language isn't only UTF-8
    
    std::string sec_func_conv(std::string tmp_str) {
      
      if (tmp_str == "") {
        
        return "";}
      
      else {
        
        std::vector<std::string> tmp_vec;
        
        boost::split( tmp_vec, tmp_str, boost::is_any_of("."), boost::token_compress_on );
        
        return tmp_vec[0];
      }
    }
    
    
    // convert to lower case (special case : LOCALE)
    
    void conv_to_lower(std::string LOCALE_UTF = "") {
      
      for (unsigned int i = 0; i < v.size(); i++) {
        
        std::string tmp_v = v[i];
        
        if (LOCALE_UTF == "" || sec_func_conv(LOCALE_UTF) == "en") {
          
          std::transform(tmp_v.begin(), tmp_v.end(), tmp_v.begin(), ::tolower);}
        
        else {
          
          #ifndef _WIN32
          
            #ifdef __APPLE__ 
          
              if (LOCALE_UTF != "" && is_language(LOCALE_UTF)) {        
            
                tmp_v = BASE_STRING_win(tmp_v, true);}               // exception for specific languages
          
              else {
            
                tmp_v = LOCALE_FUNCTION(tmp_v, true, LOCALE_UTF);
              }
          
            #else
          
              tmp_v = LOCALE_FUNCTION(tmp_v, true, LOCALE_UTF);
          
            #endif
          
          #else
          
            tmp_v = BASE_STRING_win(tmp_v, true);
          
          #endif
        }
        
        v[i] = tmp_v;
        
        tmp_v.shrink_to_fit();
      }
    }
    
    
    // open language-specific-file of stop-words
    
    void read_stopwords(std::vector<std::string> language) {
      
      stop_words = language;
    }
    
    
    // removes all occurences of the specified 'any_character' in the string
    
    void remove_all(std::string any_character = "123<>?.") {
      
      for (unsigned int i = 0; i < v.size(); i++) {
        
        v[i] = boost::remove_erase_if(v[i], boost::is_any_of(any_character));
      }
    }
    
    
    // convert to upper case  (special case : LOCALE)
    
    void conv_to_upper(std::string LOCALE_UTF = "") {
      
      for (unsigned int i = 0; i < v.size(); i++) {
        
        std::string tmp_v = v[i];
        
        if (LOCALE_UTF == "" || sec_func_conv(LOCALE_UTF) == "en") {
          
          std::transform(tmp_v.begin(), tmp_v.end(), tmp_v.begin(), ::toupper);}
        
        else {
          
          #ifndef _WIN32
          
            #ifdef __APPLE__ 
          
              if (LOCALE_UTF != "" && is_language(LOCALE_UTF)) {        
            
                tmp_v = BASE_STRING_win(tmp_v, false);}               // exception for specific languages
          
              else {
            
                tmp_v = LOCALE_FUNCTION(tmp_v, false, LOCALE_UTF);
              }
          
            #else
          
              tmp_v = LOCALE_FUNCTION(tmp_v, false, LOCALE_UTF);
          
            #endif
          
          #else
          
            tmp_v = BASE_STRING_win(tmp_v, false);
          
          #endif
        }
        
        v[i] = tmp_v;
        
        tmp_v.shrink_to_fit();
      }
    }
    
    
    // remove punctuation       [ removes all special characters ]

    void remove_punctuation() {

      for (unsigned int i = 0; i < v.size(); i++) {

        std::string tmp_v = v[i];

        tmp_v.erase(std::remove_if(tmp_v.begin(), tmp_v.end(), &ispunct), tmp_v.end());

        v[i] = tmp_v;

        tmp_v.shrink_to_fit();
      }
    }


    // remove numbers

    void remove_numbers() {

      for (unsigned int i = 0; i < v.size(); i++) {

        std::string tmp_v = v[i];

        tmp_v.erase(std::remove_if(tmp_v.begin(), tmp_v.end(), &isdigit), tmp_v.end());

        v[i] = tmp_v;

        tmp_v.shrink_to_fit();
      }
    }


    // trim token

    void trim_token() {

      for (unsigned int i = 0; i < v.size(); i++) {

        std::string tmp_v = v[i];

        boost::trim(tmp_v);

        v[i] = tmp_v;

        tmp_v.shrink_to_fit();
      }
    }


    // split a string sentence using multiple separators
    //

    void TOKENIZER(std::string separator = "-*", bool remove_punctuation = false, int threads = 1) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      std::vector<std::string> new_vec;

      for (unsigned int i = 0; i < v.size(); i++) {

        std::string tmp_x = v[i];

        std::vector<std::string> tmp_vec;

        boost::split( tmp_vec, tmp_x, boost::is_any_of(separator), boost::token_compress_on );

        tmp_x.shrink_to_fit();

        if (remove_punctuation) {

          #ifdef _OPENMP
          #pragma omp parallel for schedule(static)
          #endif
          for (unsigned int i = 0; i < tmp_vec.size(); i++) {

            tmp_vec[i].erase(std::remove_if(tmp_vec[i].begin(), tmp_vec[i].end(), &ispunct), tmp_vec[i].end());
          }
        }

        new_vec.insert(std::end(new_vec), std::begin(tmp_vec), std::end(tmp_vec));

        tmp_vec.shrink_to_fit();
      }

      v.shrink_to_fit();

      v = new_vec;

      new_vec.shrink_to_fit();
    }


    // remove stopwords

    void remove_stopwords(int threads) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      std::unordered_map<std::string, std::vector<int>> counts;

      for (unsigned int i = 0; i < v.size(); i++) {

        counts[v[i]].push_back(i);
      }

      for (unsigned int j = 0; j < stop_words.size(); j++) {

        std::vector<int> tmp_word = counts[stop_words[j]];

        if (!tmp_word.empty()) {

          counts.erase (stop_words[j]);
        }
      }

      std::vector<std::string> vec_words;
      vec_words.reserve(counts.size());                                 // improves slightly the efficiency

      for(auto imap: counts) {

        vec_words.push_back(imap.first);                                // retrieve the keys from an unordered map
      }

      std::vector<int> insert_vals;                                     // flatten map-values and build an indices-vector

      for (unsigned int k = 0; k < vec_words.size(); k++) {

        std::vector<int> tmp_vec = counts[vec_words[k]];

        insert_vals.insert(std::end(insert_vals), std::begin(tmp_vec), std::end(tmp_vec));
      }

      std::unordered_map<std::string, std::vector<int>>().swap(counts); // release memory of the 'counts' object  [ http://stackoverflow.com/questions/10464992/c-delete-vector-objects-free-memory ]

      vec_words.shrink_to_fit();                                        // http://en.cppreference.com/w/cpp/container/vector/shrink_to_fit

      std::sort(insert_vals.begin(), insert_vals.end());                // sort the indices vector

      std::vector<std::string> result(insert_vals.size());              // subset [ to preserve the words order using indexing ]

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static)
      #endif
      for (unsigned int f = 0; f < insert_vals.size(); f++) {

        result[f] = v[insert_vals[f]];
      }

      insert_vals.shrink_to_fit();

      v = result;

      result.shrink_to_fit();
    }


    // keep strings within a specific range of characters
    // [ If the 'v' vector includes a single string --meaning I return without applying the tokenization function-- then in case
    //   that I use the 'keep_n_char' function it's possible that the output result will be an empty vector ]

    void keep_n_char(long long max_length, int min_length = 2, int threads = 1) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      std::unordered_map<int, std::vector<int>> counts;

      for (unsigned int i = 0; i < v.size(); i++) {

        int tmp_size = v[i].size();

        if (tmp_size >= min_length && tmp_size < max_length) {

          counts[tmp_size].push_back(i);
        }
      }

      std::vector<int> insert_vals;                                     // flatten map-values and build an indices vector

      for (unsigned int k = 0; k < counts.size(); k++) {

        std::vector<int> tmp_vec = counts[k];

        insert_vals.insert(std::end(insert_vals), std::begin(tmp_vec), std::end(tmp_vec));
      }

      std::unordered_map<int, std::vector<int>>().swap(counts);

      std::sort(insert_vals.begin(), insert_vals.end());

      std::vector<std::string> result(insert_vals.size());

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static)
      #endif
      for (unsigned int f = 0; f < insert_vals.size(); f++) {

        result[f] = v[insert_vals[f]];
      }

      insert_vals.shrink_to_fit();

      v = result;

      result.shrink_to_fit();
    }


    // porter-2-stemmer
    // https://github.com/smassung/porter2_stemmer  [ see the COPYRIGHTS file ]
    //

    /**
    * @file porter2_stemmer.h
    * @author Sean Massung
    * @date September 2012
    *
    * Implementation of
    * http://snowball.tartarus.org/algorithms/english/stemmer.html
    *
    * Copyright (C) 2012 Sean Massung**/


    void porter2_stemmer(int threads = 1) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static)
      #endif
      for (unsigned int i = 0; i < v.size(); i++) {

        v[i] = Porter2Stemmer::stem(v[i]);
      }
    }


    // sequential n-gram-stemmer
    //

    void NGRAM_SEQ(int min_n_gram = 4, double gamma = 0.0, int round_dec_places = 3, int batches = 1, int threads = 1, bool verbose = false) {

      v = nst.frequency_seq_ngram(v, min_n_gram, gamma, round_dec_places, batches, threads, verbose);
    }


    // overlapping n-gram-stemmer
    //

    void NGRAM_OVERLAP(int n_grams, bool verbose = false) {

      v = nst.n_gram_stemming_frequency(v, n_grams, verbose);
    }


    // secondary function for the n-grams-function

    std::vector<std::string> secondary_n_grams(std::vector<std::string> vec, int n_gram = 2, std::string n_gram_delimiter = "_", int threads = 1) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      int vec_size = vec.size() - n_gram + 1;

      std::vector<std::string> out(vec_size);

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static)
      #endif
      for (int i = 0; i < vec_size; i++) {

        std::string tmp_string;

        for (int j = i; j < i + n_gram; j++) {

          if (j == i) {

            tmp_string += vec[j];}

          else {

            tmp_string += n_gram_delimiter + vec[j];
          }
        }

        out[i] = tmp_string;
      }

      return out;
    }


    // build n-grams using an std::vector

    void build_n_grams(int min_n_gram = 2, int max_n_gram = 2,std::string n_gram_delimiter = "_", int threads = 1) {

      std::vector<std::string> insert_n_grams;

      for (int i = min_n_gram; i < max_n_gram + 1; i++) {

        std::vector<std::string> tmp_vec = secondary_n_grams(v, i, n_gram_delimiter, threads);

        insert_n_grams.insert(std::end(insert_n_grams), std::begin(tmp_vec), std::end(tmp_vec));
      }

      v = insert_n_grams;

      insert_n_grams.shrink_to_fit();
    }


    // build a sequence-vector specifying the start-point, the length
    // of the vector and the distance between numbers (similar to the seq() in R but with different parameters)

    arma::rowvec SEQ(int start, int length, int by) {

      arma::rowvec sqz(length);

      int count = 0;

      while(true) {

        if (count == 0) {

          sqz[count] = start;}

        else {

          start += by;

          sqz[count] = start;
        }

        count++;

        if (length < count) {

          break;
        }
      }

      return sqz;
    }


    // creates skip-n-grams

    std::vector<std::string> secondary_skip_n_grams(std::vector<std::string> v, int n_gram, int skip, std::string n_gram_delimiter = "_") {

      std::vector<std::string> out_skip_n_gram;

      for (unsigned int i = 0; i < v.size(); i++) {

        arma::rowvec sqz = SEQ(i, n_gram, skip + 1);

        bool flag_break = false;

        std::string string1;

        for (unsigned int j = 0; j < sqz.n_elem; j++) {

          if (sqz(j) < v.size()) {

            if (j == 0) {

              string1 += v[sqz(j)];}

            else {

              string1 += n_gram_delimiter + v[sqz(j)];
            }
          }

          else {

            flag_break = true;

            break;
          }
        }

        if (!flag_break) {

          out_skip_n_gram.resize(i + 1);

          out_skip_n_gram[i] = string1;}

        else {

          break;
        }
      }

      return out_skip_n_gram;
    }


    // creates skip-n-grams: if skip = 0 it creates simple n-grams otherwise it adds iterative n-grams to a
    // vector using the 'skip-parameter' as a distance between the words

    void skip_n_grams(int n_gram, int skip, std::string n_gram_delimiter = "_", int threads = 1) {

      std::vector<std::string> insert_n_grams;

      for (int i = 0; i < skip + 1; i++) {

        if (i == 0) {

          std::vector<std::string> tmp_n_grams = secondary_n_grams(v, n_gram, n_gram_delimiter, threads);

          insert_n_grams.insert(std::end(insert_n_grams), std::begin(tmp_n_grams), std::end(tmp_n_grams));}

        else {

          std::vector<std::string> tmp_skip_grams = secondary_skip_n_grams(v, n_gram, i, n_gram_delimiter);

          insert_n_grams.insert(std::end(insert_n_grams), std::begin(tmp_skip_grams), std::end(tmp_skip_grams));
        }
      }

      v = insert_n_grams;

      insert_n_grams.shrink_to_fit();
    }


    // saves vocabulary counts in a single file
    //

    void vocab_counts_save(std::string output_path_file = "vocab_file.txt") {

      std::unordered_map<std::string, long long> myLines;

      for (unsigned int k = 0; k < v.size(); k++) {

        myLines[v[k]] += 1;
      }

      std::ofstream ofs;

      ofs.open(output_path_file, std::ios::app);

      for (auto& it : myLines) {

        ofs << it.first << "\t" << it.second << "\n";
      }

      ofs.close();
    }


    // concatenate an std::vector<std::string> to a single string

    void concatenate(std::string delimiter = " ") {

      std::string tmp_str = boost::algorithm::join(v, delimiter);

      v.clear();

      v.resize(1);

      v[0] = tmp_str;

      tmp_str.shrink_to_fit();
    }


    // save an std::string to a file

    void save_2file(std::string folder, std::string path_extend = "output_token.txt") {

      std::string tmp_file = folder + path_extend;

      std::ofstream out(tmp_file);

      out << v[0];

      v.clear();

      v.resize(1);

      v[0] = "";      // in case that the output is saved to a file, then return "" in R-session

      out.close();
    }


    // std::ios::app is the open mode "append" meaning new data will be written to the end of the file.
    // http://stackoverflow.com/questions/6932409/writting-a-string-to-the-end-of-a-file-c
    //

    void append_2file(std::string folder, std::string path_extend = "output_token_single_file.txt") {

      std::string tmp_file = folder + path_extend;

      std::ofstream out;

      out.open(tmp_file, std::ios::app);

      out << v[0];

      v.clear();

      v.resize(1);

      v[0] = "";      // in case that the output is saved to a file, then return an empty string ("") in the R-session
    }


    // return std::vector<std::string> in any case

    std::vector<std::string> _object_vector() {

      return v;
    }

    ~TOKEN() { }
};


#endif


