
/**
 * Copyright (C) 2016 Lampros Mouselimis
 *
 * @file token_stats.h
 *
 * @author Lampros Mouselimis
 *
 * @date October - December 2016
 *
 * @Notes: statistics for tokenized and transformed text
 *
 * @last_modified: December 2016
 *
 **/


#ifndef __token_stats__
#define __token_stats__



class TOKEN_stats {

  public:


    TOKEN_stats() { }

    // returns the paths of files in a folder
    // http://www.cplusplus.com/forum/unices/3548/
    //

    std::vector<std::string> list_files( const std::string& path, bool full_path = true) {

      std::vector <std::string> result;

      dirent* de;

      DIR* dp;

      errno = 0;

      dp = opendir( path.empty() ? "." : path.c_str() );

      if (dp) {

        while (true) {

          errno = 0;

          de = readdir( dp );

          if (de == NULL) break;

          std::string tmp = std::string( de->d_name );

          int count = std::count_if(tmp.begin(), tmp.end(),[](char c){ return (std::isalnum(c)); });

          if (count > 0) {

            if (full_path) {

              std::string full_str = path + tmp;

              result.push_back( full_str );}

            else {

              result.push_back( tmp );
            }
          }
        }

        closedir( dp );

        std::sort( result.begin(), result.end() );
      }

      return result;
    }


    // loads the data of a folder OR a file to a vector [ not recommended for big files ]
    //

    std::vector<std::string> path_2vector(std::string path_2folder = "", std::string path_2file = "", char file_delimiter = '\n') {

      std::vector<std::string> res;

      if (path_2folder != "") {

        std::vector<std::string> FILES = list_files(path_2folder, true);

        for (unsigned int i = 0; i < FILES.size(); i++) {

          std::ifstream myfile(FILES[i]);

          std::string line;

          while (std::getline(myfile, line, file_delimiter)) {        // file split in chunks  using the tab-delimiter

            res.push_back(line);
          }
        }
      }

      else if (path_2file != "") {

        std::ifstream myfile(path_2file);

        std::string line;

        while (std::getline(myfile, line, file_delimiter)) {        // file split in chunks  using the tab-delimiter

          res.push_back(line);
        }
      }

      else {

        Rcpp::stop("invalid path to folder or file");
      }

      return res;
    }


    // it returns a named-unsorted vector frequency_distribution in R for EITHER a folder, a file OR a character string vector
    // [ in R I can retrieve a word using for instance VEC['truthl'] where VEC is the resulted vector ]
    // it works for n-grams too, but the file(s) must be in a folder
    //

    std::unordered_map<std::string, int> frequency_distribution(std::vector<std::string> &x, std::string path_2folder = "",

                                                                std::string path_2file = "", char file_delimiter = '\n') {

      std::unordered_map<std::string, int> res;

      if (path_2folder != "") {

        std::vector<std::string> FILES = list_files(path_2folder, true);

        for (unsigned int i = 0; i < FILES.size(); i++) {

          std::ifstream myfile(FILES[i]);

          std::string line;

          while (std::getline(myfile, line, file_delimiter)) {

            res[line]++;
          }
        }
      }

      else if (path_2file != "") {

        std::ifstream myfile(path_2file);

        std::string line;

        while (std::getline(myfile, line, file_delimiter)) {

          res[line]++;
        }
      }

      else if (!x.empty()) {

        for (unsigned int i = 0; i < x.size(); i++) {

          res[x[i]]++;
        }
      }

      else {

        Rcpp::stop("valid objects are path-folder, path-file and character vector");
      }

      return res;
    }


    // count number of characters of words.
    //

    std::unordered_map<int, std::vector<std::string>> count_characters(std::vector<std::string> &x, std::string path_2folder = "",

                                                                       std::string path_2file = "", char file_delimiter = '\n') {

      std::unordered_map<int, std::vector<std::string>> res;

      if (path_2folder != "") {

        std::vector<std::string> FILES = list_files(path_2folder, true);

        for (unsigned int i = 0; i < FILES.size(); i++) {

          std::ifstream myfile(FILES[i]);

          std::string line;

          while (std::getline(myfile, line, file_delimiter)) {

            res[line.size()].push_back(line);
          }
        }
      }

      else if (path_2file != "") {

        std::ifstream myfile(path_2file);

        std::string line;

        while (std::getline(myfile, line, file_delimiter)) {

          res[line.size()].push_back(line);
        }
      }

      else if (!x.empty()) {

        for (unsigned int i = 0; i < x.size(); i++) {

          res[x[i].size()].push_back(x[i]);
        }
      }

      else {

        Rcpp::stop("valid objects are path-folder, path-file and character vector");
      }

      return res;
    }


    // collocations of n-grams [ it returns a frequency table of words that co-occur with a specific word ]
    // If two words occur together a lot, then that is evidence that they have a special function that is
    // not simply explained as the function that results from their combination
    //

    std::unordered_map<std::string, std::unordered_map<std::string, int> > collocations_ngrams(std::vector<std::string> &x, std::string path_2folder = "",

                                                                                               std::string path_2file = "", char file_delimiter = '\n',

                                                                                               std::string n_gram_delimiter = "_") {
      std::unordered_map<std::string, std::vector<std::string>> tmp_v;

      if (path_2folder != "") {

        std::vector<std::string> FILES = list_files(path_2folder, true);

        for (unsigned int i = 0; i < FILES.size(); i++) {

          std::ifstream myfile(FILES[i]);

          std::string line;

          while (std::getline(myfile, line, file_delimiter)) {

            std::vector<std::string> tmp_vec;

            boost::split( tmp_vec, line, boost::is_any_of(n_gram_delimiter), boost::token_compress_on );

            int size_ngram = tmp_vec.size();

            for (int i = 0; i < size_ngram; i++) {

              std::vector<std::string> copy_tmp_vec = tmp_vec;

              copy_tmp_vec.erase(copy_tmp_vec.begin() + i);

              for (unsigned int k = 0; k < copy_tmp_vec.size(); k++) {

                tmp_v[tmp_vec[i]].push_back(copy_tmp_vec[k]);
              }
            }
          }
        }
      }

      else if (path_2file != "") {

        std::ifstream myfile(path_2file);

        std::string line;

        while (std::getline(myfile, line, file_delimiter)) {

          std::vector<std::string> tmp_vec;

          boost::split( tmp_vec, line, boost::is_any_of(n_gram_delimiter), boost::token_compress_on );

          int size_ngram = tmp_vec.size();

          for (int i = 0; i < size_ngram; i++) {

            std::vector<std::string> copy_tmp_vec = tmp_vec;

            copy_tmp_vec.erase(copy_tmp_vec.begin() + i);

            for (unsigned int k = 0; k < copy_tmp_vec.size(); k++) {

              tmp_v[tmp_vec[i]].push_back(copy_tmp_vec[k]);
            }
          }
        }
      }

      else if (!x.empty()) {

        for (unsigned int i = 0; i < x.size(); i++) {

          std::vector<std::string> tmp_vec;

          boost::split( tmp_vec, x[i], boost::is_any_of(n_gram_delimiter), boost::token_compress_on );

          int size_ngram = tmp_vec.size();

          for (int i = 0; i < size_ngram; i++) {

            std::vector<std::string> copy_tmp_vec = tmp_vec;

            copy_tmp_vec.erase(copy_tmp_vec.begin() + i);

            for (unsigned int k = 0; k < copy_tmp_vec.size(); k++) {

              tmp_v[tmp_vec[i]].push_back(copy_tmp_vec[k]);
            }
          }
        }
      }

      else {

        Rcpp::stop("valid objects are path-folder, path-file and character vector");
      }

      std::unordered_map<std::string, std::unordered_map<std::string, int> > res;

      for(auto iter : tmp_v) {

        std::vector<std::string> tmp_value = iter.second;

        std::unordered_map<std::string, int> map_tmp;

        for (unsigned int f = 0; f < tmp_value.size(); f++) {

          map_tmp[tmp_value[f]] += 1;
        }

        res[iter.first] = map_tmp;
      }

      return res;
    }



    // levenshtein distance between single words [ https://en.wikipedia.org/wiki/Levenshtein_distance ]
    //

    double levenshtein_dist(std::string &s, std::string &t) {

      if (s == t) return 0;
      if (s.length() == 0) return t.length();
      if (t.length() == 0) return s.length();

      arma::rowvec v0(t.length() + 1);
      arma::rowvec v1(t.length() + 1);

      for (unsigned int i = 0; i < v0.n_elem ; i++) {

        v0[i] = i;
      }

      for (unsigned int i = 0; i < s.length(); i++) {

        v1[0] = i + 1;

        for (unsigned int j = 0; j < t.length(); j++) {

          int cost = (s[i] == t[j]) ? 0 : 1;           // condition ? result_if_true : result_if_false

          arma::rowvec tmp_vec = {v1[j] + 1, v0[j + 1] + 1, v0[j] + cost};

          v1[j + 1] = min(tmp_vec);
        }

        for (unsigned int j = 0; j < v0.size(); j++) {

          v0[j] = v1[j];
        }
      }

      double tmp_val = v1[t.length()];

      return(tmp_val);
    }


    // cosine distance for sentences (strings containing more than 1 word)
    // the sentences will be first split into words using a tokenizer
    // http://stackoverflow.com/questions/15173225/how-to-calculate-cosine-similarity-given-2-sentence-strings-python
    //

    double cosine_dist(std::string &x, std::string &y, std::string separator = " ") {

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


    // character n-grams:  here I want the exact n_grams of the words. In case that a word has less characters than the n_grams
    // parameter then return an empty string ("") rather than {x} (which would be the word). In that way I can discard empty strings
    // when comparing two strings
    //

    std::vector<std::string> char_n_grams(std::string &x, int n_grams, bool return_word = false, bool add_prefix = false) {

      int x_size = x.size();
      
      if (add_prefix) {

        x = "_" + x + "_";
      }

      if (n_grams >= x_size) {

        if (return_word) {

          return {x};}

        else {

          return {""};
        }
      }

      else {

        int n_size = x_size - n_grams + 1;

        std::vector<std::string> out(n_size);

        for (int i = 0; i < n_size; i++) {

          std::string n_gram;

          for (int j = i; j < i + n_grams; j++) {

            n_gram += x[j];
          }

          out[i] = n_gram;
        }

        return out;
      }
    }


    // dice-coefficient (similarity) between two strings for a specific number of n-grams
    //

    double dice_similarity(std::string x, std::string y, int n_grams) {

      // string x

      std::vector<std::string> tmp_x = char_n_grams(x, n_grams, false, true);

      std::sort(tmp_x.begin(), tmp_x.end());

      tmp_x.erase(std::unique(tmp_x.begin(), tmp_x.end()), tmp_x.end());

      int size_x = tmp_x.size();

      // string y

      std::vector<std::string> tmp_y = char_n_grams(y, n_grams, false, true);

      std::sort(tmp_y.begin(), tmp_y.end());

      tmp_y.erase(std::unique(tmp_y.begin(), tmp_y.end()), tmp_y.end());

      int size_y = tmp_y.size();

      // end-vector of both words

      std::vector<std::string> tmp_n_grams;

      std::set_intersection(tmp_x.begin(), tmp_x.end(), tmp_y.begin(), tmp_y.end(), std::back_inserter(tmp_n_grams));

      // dice-coefficient

      double dice = 1.0 - (2.0 * tmp_n_grams.size()) / (size_x + size_y);

      return dice;
    }


    // dissimilarity matrix using the dice-coefficient for the n-gram clustering [ set a threshold so that if a two-word's dissimilarity
    // value is greater than the threshold value, then they are (entirely) dissimilar and thus have a distance of 1.0 ]
    // The 'dice' method is appropriate for n-gram string characters, the 'levenstein' distance is appropriate for strings and the 'cosine'
    // distance is appropriate for sentences (which are split into words)
    // method == "dice" calculates a dissimilarity matrix for the n-grams of a string
    // method == "levenshtein" calculates the dissimilarity distance between two strings (NOT for the n-gram characters)
    // method == "cosine" calculates the dissimilarity distance for sentences (more than 1 word)
    //

    arma::mat dissimilarity_mat(std::vector<std::string> &words, int dice_n_gram = 2, std::string method = "dice", std::string split_separator = " ",

                                double dice_thresh = 0.3, bool upper = true, bool diagonal = true, int threads = 1) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      arma::mat mt(words.size(), words.size());

      mt.fill(arma::datum::nan);

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static)
      #endif
      for (unsigned int i = 0; i < words.size() - 1; i++) {

        int k = i;

        #ifdef _OPENMP
        #pragma omp parallel for schedule(static)
        #endif
        for (unsigned int j = k + 1; j < words.size(); j++) {

          double tmp_idx = 0.0;

          if (method == "dice") {

            tmp_idx = dice_similarity(words[i], words[j], dice_n_gram);

            if (tmp_idx >= dice_thresh) { tmp_idx = 1.0; }     // special case when method = 'dice' see : http://www.anthology.aclweb.org/P/P00/P00-1026.pdf, page 3
          }

          if (method == "levenshtein") {

            tmp_idx = levenshtein_dist(words[i], words[j]);
          }

          if (method == "cosine") {

            tmp_idx = cosine_dist(words[i], words[j], split_separator);
          }

          mt(j,i) = tmp_idx;

          if (upper) {

            mt(i,j) = tmp_idx;
          }
        }
      }

      if (diagonal) {

        mt.diag().zeros();
      }

      return mt;
    }


    // look-up table using n-grams for a vector of strings
    // each n-gram is associated with the initial word, thus each n-gram can belong to more than one words
    //

    std::unordered_map<std::string, std::vector<std::string>> look_up_tbl(std::vector<std::string> &VEC, int n_grams) {

      std::unordered_map<std::string, std::vector<std::string>> out;

      for (unsigned int i = 0; i < VEC.size(); i++) {

        std::vector<std::string> tmp_vec = char_n_grams(VEC[i], n_grams, false, true);

        for (unsigned int j = 0; j < tmp_vec.size(); j++) {

          out[tmp_vec[j]].push_back(VEC[i]);
        }
      }

      return out;
    }


    ~TOKEN_stats() { }
};


#endif
