


#' String tokenization and transformation  ( character string or path to a file )
#'
#'
#' @param object either a character string (text data) or a character-string-path to a file (for big .txt files it's recommended to use a path to a file).
#' @param batches a numeric value. If the \emph{batches} parameter is not NULL then the \emph{object} parameter should be a valid path to a file and the \emph{path_2folder} parameter should be a valid path to a folder. The batches parameter should be used in case of small to medium data sets (for zero memory consumption). For big data sets the \emph{big_tokenize_transform} R6 class and especially the \emph{big_text_tokenizer} function should be used.
#' @param read_file_delimiter the delimiter to use when the input file will be red (for instance a tab-delimiter or a new-line delimiter).
#' @param to_lower either TRUE or FALSE. If TRUE the character string will be converted to lower case
#' @param to_upper either TRUE or FALSE. If TRUE the character string will be converted to upper case
#' @param utf_locale the language specific locale to use in case that either the \emph{to_lower} or the \emph{to_upper} parameter is TRUE and the text file language is other than english. For instance if the language of a text file is greek then the \emph{utf_locale} parameter should be \emph{'el_GR.UTF-8'} ( \emph{language_country.encoding} ). A wrong utf-locale does not raise an error, however the runtime of the function increases.
#' @param remove_char a character string with specific characters that should be removed from the text file. If the \emph{remove_char} is "" then no removal of characters take place
#' @param remove_punctuation_string either TRUE or FALSE. If TRUE then the punctuation of the character string will be removed (applies before the split function)
#' @param remove_punctuation_vector either TRUE or FALSE. If TRUE then the punctuation of the vector of the character strings will be removed  (after the string split has taken place)
#' @param remove_numbers either TRUE or FALSE. If TRUE then any numbers in the character string will be removed
#' @param trim_token either TRUE or FALSE. If TRUE then the string will be trimmed (left and/or right)
#' @param split_string either TRUE or FALSE. If TRUE then the character string will be split using the \emph{split_separator} as delimiter. The user can also specify multiple delimiters.
#' @param split_separator a character string specifying the character delimiter(s)
#' @param remove_stopwords either TRUE, FALSE or a character vector of user defined stop words. If TRUE then by using the \emph{language} parameter the corresponding stop words vector will be uploaded.
#' @param language a character string which defaults to english. If the \emph{remove_stopwords} parameter is TRUE then the corresponding stop words vector will be uploaded. Available languages
#' are \emph{afrikaans}, \emph{arabic}, \emph{armenian}, \emph{basque}, \emph{bengali}, \emph{breton}, \emph{bulgarian}, \emph{catalan},
#' \emph{croatian}, \emph{czech}, \emph{danish}, \emph{dutch}, \emph{english}, \emph{estonian},
#' \emph{finnish}, \emph{french}, \emph{galician}, \emph{german}, \emph{greek}, \emph{hausa}, \emph{hebrew}, \emph{hindi}, \emph{hungarian},
#' \emph{indonesian}, \emph{irish}, \emph{italian}, \emph{latvian}, \emph{marathi},
#' \emph{norwegian}, \emph{persian}, \emph{polish}, \emph{portuguese}, \emph{romanian}, \emph{russian}, \emph{slovak}, \emph{slovenian},
#' \emph{somalia}, \emph{spanish}, \emph{swahili}, \emph{swedish}, \emph{turkish}, \emph{yoruba}, \emph{zulu}
#' @param min_num_char an integer specifying the minimum number of characters to keep. If the \emph{min_num_char} is greater than 1 then character strings with more than 1 characters will be returned
#' @param max_num_char an integer specifying the maximum number of characters to keep. The \emph{max_num_char} should be less than or equal to \emph{Inf} (in this function the Inf value translates to a word-length of 1000000000)
#' @param stemmer a character string specifying the stemming method. One of the following \emph{porter2_stemmer}, \emph{ngram_sequential}, \emph{ngram_overlap}. See details for more information.
#' @param min_n_gram an integer specifying the minimum number of n-grams. The minimum number of min_n_gram is 1.
#' @param max_n_gram an integer specifying the maximum number of n-grams. The minimum number of max_n_gram is 1.
#' @param skip_n_gram an integer specifying the number of skip-n-grams. The minimum number of skip_n_gram is 1. The skip_n_gram gives the (max.) n-grams using the \emph{skip_distance} parameter. If \emph{skip_n_gram} is greater than 1 then both \emph{min_n_gram} and \emph{max_n_gram} should be set to 1.
#' @param skip_distance an integer specifying the skip distance between the words. The minimum value for the skip distance is 0, in which case simple n-grams will be returned.
#' @param n_gram_delimiter a character string specifying the n-gram delimiter (applies to both n-gram and skip-n-gram cases)
#' @param concat_delimiter either NULL or a character string specifying the delimiter to use in order to concatenate the end-vector of character strings to a single character string (recommended in case that the end-vector should be saved to a file)
#' @param path_2folder a character string specifying the path to the folder where the file(s) will be saved
#' @param stemmer_ngram a numeric value greater than 1. Applies to both \emph{ngram_sequential} and \emph{ngram_overlap} methods. In case of \emph{ngram_sequential} the first \emph{n} characters will be picked, whereas in the case of \emph{ngram_overlap} the overlapping stemmer_ngram characters will be build.
#' @param stemmer_gamma a float number greater or equal to 0.0. Applies only to \emph{ngram_sequential}. Is a threshold value, which defines how much frequency deviation of two N-grams is acceptable. It is kept either zero or to a minimum value.
#' @param stemmer_truncate a numeric value greater than 0. Applies only to \emph{ngram_sequential}. The ngram_sequential is modified to use relative frequencies (float numbers between 0.0 and 1.0 for the ngrams of a specific word in the corpus) and the stemmer_truncate parameter controls the number of rounding digits for the ngrams of the word. The main purpose was to give the same relative frequency to words appearing approximately the same on the corpus.
#' @param stemmer_batches a numeric value greater than 0. Applies only to \emph{ngram_sequential}. Splits the corpus into batches with the option to run the batches in multiple threads.
#' @param threads an integer specifying the number of cores to run in parallel
#' @param vocabulary_path_file either NULL or a character string specifying the output path to a file where the vocabulary should be saved once the text is tokenized
#' @param verbose either TRUE or FALSE. If TRUE then information will be printed out
#' @return a character vector
#' @importFrom utils read.table
#' @export
#' @details
#' It is memory efficient to read the data using a \emph{path file} in case of a big file, rather than importing the data in the R-session and then calling the \emph{tokenize_transform_text} function.
#'
#' It is memory efficient to give a \emph{path_2folder} in case that a big file should be saved, rather than return the vector of all character strings in the R-session.
#'
#' The \emph{skip-grams} are a generalization of n-grams in which the components (typically words) need not to be consecutive in the text under consideration, but may leave gaps that are skipped over. They provide one way of overcoming the \emph{data sparsity problem} found with conventional n-gram analysis.
#'
#' Many character string pre-processing functions (such as the \emph{utf-locale} or the \emph{split-string} function ) are based on the \emph{boost} library ( \url{http://www.boost.org/} ).
#'
#' Stemming of the english language is done using the porter2-stemmer, for details see \url{https://github.com/smassung/porter2_stemmer}
#'
#' N-gram stemming is language independent and supported by the following two functions:
#'
#' \describe{
#'  \item{ngram_overlap}{The \emph{ngram_overlap} stemming method is based on \emph{N-Gram Morphemes for Retrieval, Paul McNamee and James Mayfield}, \url{http://clef.isti.cnr.it/2007/working_notes/mcnameeCLEF2007.pdf}}
#'  \item{ngram_sequential}{The \emph{ngram_sequential} stemming method is a modified version based on \emph{Generation, Implementation and Appraisal of an N-gram based Stemming Algorithm, B. P. Pande, Pawan Tamta, H. S. Dhami}, \url{https://arxiv.org/pdf/1312.4824.pdf}}
#' }
#'
#' The list of stop-words in the available languages was downloaded from the following link, \url{https://github.com/6/stopwords-json}
#'
#' @examples
#'
#' library(textTinyR)
#'
#' token_str = "CONVERT to lower, remove.. punctuation11234, trim token and split "
#'
#' res = tokenize_transform_text(object = token_str, to_lower = TRUE, split_string = TRUE)



tokenize_transform_text = function(object = NULL, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",

                                  remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,

                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,

                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0,

                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE) {


  if (is.null(object)) stop("the object parameter should be a (non-NULL) character string")
  if (!is.null(batches)) {
    if (!inherits(batches, c('numeric', 'integer'))) { stop("the batches parameter should be a numeric value")}}
  if (!inherits(path_2folder, "character")) stop("the path_2folder parameter should be a ( character string ) valid path to a folder")
  if (path_2folder != "") {
    str_SPL = strsplit(path_2folder, "")[[1]]
    if (!str_SPL[nchar(path_2folder)] %in% c("/", "\\")) stop('the path_2folder path should end in slash')}
  if (!is.null(batches) && path_2folder == "") stop("give the path to a valid folder in case that the batches parameter is not NULL")
  if (length(object) > 1) stop("the object parameter should be a character string or a character path to a file of length 1")
  try_err_file_input = inherits(tryCatch(normalizePath(object, mustWork = T), error = function(e) e), "error")
  FLAG_path = T
  if (try_err_file_input) { FLAG_path = F }
  if (try_err_file_input && !is.null(batches)) stop("in case that the batches parameter is not NULL the object parameter should be a valid path to a file")
  if (!is.logical(to_lower)) stop("the to_lower parameter should be either TRUE or FALSE")
  if (!inherits(read_file_delimiter, 'character')) stop("the read_file_delimiter parameter should be a character string")
  if (!is.character(remove_char)) stop("the remove_char parameter should be a character string")
  if (!is.logical(to_upper)) stop("the to_upper parameter should be either TRUE or FALSE")
  if (!inherits(utf_locale, 'character')) stop("the 'utf_locale' parameter should be a character string")
  if (!is.logical(remove_punctuation_string)) stop("the remove_punctuation_string parameter should be either TRUE or FALSE")
  if (!is.logical(remove_punctuation_vector)) stop("the remove_punctuation_vector parameter should be either TRUE or FALSE")
  if (!is.logical(remove_numbers)) stop("the remove_numbers parameter should be either TRUE or FALSE")
  if (!is.logical(trim_token)) stop("the trim_token parameter should be either TRUE or FALSE")
  if (!inherits(remove_stopwords, c('character', 'vector', 'logical'))) {
      stop("the 'remove_stopwords' parameter should be either TRUE, FALSE or a character vector of stop words")}
  if (!language %in% c("afrikaans", "arabic", "armenian", "basque", "bengali", "breton",
                       "bulgarian", "catalan", "croatian", "czech", "danish", "dutch", "english",
                       "estonian", "finnish", "french","galician", "german", "greek", "hausa", "hebrew",
                       "hindi", "hungarian", "indonesian", "irish", "italian", "latvian", "marathi",
                       "norwegian", "persian", "polish", "portuguese", "romanian", "russian", "slovak",
                       "slovenian", "somalia", "spanish", "swahili", "swedish", "turkish", "yoruba", "zulu"))
    stop("available languages in case of stop-word removal are 'afrikaans', 'arabic', 'armenian',
        'basque', 'bengali', 'breton', 'bulgarian', 'catalan', 'croatian', 'czech',
        'danish', 'dutch', 'english', 'estonian', 'finnish', 'french', 'galician',
        'german', 'greek', 'hausa', 'hebrew', 'hindi', 'hungarian', 'indonesian', 'irish',
        'italian', 'latvian', 'marathi', 'norwegian', 'persian', 'polish', 'portuguese',
        'romanian', 'russian', 'slovak', 'slovenian', 'somalia','spanish', 'swahili',
         'swedish', 'turkish', 'yoruba', 'zulu'.

         A user defined character vector of stop words can be used as input, if the target language is not included")
  if (min_num_char < 1) stop("the min_num_char parameter should be greater than 0")
  if (min_num_char >= max_num_char) stop("the max_num_char parameter should be greater than the min_num_char")
  if (max_num_char == Inf) max_num_char = 1000000000
  if (!is.null(stemmer)) {
    if (!stemmer %in% c("porter2_stemmer", "ngram_sequential", "ngram_overlap")) stop("valid stemming methods are porter2_stemmer, ngram_sequential or ngram_overlap")
    if (stemmer == "ngram_sequential") {
      if (stemmer_ngram < 1) stop("the minimum value for the stemmer_ngram parameter should be 1")
      if (stemmer_gamma < 0.0) stop("the minimum value for the stemmer_gamma parameter should be 0.0")
      if (stemmer_truncate < 1) stop("the minimum value for the stemmer_truncate parameter should be 1")
      if (stemmer_batches < 1) stop("the minimum value for the stemmer_batches parameter should be 1")
    }
    if (stemmer == "ngram_overlap") {
      if (stemmer_ngram < 1) stop("the minimum value for the stemmer_ngram parameter should be 1")
    }
  }
  if (is.null(stemmer)) stemmer = "NULL"
  if (min_n_gram < 1) stop("the min_n_gram parameter should be greater than 0")
  if (max_n_gram < 1) stop("the max_n_gram parameter should be greater than 0")
  if (skip_n_gram < 1) stop("the skip_n_gram parameter should be greater than 0")
  if (skip_distance < 0) stop("the skip_distance parameter should be greater or equal to 0")
  if (min_n_gram > max_n_gram) stop("the min_n_gram parameter should be less than or equal to the max_n_gram parameter")
  if (!is.character(n_gram_delimiter)) stop("the n_gram_delimiter parameter should be a character string")
  if (!inherits(split_separator, 'character')) stop("the split_separator should be a character string consisting of one or more delimiters")
  if (threads < 1) stop("the number of threads should be at least 1")
  if (!is.logical(verbose)) stop("the verbose parameter should be either TRUE or FALSE")
  if (!is.null(concat_delimiter)) {
    if (!inherits(concat_delimiter, 'character')) {
      stop("the concat_delimiter parameter should be a character string")}}
  if (is.null(concat_delimiter)) concat_delimiter = "NULL"
  if (!is.logical(split_string)) stop("the split_string parameter should be either TRUE or FALSE")
  if (!is.null(vocabulary_path_file)) {
    if (!inherits(vocabulary_path_file, 'character')) stop("the vocabulary_path_file parameter should be a character string specifying a valid path to a file")}
  if (is.null(vocabulary_path_file)) vocabulary_path_file = ""
  if ((to_lower || to_upper) && language != "english") {          # THIS IS NOT THE ONLY EXCEPTION: in case of a user-defined stop-words list if the language is other than english and the default language stays the same ('english'), then the output is incorrect
    warning("if the 'language' parameter is not english and either a 'to_lower' or a 'to_upper' conversion takes place consider changing the 'utf_locale' parameter", call. = F)
  }

  if (verbose) { start = Sys.time() }

  if (is.logical(remove_stopwords)) {

    if (remove_stopwords) {

      language_path = system.file("stopwords", paste0(language, ".txt"), package = "textTinyR")

      language_stop_words = utils::read.table(language_path, quote = "\"", comment.char = "", stringsAsFactors = F)

      language_stop_words[nrow(language_stop_words) + 1, ] = ""                  # add the empty character to the stopwords

      language_stop_words = as.vector(language_stop_words[, 1])}

    else {

      language_stop_words = character(0)
    }
  }

  if (inherits(remove_stopwords, c('character', 'vector'))) {

    language_stop_words = remove_stopwords

    remove_stopwords = T
  }

  gc();

  if (is.null(batches)) {

    res = res_token(object, language_stop_words, language, utf_locale, FLAG_path, read_file_delimiter, max_num_char, remove_char = remove_char, cpp_to_lower = to_lower,

                    cpp_to_upper = to_upper, cpp_remove_punctuation = remove_punctuation_string, remove_punctuation_vector = remove_punctuation_vector, cpp_remove_numbers = remove_numbers,

                    cpp_trim_token = trim_token, cpp_tokenization_function = split_string, cpp_string_separator = split_separator, cpp_remove_stopwords = remove_stopwords,

                    min_num_char = min_num_char, stemmer = stemmer, min_n_gram = min_n_gram, max_n_gram = max_n_gram, skip_n_gram = skip_n_gram, skip_distance = skip_distance,

                    n_gram_delimiter = n_gram_delimiter, concat_delimiter = concat_delimiter, path_2file = path_2folder, stemmer_ngram = stemmer_ngram, stemmer_gamma = stemmer_gamma,

                    stemmer_truncate = stemmer_truncate, stemmer_batches = stemmer_batches, threads = threads, verbose = verbose, save_2single_file = F, path_extend = "output_token.txt",

                    vocabulary_path = vocabulary_path_file)

    gc();

    token_structure = structure(list(token = res), class = "tokenization and transformation")}

  else {

    if (concat_delimiter == "NULL") concat_delimiter = "\n"

    res = batch_2file(INPUT_FILE = object, OUTPUT_PATH = path_2folder, batches, read_file_delimiter, language_stop_words, language, utf_locale, max_num_char, remove_char = remove_char,

                      cpp_to_lower = to_lower, cpp_to_upper = to_upper, cpp_remove_punctuation = remove_punctuation_string, remove_punctuation_vector = remove_punctuation_vector,

                      cpp_remove_numbers = remove_numbers, cpp_trim_token = trim_token, cpp_tokenization_function = split_string, cpp_string_separator = split_separator,

                      cpp_remove_stopwords = remove_stopwords, min_num_char = min_num_char, stemmer = stemmer, min_n_gram = min_n_gram, max_n_gram = max_n_gram, skip_n_gram = skip_n_gram,

                      skip_distance = skip_distance, n_gram_delimiter = n_gram_delimiter, stemmer_ngram = stemmer_ngram, stemmer_gamma = stemmer_gamma, stemmer_truncate = stemmer_truncate,

                      stemmer_batches = stemmer_batches, threads = threads, concat_delimiter = concat_delimiter, vocabulary_path = vocabulary_path_file, verbose = verbose)
    gc();

    token_structure = structure(list(token = paste0("the batch-data-output is saved in : ", path_2folder)), class = "tokenization and transformation")
  }

  if (verbose) {

    end = Sys.time()

    t = end - start

    cat('\n'); cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');
  }

  return(token_structure)
}




#' String tokenization and transformation ( vector of documents )
#'
#'
#' @param object a character string vector of documents
#' @param as_token if TRUE then the output of the function is a list of (split) token. Otherwise is a vector of character strings (sentences)
#' @param to_lower either TRUE or FALSE. If TRUE the character string will be converted to lower case
#' @param to_upper either TRUE or FALSE. If TRUE the character string will be converted to upper case
#' @param utf_locale the language specific locale to use in case that either the \emph{to_lower} or the \emph{to_upper} parameter is TRUE and the text file language is other than english. For instance if the language of a text file is greek then the \emph{utf_locale} parameter should be \emph{'el_GR.UTF-8'} ( \emph{language_country.encoding} ). A wrong utf-locale does not raise an error, however the runtime of the function increases.
#' @param remove_char a character string with specific characters that should be removed from the text file. If the \emph{remove_char} is "" then no removal of characters take place
#' @param remove_punctuation_string either TRUE or FALSE. If TRUE then the punctuation of the character string will be removed (applies before the split function)
#' @param remove_punctuation_vector either TRUE or FALSE. If TRUE then the punctuation of the vector of the character strings will be removed  (after the string split has taken place)
#' @param remove_numbers either TRUE or FALSE. If TRUE then any numbers in the character string will be removed
#' @param trim_token either TRUE or FALSE. If TRUE then the string will be trimmed (left and/or right)
#' @param split_string either TRUE or FALSE. If TRUE then the character string will be split using the \emph{split_separator} as delimiter. The user can also specify multiple delimiters.
#' @param split_separator a character string specifying the character delimiter(s)
#' @param remove_stopwords either TRUE, FALSE or a character vector of user defined stop words. If TRUE then by using the \emph{language} parameter the corresponding stop words vector will be uploaded.
#' @param language a character string which defaults to english. If the \emph{remove_stopwords} parameter is TRUE then the corresponding stop words vector will be uploaded. Available languages
#' are \emph{afrikaans}, \emph{arabic}, \emph{armenian}, \emph{basque}, \emph{bengali}, \emph{breton}, \emph{bulgarian}, \emph{catalan},
#' \emph{croatian}, \emph{czech}, \emph{danish}, \emph{dutch}, \emph{english}, \emph{estonian},
#' \emph{finnish}, \emph{french}, \emph{galician}, \emph{german}, \emph{greek}, \emph{hausa}, \emph{hebrew}, \emph{hindi}, \emph{hungarian},
#' \emph{indonesian}, \emph{irish}, \emph{italian}, \emph{latvian}, \emph{marathi},
#' \emph{norwegian}, \emph{persian}, \emph{polish}, \emph{portuguese}, \emph{romanian}, \emph{russian}, \emph{slovak}, \emph{slovenian},
#' \emph{somalia}, \emph{spanish}, \emph{swahili}, \emph{swedish}, \emph{turkish}, \emph{yoruba}, \emph{zulu}
#' @param min_num_char an integer specifying the minimum number of characters to keep. If the \emph{min_num_char} is greater than 1 then character strings with more than 1 characters will be returned
#' @param max_num_char an integer specifying the maximum number of characters to keep. The \emph{max_num_char} should be less than or equal to \emph{Inf} (in this function the Inf value translates to a word-length of 1000000000)
#' @param stemmer a character string specifying the stemming method. Available method is the \emph{porter2_stemmer}. See details for more information.
#' @param min_n_gram an integer specifying the minimum number of n-grams. The minimum number of min_n_gram is 1.
#' @param max_n_gram an integer specifying the maximum number of n-grams. The minimum number of max_n_gram is 1.
#' @param skip_n_gram an integer specifying the number of skip-n-grams. The minimum number of skip_n_gram is 1. The skip_n_gram gives the (max.) n-grams using the \emph{skip_distance} parameter. If \emph{skip_n_gram} is greater than 1 then both \emph{min_n_gram} and \emph{max_n_gram} should be set to 1.
#' @param skip_distance an integer specifying the skip distance between the words. The minimum value for the skip distance is 0, in which case simple n-grams will be returned.
#' @param n_gram_delimiter a character string specifying the n-gram delimiter (applies to both n-gram and skip-n-gram cases)
#' @param concat_delimiter either NULL or a character string specifying the delimiter to use in order to concatenate the end-vector of character strings to a single character string (recommended in case that the end-vector should be saved to a file)
#' @param path_2folder a character string specifying the path to the folder where the file(s) will be saved
#' @param threads an integer specifying the number of cores to run in parallel
#' @param vocabulary_path_file either NULL or a character string specifying the output path to a file where the vocabulary should be saved once the text is tokenized
#' @param verbose either TRUE or FALSE. If TRUE then information will be printed out
#' @return a character vector
#' @importFrom utils read.table
#' @export
#' @details
#'
#' It is memory efficient to give a \emph{path_2folder} in case that a big file should be saved, rather than return the vector of all character strings in the R-session.
#'
#' The \emph{skip-grams} are a generalization of n-grams in which the components (typically words) need not to be consecutive in the text under consideration, but may leave gaps that are skipped over. They provide one way of overcoming the \emph{data sparsity problem} found with conventional n-gram analysis.
#'
#' Many character string pre-processing functions (such as the \emph{utf-locale} or the \emph{split-string} function ) are based on the \emph{boost} library ( \url{http://www.boost.org/} ).
#'
#' Stemming of the english language is done using the porter2-stemmer, for details see \url{https://github.com/smassung/porter2_stemmer}
#'
#' The list of stop-words in the available languages was downloaded from the following link, \url{https://github.com/6/stopwords-json}
#'
#' @examples
#'
#' library(textTinyR)
#'
#' token_doc_vec = c("CONVERT to lower", "remove.. punctuation11234", "trim token and split ")
#'
#' res = tokenize_transform_vec_docs(object = token_doc_vec, to_lower = TRUE, split_string = TRUE)



tokenize_transform_vec_docs = function(object = NULL, as_token = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                       remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                       min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                       concat_delimiter = NULL, path_2folder = "", threads = 1, vocabulary_path_file = NULL, verbose = FALSE) {


  if (is.null(object)) stop("the object parameter should be a (non-NULL) character string")
  if (!inherits(object, c('vector', 'character'))) stop("the object parameter should be a character vector")
  if (length(object) < 2) stop("the length of the vector should be at least 2")
  if (!is.logical(as_token)) stop("the as_token parameter should be either TRUE or FALSE")
  if (!inherits(path_2folder, "character")) stop("the path_2folder parameter should be a ( character string ) valid path to a folder")
  if (path_2folder != "") {
    str_SPL = strsplit(path_2folder, "")[[1]]
    if (!str_SPL[nchar(path_2folder)] %in% c("/", "\\")) stop('the path_2folder path should end in slash')}
  if (!is.logical(to_lower)) stop("the to_lower parameter should be either TRUE or FALSE")
  if (!is.character(remove_char)) stop("the remove_char parameter should be a character string")
  if (!is.logical(to_upper)) stop("the to_upper parameter should be either TRUE or FALSE")
  if (!inherits(utf_locale, 'character')) stop("the 'utf_locale' parameter should be a character string")
  if (!is.logical(remove_punctuation_string)) stop("the remove_punctuation_string parameter should be either TRUE or FALSE")
  if (!is.logical(remove_punctuation_vector)) stop("the remove_punctuation_vector parameter should be either TRUE or FALSE")
  if (!is.logical(remove_numbers)) stop("the remove_numbers parameter should be either TRUE or FALSE")
  if (!is.logical(trim_token)) stop("the trim_token parameter should be either TRUE or FALSE")
  if (!inherits(remove_stopwords, c('character', 'vector', 'logical'))) {
    stop("the 'remove_stopwords' parameter should be either TRUE, FALSE or a character vector of stop words")}
  if (!language %in% c("afrikaans", "arabic", "armenian", "basque", "bengali", "breton",
                       "bulgarian", "catalan", "croatian", "czech", "danish", "dutch", "english",
                       "estonian", "finnish", "french","galician", "german", "greek", "hausa", "hebrew",
                       "hindi", "hungarian", "indonesian", "irish", "italian", "latvian", "marathi",
                       "norwegian", "persian", "polish", "portuguese", "romanian", "russian", "slovak",
                       "slovenian", "somalia", "spanish", "swahili", "swedish", "turkish", "yoruba", "zulu"))
    stop("available languages in case of stop-word removal are 'afrikaans', 'arabic', 'armenian',
         'basque', 'bengali', 'breton', 'bulgarian', 'catalan', 'croatian', 'czech',
         'danish', 'dutch', 'english', 'estonian', 'finnish', 'french', 'galician',
         'german', 'greek', 'hausa', 'hebrew', 'hindi', 'hungarian', 'indonesian', 'irish',
         'italian', 'latvian', 'marathi', 'norwegian', 'persian', 'polish', 'portuguese',
         'romanian', 'russian', 'slovak', 'slovenian', 'somalia','spanish', 'swahili',
         'swedish', 'turkish', 'yoruba', 'zulu'.

         A user defined character vector of stop words can be used as input, if the target language is not included")
  if (min_num_char < 1) stop("the min_num_char parameter should be greater than 0")
  if (min_num_char >= max_num_char) stop("the max_num_char parameter should be greater than the min_num_char")
  if (max_num_char == Inf) max_num_char = 1000000000
  if (!is.null(stemmer)) {
    if (!stemmer %in% c("porter2_stemmer")) stop("valid stemming method is porter2_stemmer")
  }
  if (is.null(stemmer)) stemmer = "NULL"
  if (min_n_gram < 1) stop("the min_n_gram parameter should be greater than 0")
  if (max_n_gram < 1) stop("the max_n_gram parameter should be greater than 0")
  if (skip_n_gram < 1) stop("the skip_n_gram parameter should be greater than 0")
  if (skip_distance < 0) stop("the skip_distance parameter should be greater or equal to 0")
  if (min_n_gram > max_n_gram) stop("the min_n_gram parameter should be less than or equal to the max_n_gram parameter")
  if (!is.character(n_gram_delimiter)) stop("the n_gram_delimiter parameter should be a character string")
  if (!inherits(split_separator, 'character')) stop("the split_separator should be a character string consisting of one or more delimiters")
  if (threads < 1) stop("the number of threads should be at least 1")
  if (!is.logical(verbose)) stop("the verbose parameter should be either TRUE or FALSE")
  if (!is.null(concat_delimiter)) {
    if (!inherits(concat_delimiter, 'character')) {
      stop("the concat_delimiter parameter should be a character string")}}
  if (is.null(concat_delimiter)) concat_delimiter = "NULL"
  if (!is.logical(split_string)) stop("the split_string parameter should be either TRUE or FALSE")
  if (!is.null(vocabulary_path_file)) {
    if (!inherits(vocabulary_path_file, 'character')) stop("the vocabulary_path_file parameter should be a character string specifying a valid path to a file")}
  if (is.null(vocabulary_path_file)) vocabulary_path_file = ""
  if ((to_lower || to_upper) && language != "english") {          # THIS IS NOT THE ONLY EXCEPTION: in case of a user-defined stop-words list if the language is other than english and the default language stays the same ('english'), then the output is incorrect
    warning("if the 'language' parameter is not english and either a 'to_lower' or a 'to_upper' conversion takes place consider changing the 'utf_locale' parameter", call. = F)
  }

  if (.Platform$OS.type == "windows") {

    if (!utf_locale %in% c("", "en.UTF-8")) {

      threads = 1                                   # single thread when utf-locale is not english and OS is windows
    }
  }

  if (verbose) { start = Sys.time() }

  if (is.logical(remove_stopwords)) {

    if (remove_stopwords) {

      language_path = system.file("stopwords", paste0(language, ".txt"), package = "textTinyR")

      language_stop_words = utils::read.table(language_path, quote = "\"", comment.char = "", stringsAsFactors = F)

      language_stop_words[nrow(language_stop_words) + 1, ] = ""                  # add the empty character to the stopwords

      language_stop_words = as.vector(language_stop_words[, 1])}

    else {

      language_stop_words = character(0)
    }
  }

  if (inherits(remove_stopwords, c('character', 'vector'))) {

    language_stop_words = remove_stopwords

    remove_stopwords = T
  }

  gc();

  if (as_token) {

    res = res_token_list( object, language_stop_words, language, utf_locale, max_num_char, remove_char = remove_char, cpp_to_lower = to_lower, cpp_to_upper = to_upper,

                          cpp_remove_punctuation = remove_punctuation_string, remove_punctuation_vector = remove_punctuation_vector, cpp_remove_numbers = remove_numbers,

                          cpp_trim_token = trim_token, cpp_tokenization_function = split_string, cpp_string_separator = split_separator, cpp_remove_stopwords = remove_stopwords,

                          min_num_char = min_num_char, stemmer = stemmer, min_n_gram = min_n_gram, max_n_gram = max_n_gram, skip_n_gram = skip_n_gram, skip_distance = skip_distance,

                          n_gram_delimiter = n_gram_delimiter, concat_delimiter = concat_delimiter, path_2file = path_2folder, stemmer_ngram = 4, stemmer_gamma = 0.0,                         # add constant values for stemming other than porter_2stemmer [ n-gram stemming applies to a whole corpus and not to single sub-vectors of documents ]

                          stemmer_truncate = 3, stemmer_batches = 1, threads = threads, verbose = verbose, vocabulary_path = vocabulary_path_file )}

  else {

    res = res_token_vector( object, language_stop_words, language, utf_locale, max_num_char, remove_char = remove_char, cpp_to_lower = to_lower, cpp_to_upper = to_upper,

                           cpp_remove_punctuation = remove_punctuation_string, remove_punctuation_vector = remove_punctuation_vector, cpp_remove_numbers = remove_numbers,

                           cpp_trim_token = trim_token, cpp_tokenization_function = split_string, cpp_string_separator = split_separator, cpp_remove_stopwords = remove_stopwords,

                           min_num_char = min_num_char, stemmer = stemmer, min_n_gram = min_n_gram, max_n_gram = max_n_gram, skip_n_gram = skip_n_gram, skip_distance = skip_distance,

                           n_gram_delimiter = n_gram_delimiter, concat_delimiter = concat_delimiter, path_2file = path_2folder, stemmer_ngram = 4, stemmer_gamma = 0.0,                         # add constant values for stemming other than porter_2stemmer [ n-gram stemming applies to a whole corpus and not to single sub-vectors of documents ]

                           stemmer_truncate = 3, stemmer_batches = 1, threads = threads, verbose = verbose, vocabulary_path = vocabulary_path_file )
  }

  gc();

  if (path_2folder != "") {

    res = paste(c("the data is saved in FOLDER : '", path_2folder, "' and FILE : 'output_token_single_file.txt'"), collapse = '')
  }

  token_structure = structure(list(token = res), class = "tokenization and transformation")

  if (verbose) {

    end = Sys.time()

    t = end - start

    cat('\n'); cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');
  }

  return(token_structure)
}




#' utf-locale for the available languages
#'
#'
#' @param language a character string specifying the language for which the utf-locale should be returned
#' @return a utf locale
#' @importFrom utils read.csv
#' @details
#'
#' This is a limited list of language-locale. The locale depends mostly on the text input.
#'
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' utf_locale(language = "english")


utf_locale = function(language = "english") {

  if (!inherits(language, 'character')) stop("the 'language' parameter should be a character string")
  if (!language %in% c("afrikaans", "arabic", "armenian", "basque", "bengali", "breton",
                       "bulgarian", "catalan", "croatian", "czech", "danish",
                       "dutch", "english", "estonian", "finnish", "french",
                       "galician", "german", "greek", "hausa", "hebrew", "hindi", "hungarian",
                       "indonesian", "irish", "italian", "latvian", "marathi", "norwegian",
                       "persian", "polish", "portuguese", "romanian", "russian", "slovak",
                       "slovenian", "somalia", "spanish", "swahili", "swedish", "turkish", "yoruba", "zulu"))
    stop("available languages are 'afrikaans', 'arabic', 'armenian',
        'basque', 'bengali', 'breton', 'bulgarian', 'catalan', 'croatian', 'czech',
        'danish', 'dutch', 'english', 'estonian', 'finnish', 'french', 'galician',
        'german', 'greek', 'hausa', 'hebrew', 'hindi', 'hungarian', 'indonesian', 'irish',
        'italian', 'latvian', 'marathi', 'norwegian', 'persian',
        'polish', 'portuguese', 'romanian', 'russian', 'slovak', 'slovenian', 'somalia',
        'spanish', 'swahili', 'swedish', 'turkish', 'yoruba', 'zulu'")

  locale_file_path = system.file("locale", "locale_stopword_encoding.csv", package = "textTinyR")

  locale = utils::read.csv(locale_file_path, header = T, stringsAsFactors = F)

  lst = list()

  for (i in 1:nrow(locale)) {

    lst[[locale[i, 1]]] = locale[i, 2]
  }

  return(lst[[language]])
}





#' String tokenization and transformation for big data sets
#'
#'
#' @param start_query a character string. The \emph{start_query} is the first word of the subset of the data and should appear frequently at the beginning of each line int the text file.
#' @param end_query a character string. The \emph{end_query} is the last word of the subset of the data and should appear frequently at the end of each line in the text file.
#' @param min_lines a numeric value specifying the minimum number of lines. For instance if min_lines = 2, then only subsets of text with more than 1 lines will be kept.
#' @param trimmed_line either TRUE or FALSE. If FALSE then each line of the text file will be trimmed both sides before applying the start_query and end_query
#' @param verbose either TRUE or FALSE. If TRUE then information will be printed in the console
#' @param input_path_folder a character string specifying the folder where the input files are saved
#' @param output_path_folder a character string specifying the folder where the output files should be saved
#' @param input_path_file a character string specifying the path to the input file
#' @param batches a numeric value specifying the number of batches to use. The batches will be used to split the initial data into subsets. Those subsets will be either saved in files (\emph{big_text_splitter} function) or will be used internally for low memory processing (\emph{big_text_tokenizer} function).
#' @param read_file_delimiter the delimiter to use when the input file will be red (for instance a tab-delimiter or a new-line delimiter).
#' @param to_lower either TRUE or FALSE. If TRUE the character string will be converted to lower case
#' @param remove_char a character string with specific characters that should be removed from the text file. If the \emph{remove_char} is "" then no removal of characters take place
#' @param to_upper either TRUE or FALSE. If TRUE the character string will be converted to upper case
#' @param utf_locale the language specific locale to use in case that either the \emph{to_lower} or the \emph{to_upper} parameter is TRUE and the text file language is other than english. For instance if the language of a text file is greek then the \emph{utf_locale} parameter should be \emph{'el_GR.UTF-8'} ( \emph{language_country.encoding} ). A wrong utf-locale does not raise an error, however the runtime of the function increases.
#' @param remove_punctuation_string either TRUE or FALSE. If TRUE then the punctuation of the character string will be removed (applies before the split function)
#' @param remove_punctuation_vector either TRUE or FALSE. If TRUE then the punctuation of the vector of the character strings will be removed  (after the string split has taken place)
#' @param remove_numbers either TRUE or FALSE. If TRUE then any numbers in the character string will be removed
#' @param trim_token either TRUE or FALSE. If TRUE then the string will be trimmed (left and/or right)
#' @param split_string either TRUE or FALSE. If TRUE then the character string will be split using the \emph{split_separator} as delimiter. The user can also specify multiple delimiters.
#' @param split_separator a character string specifying the character delimiter(s)
#' @param remove_stopwords either TRUE, FALSE or a character vector of user defined stop words. If TRUE then by using the \emph{language} parameter the corresponding stop words vector will be uploaded.
#' @param language a character string which defaults to english. If the \emph{remove_stopwords} parameter is TRUE then the corresponding stop words vector will be uploaded. Available languages
#' are \emph{afrikaans}, \emph{arabic}, \emph{armenian}, \emph{basque}, \emph{bengali}, \emph{breton}, \emph{bulgarian}, \emph{catalan},
#' \emph{croatian}, \emph{czech}, \emph{danish}, \emph{dutch}, \emph{english}, \emph{estonian},
#' \emph{finnish}, \emph{french}, \emph{galician}, \emph{german}, \emph{greek}, \emph{hausa}, \emph{hebrew}, \emph{hindi}, \emph{hungarian},
#' \emph{indonesian}, \emph{irish}, \emph{italian}, \emph{latvian}, \emph{marathi},
#' \emph{norwegian}, \emph{persian}, \emph{polish}, \emph{portuguese}, \emph{romanian}, \emph{russian}, \emph{slovak}, \emph{slovenian},
#' \emph{somalia}, \emph{spanish}, \emph{swahili}, \emph{swedish}, \emph{turkish}, \emph{yoruba}, \emph{zulu}
#' @param min_num_char an integer specifying the minimum number of characters to keep. If the \emph{min_num_char} is greater than 1 then character strings with more than 1 characters will be returned
#' @param max_num_char an integer specifying the maximum number of characters to keep. The \emph{max_num_char} should be less than or equal to \emph{Inf} (in this function the Inf value translates to a word-length of 1000000000)
#' @param stemmer a character string specifying the stemming method. One of the following \emph{porter2_stemmer}, \emph{ngram_sequential}, \emph{ngram_overlap}. See details for more information.
#' @param min_n_gram an integer specifying the minimum number of n-grams. The minimum number of min_n_gram is 1.
#' @param max_n_gram an integer specifying the maximum number of n-grams. The minimum number of max_n_gram is 1.
#' @param skip_n_gram an integer specifying the number of skip-n-grams. The minimum number of skip_n_gram is 1. The skip_n_gram gives the (max.) n-grams using the \emph{skip_distance} parameter. If \emph{skip_n_gram} is greater than 1 then both \emph{min_n_gram} and \emph{max_n_gram} should be set to 1.
#' @param skip_distance an integer specifying the skip distance between the words. The minimum value for the skip distance is 0, in which case simple n-grams will be returned.
#' @param n_gram_delimiter a character string specifying the n-gram delimiter (applies to both n-gram and skip-n-gram cases)
#' @param concat_delimiter either NULL or a character string specifying the delimiter to use in order to concatenate the end-vector of character strings to a single character string (recommended in case that the end-vector should be saved to a file)
#' @param path_2folder a character string specifying the path to the folder where the file(s) will be saved
#' @param stemmer_ngram a numeric value greater than 1. Applies to both \emph{ngram_sequential} and \emph{ngram_overlap} methods. In case of \emph{ngram_sequential} the first stemmer_ngram characters will be picked, whereas in the case of \emph{ngram_overlap} the overlapping stemmer_ngram characters will be build.
#' @param stemmer_gamma a float number greater or equal to 0.0. Applies only to \emph{ngram_sequential}. Is a threshold value, which defines how much frequency deviation of two N-grams is acceptable. It is kept either zero or to a minimum value.
#' @param stemmer_truncate a numeric value greater than 0. Applies only to \emph{ngram_sequential}. The ngram_sequential is modified to use relative frequencies (float numbers between 0.0 and 1.0 for the ngrams of a specific word in the corpus) and the stemmer_truncate parameter controls the number of rounding digits for the ngrams of the word. The main purpose was to give the same relative frequency to words appearing approximately the same on the corpus.
#' @param stemmer_batches a numeric value greater than 0. Applies only to \emph{ngram_sequential}. Splits the corpus into batches with the option to run the batches in multiple threads.
#' @param threads an integer specifying the number of cores to run in parallel
#' @param save_2single_file either TRUE or FALSE. If TRUE then the output data will be saved in a single file. Otherwise the data will be saved in multiple files with incremented enumeration
#' @param increment_batch_nr a numeric value. The enumeration of the output files will start from the \emph{increment_batch_nr}. If the \emph{save_2single_file} parameter is TRUE then the \emph{increment_batch_nr} parameter won't be taken into consideration.
#' @param vocabulary_path_file either NULL or a character string specifying the output file where the vocabulary should be saved (after tokenization and transformation is applied). Applies to the \emph{vocabulary_accumulator} method.
#' @param vocabulary_path_folder either NULL or a character string specifying the output folder where the vocabulary batches should be saved (after tokenization and transformation is applied). Applies to the \emph{big_text_tokenizer} method.
#' @param max_num_chars a numeric value to limit the words of the output vocabulary to a maximum number of characters (applies to the \emph{vocabulary_accumulator} function)
#' @export
#' @details
#'
#' the \emph{big_text_splitter} function splits a text file into sub-text-files using either the batches parameter (big-text-splitter-bytes) or both the batches and the end_query parameter (big-text-splitter-query). The end_query parameter (if not NULL) should be a character string specifying a word that appears repeatedly at the end of each line in the text file.
#'
#' the \emph{big_text_parser} function parses text files from an input folder and saves those processed files to an output folder. The \emph{big_text_parser} is appropriate for files with a structure using the start- and end- query parameters.
#'
#' the \emph{big_text_tokenizer} function tokenizes and transforms the text files of a folder and saves those files to either a folder or a single file. There is also the option to save a frequency vocabulary of those transformed tokens to a file.
#'
#' the \emph{vocabulary_accumulator} function takes the resulted vocabulary files of the \emph{big_text_tokenizer} and returns the vocabulary sums sorted in decreasing order. The parameter \emph{max_num_chars} limits the number of the corpus using the number of characters of each word.
#'
#' The \emph{ngram_sequential} or \emph{ngram_overlap} stemming method applies to each single batch and not to the whole corpus of the text file. Thus, it is possible that the stems of the same words for randomly selected batches might differ.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @section Methods:
#'
#' \describe{
#'  \item{\code{big_tokenize_transform$new(verbose = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{big_text_splitter(input_path_file = NULL, output_path_folder = NULL, end_query = NULL, batches = NULL, trimmed_line = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{big_text_parser(input_path_folder = NULL, output_path_folder = NULL, start_query = NULL, end_query = NULL, min_lines = 1, trimmed_line = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{big_text_tokenizer(input_path_folder = NULL, batches = NULL, read_file_delimiter = " ", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " .,;:()?!", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE, increment_batch_nr = 1, vocabulary_path_folder = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{vocabulary_accumulator(input_path_folder = NULL, vocabulary_path_file = NULL, max_num_chars = 100)}}{}
#'  }
#'
#' @usage # utl <- big_tokenize_transform$new(verbose = FALSE)
#' @examples
#'
#' library(textTinyR)
#'
#'
#' # fs <- big_tokenize_transform$new(verbose = FALSE)
#'
#' #---------------
#' # file splitter:
#' #---------------
#'
#' # fs$big_text_splitter(input_path_file = "input.txt",
#'
#' #                      output_path_folder = "/folder/output/",
#'
#' #                      end_query = "endword", batches = 5,
#'
#' #                      trimmed_line = FALSE)
#'
#'
#' #-------------
#' # file parser:
#' #-------------
#'
#' # fs$big_text_parser(input_path_folder = "/folder/output/",
#'
#' #                    output_path_folder = "/folder/parser/",
#'
#' #                    start_query = "startword", end_query = "endword",
#'
#' #                    min_lines = 1, trimmed_line = TRUE)
#'
#'
#' #----------------
#' # file tokenizer:
#' #----------------
#'
#'
#' # fs$big_text_tokenizer(input_path_folder = "/folder/parser/",
#'
#' #                       batches = 5, split_string=TRUE,
#'
#' #                       to_lower = TRUE, trim_token = TRUE,
#'
#' #                       max_num_char = 100, remove_stopwords = TRUE,
#'
#' #                       stemmer = "porter2_stemmer", threads = 1,
#'
#' #                       path_2folder="/folder/output_token/",
#'
#' #                       vocabulary_path_folder="/folder/VOCAB/")
#'
#' #-------------------
#' # vocabulary counts:
#' #-------------------
#'
#'
#' # fs$vocabulary_accumulator(input_path_folder = "/folder/VOCAB/",
#'
#' #                           vocabulary_path_file = "/folder/vocab.txt",
#'
#' #                           max_num_chars = 50)




big_tokenize_transform <- R6::R6Class("big_tokenize_transform",

                                      public = list(

                                        verbose = FALSE,


                                       #----------------
                                       # initialization
                                       #----------------

                                        initialize = function(verbose = FALSE) {

                                          self$verbose <- verbose

                                          if (!is.logical(self$verbose)) stop("the verbose parameter should be either TRUE or FALSE")
                                        },


                                       #---------------------------------------------------------------------------------------------
                                       # text FILE splitter for big data sets (is used independently if the fie has structure or not)
                                       #---------------------------------------------------------------------------------------------


                                       big_text_splitter = function(input_path_file = NULL, output_path_folder = NULL, end_query = NULL, batches = NULL, trimmed_line = FALSE) {

                                         try_err_file_input = inherits(tryCatch(normalizePath(input_path_file, mustWork = T), error = function(e) e), "error")
                                         if (try_err_file_input) stop("the input_path_file parameter should be a non-null valid path to a file")
                                         if (is.null(output_path_folder)) stop("the output_path_folder parameter should be a non-null valid path to a folder")
                                         if (!inherits(output_path_folder, "character")) stop("the output_path_folder parameter should be a valid ( character string ) path to a folder")
                                         if (!is.null(output_path_folder)) {
                                           str_SPL = strsplit(output_path_folder, "")[[1]]
                                           if (!str_SPL[nchar(output_path_folder)] %in% c("/", "\\")) stop('the output_path_folder path should end in slash')}
                                         if (!inherits(batches, c("numeric", "integer"))) stop("the batches parameter should be a numeric value")
                                         if (batches < 2) stop("the batches parameter should be at least 2")
                                         if (!is.logical(trimmed_line)) stop("the trimmed_line parameter should be either TRUE or FALSE")
                                         if (!is.null(end_query)) {
                                           if (!is.character(end_query)) stop("the end_query parameter should be a character string")}
                                         if (is.null(end_query)) end_query = "NULL"

                                         bts = big_splitter_bytes(input_path_file, batches, end_query, output_path_folder, trimmed_line, self$verbose)

                                         gc()

                                         invisible()
                                       },


                                       #-------------------------------------------------------------------------------------------------------------------------
                                       # text FOLDER parser for big data sets (is used if the files in the folder have a structure using a start- and end- query)
                                       #-------------------------------------------------------------------------------------------------------------------------


                                       big_text_parser = function(input_path_folder = NULL, output_path_folder = NULL, start_query = NULL, end_query = NULL, min_lines = 1, trimmed_line = FALSE) {

                                         if (is.null(input_path_folder)) stop("the input_path_folder parameter should be a non-null valid path to a folder")
                                         if (!inherits(input_path_folder, "character")) stop("the input_path_folder parameter should be a ( character string ) valid path to a folder")
                                         if (!is.null(input_path_folder)) {
                                           str_SPL = strsplit(input_path_folder, "")[[1]]
                                           if (!str_SPL[nchar(input_path_folder)] %in% c("/", "\\")) stop('the input_path_folder path should end in slash')}
                                         if (is.null(output_path_folder)) stop("the output_path_folder parameter should be a non-null valid path to a folder")
                                         if (!inherits(output_path_folder, "character")) stop("the output_path_folder parameter should be a ( character string ) valid path to a folder")
                                         if (!is.null(output_path_folder)) {
                                           str_SPL = strsplit(output_path_folder, "")[[1]]
                                           if (!str_SPL[nchar(output_path_folder)] %in% c("/", "\\")) stop('the output_path_folder path should end in slash')}
                                         if (!is.logical(trimmed_line)) stop("the trimmed_line parameter should be either TRUE or FALSE")
                                         if (!is.character(start_query)) stop("the start_query parameter should be a character string")
                                         if (!is.character(end_query)) stop("the end_query parameter should be a character string")
                                         if (min_lines < 1) stop("the min_lines parameter should a numeric value greater than 0")

                                         btp = big_parser(input_path_folder, start_query, end_query, output_path_folder, min_lines, trimmed_line, self$verbose)

                                         gc()

                                         invisible()
                                       },



                                       #----------------------------------------
                                       # text folder tokenizer for big data sets
                                       #----------------------------------------


                                       big_text_tokenizer = function(input_path_folder = NULL, batches = NULL, read_file_delimiter = "\n", to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "",

                                                                     remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE,

                                                                     split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL,

                                                                     min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "",

                                                                     stemmer_ngram = 4, stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, save_2single_file = FALSE,

                                                                     increment_batch_nr = 1, vocabulary_path_folder = NULL) {


                                         if (is.null(input_path_folder)) stop("the input_path_folder parameter should be a non-null valid path to a folder")
                                         if (!inherits(input_path_folder, "character")) stop("the input_path_folder parameter should be a ( character string ) valid path to a folder")
                                         if (!is.null(input_path_folder)) {
                                           str_SPL = strsplit(input_path_folder, "")[[1]]
                                           if (!str_SPL[nchar(input_path_folder)] %in% c("/", "\\")) stop('the input_path_folder path should end in slash')}
                                         if (!inherits(batches, c("numeric", "integer"))) stop("the batches parameter should be a numeric value")
                                         if (batches < 2) stop("the batches parameter should be at least 2")
                                         if (!inherits(read_file_delimiter, 'character')) stop("the read_file_delimiter parameter should be a character string")
                                         if (is.null(path_2folder)) stop("the path_2folder parameter should be a non-null valid path to a folder")
                                         if (!inherits(path_2folder, "character")) stop("the path_2folder parameter should be a ( character string ) valid path to a folder")
                                         if (path_2folder == "") stop("the path_2folder parameter should be a valid path to a folder where the output file(s) will be saved")
                                         if (path_2folder != "") {
                                           str_SPL = strsplit(path_2folder, "")[[1]]
                                           if (!str_SPL[nchar(path_2folder)] %in% c("/", "\\")) stop('the path_2folder path should end in slash')}
                                         if (!is.logical(to_lower)) stop("the to_lower parameter should be either TRUE or FALSE")
                                         if (!is.logical(to_upper)) stop("the to_upper parameter should be either TRUE or FALSE")
                                         if (!is.character(remove_char)) stop("the remove_char parameter should be a character string")
                                         if (!inherits(utf_locale, 'character')) stop("the 'utf_locale' parameter should be a character string")
                                         if (!is.logical(remove_punctuation_string)) stop("the remove_punctuation_string parameter should be either TRUE or FALSE")
                                         if (!is.logical(remove_punctuation_vector)) stop("the remove_punctuation_vector parameter should be either TRUE or FALSE")
                                         if (!is.logical(remove_numbers)) stop("the remove_numbers parameter should be either TRUE or FALSE")
                                         if (!is.logical(trim_token)) stop("the trim_token parameter should be either TRUE or FALSE")
                                         if (!is.logical(split_string)) stop("the split_string parameter should be either TRUE or FALSE")
                                         if (!inherits(split_separator, 'character')) stop("the split_separator should be a character string consisting of one or more delimiters")
                                         if (!inherits(remove_stopwords, c('character', 'vector', 'logical'))) {
                                           stop("the 'remove_stopwords' parameter should be either TRUE, FALSE or a character vector of stop words")}
                                         if (!language %in% c("afrikaans", "arabic", "armenian", "basque", "bengali", "breton",
                                                              "bulgarian", "catalan", "croatian", "czech", "danish",
                                                              "dutch", "english", "estonian", "finnish", "french",
                                                              "galician", "german", "greek", "hausa", "hebrew", "hindi", "hungarian",
                                                              "indonesian", "irish", "italian", "latvian", "marathi", "norwegian",
                                                              "persian", "polish", "portuguese", "romanian", "russian", "slovak",
                                                              "slovenian", "somalia", "spanish", "swahili", "swedish", "turkish", "yoruba", "zulu"))
                                           stop("available languages in case of stop-word removal are 'afrikaans', 'arabic', 'armenian',
                                                'basque', 'bengali', 'breton', 'bulgarian', 'catalan', 'croatian', 'czech',
                                                'danish', 'dutch', 'english', 'estonian', 'finnish', 'french', 'galician',
                                                'german', 'greek', 'hausa', 'hebrew', 'hindi', 'hungarian', 'indonesian', 'irish',
                                                'italian', 'latvian', 'marathi', 'norwegian', 'persian', 'polish', 'portuguese',
                                                'romanian', 'russian', 'slovak', 'slovenian', 'somalia', 'spanish', 'swahili',
                                                'swedish', 'turkish', 'yoruba', 'zulu'.

                                                A user defined character vector of stop words can be used as input, if the target language is not included")
                                         if (min_num_char < 1) stop("the min_num_char parameter should be greater than 0")
                                         if (min_num_char >= max_num_char) stop("the max_num_char parameter should be greater than the min_num_char")
                                         if (max_num_char == Inf) max_num_char = 1000000000
                                         if (!is.null(stemmer)) {
                                           if (!stemmer %in% c("porter2_stemmer", "ngram_sequential", "ngram_overlap")) stop("valid stemming methods are porter2_stemmer, ngram_sequential or ngram_overlap")
                                           if (stemmer == "ngram_sequential") {
                                             if (stemmer_ngram < 1) stop("the minimum value for the stemmer_ngram parameter should be 1")
                                             if (stemmer_gamma < 0.0) stop("the minimum value for the stemmer_gamma parameter should be 0.0")
                                             if (stemmer_truncate < 1) stop("the minimum value for the stemmer_truncate parameter should be 1")
                                             if (stemmer_batches < 1) stop("the minimum value for the stemmer_batches parameter should be 1")
                                           }
                                           if (stemmer == "ngram_overlap") {
                                             if (stemmer_ngram < 1) stop("the minimum value for the stemmer_ngram parameter should be 1")
                                           }
                                         }
                                         if (is.null(stemmer)) stemmer = "NULL"
                                         if (min_n_gram < 1) stop("the min_n_gram parameter should be greater than 0")
                                         if (max_n_gram < 1) stop("the max_n_gram parameter should be greater than 0")
                                         if (skip_n_gram < 1) stop("the skip_n_gram parameter should be greater than 0")
                                         if (skip_distance < 0) stop("the skip_distance parameter should be greater or equal to 0")
                                         if (min_n_gram > max_n_gram) stop("the min_n_gram parameter should be less than or equal to the max_n_gram parameter")
                                         if (!is.character(n_gram_delimiter)) stop("the n_gram_delimiter parameter should be a character string")
                                         if (threads < 1) stop("the number of threads should be at least 1")
                                         if (!is.logical(save_2single_file)) stop("the save_2single_file parameter should be either TRUE or FALSE")
                                         if (!is.null(concat_delimiter)) {
                                           if (!inherits(concat_delimiter, 'character')) {
                                             stop("the concat_delimiter parameter should be a character string")}}
                                         if (is.null(concat_delimiter)) concat_delimiter = "NULL"
                                         if (increment_batch_nr < 0) stop("the increment_batch_nr parameter should be a number greater or equal to 0")
                                         if (!is.null(vocabulary_path_folder)) {
                                           if (!inherits(vocabulary_path_folder, 'character')) stop("the vocabulary_path_folder parameter should be a character string specifying a valid path to a folder")
                                           str_SPL = strsplit(vocabulary_path_folder, "")[[1]]
                                           if (!str_SPL[nchar(vocabulary_path_folder)] %in% c("/", "\\")) stop('the vocabulary_path_folder parameter should end in slash')}
                                         if (is.null(vocabulary_path_folder)) vocabulary_path_folder = ""
                                         if ((to_lower || to_upper) && language != "english") {          # THIS IS NOT THE ONLY EXCEPTION: in case of a user-defined stop-words list if the language is other than english and the default language stays the same ('english'), then the output is incorrect
                                           warning("if the 'language' parameter is not english and either a 'to_lower' or a 'to_upper' conversion takes place consider changing the 'utf_locale' parameter", call. = F)
                                         }

                                         if (is.logical(remove_stopwords)) {

                                           if (remove_stopwords) {

                                             language_path = system.file("stopwords", paste0(language, ".txt"), package = "textTinyR")

                                             language_stop_words = read.table(language_path, quote = "\"", comment.char = "", stringsAsFactors = F)

                                             language_stop_words[nrow(language_stop_words) + 1, ] = ""                  # add the empty character to the stopwords

                                             language_stop_words = as.vector(language_stop_words[, 1])}

                                           else {

                                             language_stop_words = character(0)
                                           }
                                         }

                                         if (inherits(remove_stopwords, c('character', 'vector'))) {

                                           language_stop_words = remove_stopwords

                                           remove_stopwords = T
                                         }

                                         gc();

                                         btk = big_tokenize(input_path_folder, path_2folder, batches, language_stop_words, language, utf_locale, read_file_delimiter, max_num_char, increment_batch_nr, remove_char,

                                                            to_lower, to_upper, remove_punctuation_string, remove_punctuation_vector, remove_numbers, trim_token, split_string, split_separator, remove_stopwords,

                                                            min_num_char, stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, concat_delimiter, stemmer_ngram, stemmer_gamma,

                                                            stemmer_truncate, stemmer_batches, threads, save_2single_file, vocabulary_path_folder, self$verbose)

                                         gc();

                                         invisible()
                                       },



                                       #-----------------------------------------
                                       # vocabulary counts from a folder of files
                                       #-----------------------------------------


                                       vocabulary_accumulator = function(input_path_folder = NULL, vocabulary_path_file = NULL, max_num_chars = 100) {

                                         if (is.null(input_path_folder)) stop("the input_path_folder parameter should be a non-null valid path to a folder")
                                         if (!inherits(input_path_folder, "character")) stop("the input_path_folder parameter should be a ( character string ) valid path to a folder")
                                         if (!is.null(input_path_folder)) {
                                           str_SPL = strsplit(input_path_folder, "")[[1]]
                                           if (!str_SPL[nchar(input_path_folder)] %in% c("/", "\\")) stop('the input_path_folder path should end in slash')}
                                         if (is.null(vocabulary_path_file)) stop("the vocabulary_path_file parameter should be a non-null valid path to a file")
                                         if (!inherits(vocabulary_path_file, 'character')) stop("the vocabulary_path_file parameter should be a character string specifying a valid path to a file")
                                         if (max_num_chars < 1) stop("the max_num_chars parameter should be greater than 0")


                                         btv = vocabulary_counts_big_tokenize(input_path_folder, vocabulary_path_file, max_num_chars, self$verbose)

                                         gc()

                                         invisible()
                                       }
                                      )
)




#' returns the vocabulary counts for small or medium ( xml and not only ) files
#'
#'
#'
#' @param input_path_file a character string specifying a valid path to the input file
#' @param start_query a character string. The \emph{start_query} is the first word of the subset of the data and should appear frequently at the beginning of each line in the text file.
#' @param end_query a character string. The \emph{end_query} is the last word of the subset of the data and should appear frequently at the end of each line in the text file.
#' @param vocabulary_path_file a character string specifying the output file where the vocabulary should be saved (after tokenization and transformation is applied).
#' @param min_lines a numeric value specifying the minimum number of lines. For instance if min_lines = 2, then only subsets of text with more than 1 lines will be kept.
#' @param trimmed_line either TRUE or FALSE. If FALSE then each line of the text file will be trimmed both sides before applying the start_query and end_query
#' @param to_lower either TRUE or FALSE. If TRUE the character string will be converted to lower case
#' @param to_upper either TRUE or FALSE. If TRUE the character string will be converted to upper case
#' @param utf_locale the language specific locale to use in case that either the \emph{to_lower} or the \emph{to_upper} parameter is TRUE and the text file language is other than english. For instance if the language of a text file is greek then the \emph{utf_locale} parameter should be \emph{'el_GR.UTF-8'} ( \emph{language_country.encoding} ). A wrong utf-locale does not raise an error, however the runtime of the function increases.
#' @param max_num_char an integer specifying the maximum number of characters to keep. The \emph{max_num_char} should be less than or equal to \emph{Inf} (in this function the Inf value translates to a word-length of 1000000000)
#' @param remove_char a character string with specific characters that should be removed from the text file. If the \emph{remove_char} is "" then no removal of characters take place
#' @param remove_punctuation_string either TRUE or FALSE. If TRUE then the punctuation of the character string will be removed (applies before the split function)
#' @param remove_punctuation_vector either TRUE or FALSE. If TRUE then the punctuation of the vector of the character strings will be removed  (after the string split has taken place)
#' @param remove_numbers either TRUE or FALSE. If TRUE then any numbers in the character string will be removed
#' @param trim_token either TRUE or FALSE. If TRUE then the string will be trimmed (left and/or right)
#' @param split_string either TRUE or FALSE. If TRUE then the character string will be split using the \emph{split_separator} as delimiter. The user can also specify multiple delimiters.
#' @param split_separator a character string specifying the character delimiter(s)
#' @param remove_stopwords either TRUE, FALSE or a character vector of user defined stop words. If TRUE then by using the \emph{language} parameter the corresponding stop words vector will be uploaded.
#' @param language a character string which defaults to english. If the \emph{remove_stopwords} parameter is TRUE then the corresponding stop words vector will be uploaded. Available languages
#' are \emph{afrikaans}, \emph{arabic}, \emph{armenian}, \emph{basque}, \emph{bengali}, \emph{breton}, \emph{bulgarian}, \emph{catalan},
#' \emph{croatian}, \emph{czech}, \emph{danish}, \emph{dutch}, \emph{english}, \emph{estonian},
#' \emph{finnish}, \emph{french}, \emph{galician}, \emph{german}, \emph{greek}, \emph{hausa}, \emph{hebrew}, \emph{hindi}, \emph{hungarian},
#' \emph{indonesian}, \emph{irish}, \emph{italian}, \emph{latvian}, \emph{marathi},
#' \emph{norwegian}, \emph{persian}, \emph{polish}, \emph{portuguese}, \emph{romanian}, \emph{russian}, \emph{slovak}, \emph{slovenian},
#' \emph{somalia}, \emph{spanish}, \emph{swahili}, \emph{swedish}, \emph{turkish}, \emph{yoruba}, \emph{zulu}
#' @param min_num_char an integer specifying the minimum number of characters to keep. If the \emph{min_num_char} is greater than 1 then character strings with more than 1 characters will be returned
#' @param stemmer a character string specifying the stemming method. One of the following \emph{porter2_stemmer}, \emph{ngram_sequential}, \emph{ngram_overlap}
#' @param min_n_gram an integer specifying the minimum number of n-grams. The minimum number of min_n_gram is 1.
#' @param max_n_gram an integer specifying the maximum number of n-grams. The minimum number of max_n_gram is 1.
#' @param skip_n_gram an integer specifying the number of skip-n-grams. The minimum number of skip_n_gram is 1. The skip_n_gram gives the (max.) n-grams using the \emph{skip_distance} parameter. If \emph{skip_n_gram} is greater than 1 then both \emph{min_n_gram} and \emph{max_n_gram} should be set to 1.
#' @param skip_distance an integer specifying the skip distance between the words. The minimum value for the skip distance is 0, in which case simple n-grams will be returned.
#' @param n_gram_delimiter a character string specifying the n-gram delimiter (applies to both n-gram and skip-n-gram cases)
#' @param stemmer_ngram a numeric value greater than 1. Applies to both \emph{ngram_sequential} and \emph{ngram_overlap} methods. In case of \emph{ngram_sequential} the first stemmer_ngram characters will be picked, whereas in the case of \emph{ngram_overlap} the overlapping stemmer_ngram characters will be build.
#' @param stemmer_gamma a float number greater or equal to 0.0. Applies only to \emph{ngram_sequential}. Is a threshold value, which defines how much frequency deviation of two N-grams is acceptable. It is kept either zero or to a minimum value.
#' @param stemmer_truncate a numeric value greater than 0. Applies only to \emph{ngram_sequential}. The ngram_sequential is modified to use relative frequencies (float numbers between 0.0 and 1.0 for the ngrams of a specific word in the corpus) and the stemmer_truncate parameter controls the number of rounding digits for the ngrams of the word. The main purpose was to give the same relative frequency to words appearing approximately the same on the corpus.
#' @param stemmer_batches a numeric value greater than 0. Applies only to \emph{ngram_sequential}. Splits the corpus into batches with the option to run the batches in multiple threads.
#' @param threads an integer specifying the number of cores to run in parallel
#' @param verbose either TRUE or FALSE. If TRUE then information will be printed in the console
#' @export
#' @importFrom utils read.table
#' @details
#' The text file should have a structure (such as an xml-structure), so that subsets can be extracted using the \emph{start_query} and \emph{end_query} parameters
#'
#' For big files the \emph{vocabulary_accumulator} method of the \emph{big_tokenize_transform} class is appropriate
#' @examples
#'
#' library(textTinyR)
#'
#' # vps = vocabulary_parser(input_path_file = '/folder/input_data.txt',
#'
#' #                         start_query = 'start_word', end_query = 'end_word',
#'
#' #                         vocabulary_path_file = '/folder/vocab.txt',
#'
#' #                         to_lower = TRUE, split_string = TRUE)


vocabulary_parser = function(input_path_file = NULL, start_query = NULL, end_query = NULL, vocabulary_path_file = NULL, min_lines = 1, trimmed_line = FALSE, to_lower = FALSE,

                             to_upper = FALSE, utf_locale = "", max_num_char = Inf, remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                             remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                             min_num_char = 1, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", stemmer_ngram = 4,

                             stemmer_gamma = 0.0, stemmer_truncate = 3, stemmer_batches = 1, threads = 1, verbose = FALSE) {


  try_err_file_input = inherits(tryCatch(normalizePath(input_path_file, mustWork = T), error = function(e) e), "error")
  if (try_err_file_input) stop("the input_path_file parameter should be a non-null valid path to a file")
  if (!is.character(start_query)) stop("the start_query parameter should be a character string")
  if (!is.character(end_query)) stop("the end_query parameter should be a character string")
  if (is.null(vocabulary_path_file)) stop("the vocabulary_path_file parameter should be a non-null valid path to a file")
  if (!inherits(vocabulary_path_file, 'character')) stop("the vocabulary_path_file parameter should a character string specifying a valid path to a file")
  if (min_lines < 1) stop("the min_lines parameter should a numeric value greater than 0")
  if (!is.logical(trimmed_line)) stop("the trimmed_line parameter should be either TRUE or FALSE")
  if (!is.logical(to_lower)) stop("the to_lower parameter should be either TRUE or FALSE")
  if (!is.character(remove_char)) stop("the remove_char parameter should be a character string")
  if (!is.logical(to_upper)) stop("the to_upper parameter should be either TRUE or FALSE")
  if (!inherits(utf_locale, 'character')) stop("the 'utf_locale' parameter should be a character string")
  if (!is.logical(remove_punctuation_string)) stop("the remove_punctuation_string parameter should be either TRUE or FALSE")
  if (!is.logical(remove_punctuation_vector)) stop("the remove_punctuation_vector parameter should be either TRUE or FALSE")
  if (!is.logical(remove_numbers)) stop("the remove_numbers parameter should be either TRUE or FALSE")
  if (!is.logical(trim_token)) stop("the trim_token parameter should be either TRUE or FALSE")
  if (!is.logical(split_string)) stop("the split_string parameter should be either TRUE or FALSE")
  if (!inherits(split_separator, 'character')) stop("the split_separator should be a character string consisting of one or more delimiters")
  if (!inherits(remove_stopwords, c('character', 'vector', 'logical'))) {
    stop("the 'remove_stopwords' parameter should be either TRUE, FALSE or a character vector of stop words")}
  if (!language %in% c("afrikaans", "arabic", "armenian", "basque", "bengali", "breton",
                       "bulgarian", "catalan", "croatian", "czech", "danish",
                       "dutch", "english", "estonian", "finnish", "french",
                       "galician", "german", "greek", "hausa", "hebrew", "hindi", "hungarian",
                       "indonesian", "irish", "italian", "latvian", "marathi", "norwegian",
                       "persian", "polish", "portuguese", "romanian", "russian", "slovak",
                       "slovenian", "somalia", "spanish", "swahili", "swedish", "turkish", "yoruba", "zulu"))
    stop("available languages in case of stop-word removal are 'afrikaans', 'arabic', 'armenian',
                                                'basque', 'bengali', 'breton', 'bulgarian', 'catalan', 'croatian', 'czech',
                                                'danish', 'dutch', 'english', 'estonian', 'finnish', 'french', 'galician',
                                                'german', 'greek', 'hausa', 'hebrew', 'hindi', 'hungarian', 'indonesian', 'irish',
                                                'italian', 'latvian', 'marathi', 'norwegian', 'persian', 'polish', 'portuguese',
                                                'romanian', 'russian', 'slovak', 'slovenian', 'somalia', 'spanish', 'swahili',
                                                'swedish', 'turkish', 'yoruba', 'zulu'.

                                                A user defined character vector of stop words can be used as input, if the target language is not included")
  if (min_num_char < 1) stop("the min_num_char parameter should be greater than 0")
  if (min_num_char >= max_num_char) stop("the max_num_char parameter should be greater than the min_num_char")
  if (max_num_char == Inf) max_num_char = 1000000000
  if (!is.null(stemmer)) {
    if (!stemmer %in% c("porter2_stemmer", "ngram_sequential", "ngram_overlap")) stop("valid stemming methods are porter2_stemmer, ngram_sequential or ngram_overlap")
    if (stemmer == "ngram_sequential") {
      if (stemmer_ngram < 1) stop("the minimum value for the stemmer_ngram parameter should be 1")
      if (stemmer_gamma < 0.0) stop("the minimum value for the stemmer_gamma parameter should be 0.0")
      if (stemmer_truncate < 1) stop("the minimum value for the stemmer_truncate parameter should be 1")
      if (stemmer_batches < 1) stop("the minimum value for the stemmer_batches parameter should be 1")
    }
    if (stemmer == "ngram_overlap") {
      if (stemmer_ngram < 1) stop("the minimum value for the stemmer_ngram parameter should be 1")
    }
  }
  if (min_n_gram < 1) stop("the min_n_gram parameter should be greater than 0")
  if (max_n_gram < 1) stop("the max_n_gram parameter should be greater than 0")
  if (skip_n_gram < 1) stop("the skip_n_gram parameter should be greater than 0")
  if (skip_distance < 0) stop("the skip_distance parameter should be greater or equal to 0")
  if (min_n_gram > max_n_gram) stop("the min_n_gram parameter should be less than or equal to the max_n_gram parameter")
  if (!is.character(n_gram_delimiter)) stop("the n_gram_delimiter parameter should be a character string")
  if (threads < 1) stop("the number of threads should be at least 1")
  if (!is.logical(verbose)) stop("the verbose parameter should be either TRUE or FALSE")
  if ((to_lower || to_upper) && language != "english") {          # THIS IS NOT THE ONLY EXCEPTION: in case of a user-defined stop-words list if the language is other than english and the default language stays the same ('english'), then the output is incorrect
    warning("if the 'language' parameter is not english and either a 'to_lower' or a 'to_upper' conversion takes place consider changing the 'utf_locale' parameter", call. = F)
  }
  query_transform = F

  tmp_fl_stopw = F

  if (inherits(remove_stopwords, 'logical')) {

    if (remove_stopwords) {

      tmp_fl_stopw = T
    }
  }

  if (max_num_char < 1000000000 || remove_char != "" || to_lower || to_upper || remove_punctuation_string || remove_punctuation_vector || remove_numbers || trim_token || split_string ||

      inherits(remove_stopwords, c('character', 'vector')) || tmp_fl_stopw || min_num_char > 1 || !is.null(stemmer) || min_n_gram > 1 || max_n_gram > 1 || skip_n_gram > 1) { query_transform = T }

  if (is.null(stemmer)) stemmer = "NULL"

  if (is.logical(remove_stopwords)) {

    if (remove_stopwords) {

      language_path = system.file("stopwords", paste0(language, ".txt"), package = "textTinyR")

      language_stop_words = utils::read.table(language_path, quote = "\"", comment.char = "", stringsAsFactors = F)

      language_stop_words[nrow(language_stop_words) + 1, ] = ""                  # add the empty character to the stopwords

      language_stop_words = as.vector(language_stop_words[, 1])}

    else {

      language_stop_words = character(0)
    }
  }

  if (inherits(remove_stopwords, c('character', 'vector'))) {

    language_stop_words = remove_stopwords

    remove_stopwords = T
  }

  gc();

  vp = vocabulary_counts(input_path_file, start_query, end_query, language_stop_words, vocabulary_path_file, min_lines, trimmed_line, query_transform, language, utf_locale, max_num_char, remove_char,

                         to_lower, to_upper, remove_punctuation_string, remove_punctuation_vector, remove_numbers, trim_token, split_string, split_separator, remove_stopwords, min_num_char,

                         stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, stemmer_ngram, stemmer_gamma, stemmer_truncate, stemmer_batches, threads, verbose)

  gc();

  return(structure(list(vocab_parser = paste0("the vocabulary file is saved in : ", vocabulary_path_file)), class = "vocabulary-counts-small-medium-files"))
}




#' bytes converter of a text file ( KB, MB or GB )
#'
#' @param input_path_file a character string specifying the path to the input file
#' @param unit a character string specifying the unit. One of \emph{KB}, \emph{MB}, \emph{GB}
#' @return a number
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' # bc = bytes_converter(input_path_file = 'some_file.txt', unit = "MB")


bytes_converter = function(input_path_file = NULL, unit = "MB") {

  try_err_file_input = inherits(tryCatch(normalizePath(input_path_file, mustWork = T), error = function(e) e), "error")
  if (try_err_file_input) stop("the input_path_file parameter should be a non-null valid path to a file")
  if (!unit %in% c('KB', 'MB', 'GB')) stop("the unit parameter should be one of 'KB', 'MB' or 'GB'")

  return(convert_bytes(input_path_file, unit))
}




#' text file parser
#'
#'
#' @param input_path_file a character string specifying the path to the input file
#' @param output_path_file a character string specifying the path to the output file
#' @param start_query a character string. The \emph{start_query} is the first word of the subset of the data and should appear frequently at the beginning of each line in the text file.
#' @param end_query a character string. The \emph{end_query} is the last word of the subset of the data and should appear frequently at the end of each line in the text file.
#' @param min_lines a numeric value specifying the minimum number of lines. For instance if min_lines = 2, then only subsets of text with more than 1 lines will be kept.
#' @param trimmed_line either TRUE or FALSE. If FALSE then each line of the text file will be trimmed both sides before applying the start_query and end_query
#' @param verbose either TRUE or FALSE. If TRUE then information will be printed in the console
#' @details
#' The text file should have a structure (such as an xml-structure), so that subsets can be extracted using the \emph{start_query} and \emph{end_query} parameters.
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' # fp = text_file_parser(input_path_file = '/folder/input_data.txt',
#'
#' #                       output_path_file = '/folder/output_data.txt',
#'
#' #                       start_query = 'word_a', end_query = 'word_w',
#'
#' #                       min_lines = 1, trimmed_line = FALSE)


text_file_parser = function(input_path_file = NULL, output_path_file = NULL, start_query = NULL, end_query = NULL, min_lines = 1, trimmed_line = FALSE, verbose = FALSE) {

  try_err_file_input = inherits(tryCatch(normalizePath(input_path_file, mustWork = T), error = function(e) e), "error")
  if (try_err_file_input) stop("the input_path_file parameter should be a non-null valid path to a file")
  if (is.null(output_path_file)) stop("the output_path_file parameter should be a non-null valid path to a file")
  if (!is.null(output_path_file)) {
    if (!inherits(output_path_file, 'character')) stop("the output_path_file parameter should a character string specifying a valid path to a file")}
  if (!is.character(start_query)) stop("the start_query parameter should be a character string")
  if (!is.character(end_query)) stop("the end_query parameter should be a character string")
  if (min_lines < 1) stop("the min_lines parameter should a numeric value greater than 0")
  if (!is.logical(trimmed_line)) stop("the trimmed_line parameter should be either TRUE or FALSE")
  if (!is.logical(verbose)) stop("the verbose parameter should be either TRUE or FALSE")

  tfp = file_parser(input_path_file, start_query, end_query, output_path_file, min_lines, trimmed_line, verbose)

  return(structure(list(text_parser = paste0("the output-data is saved in : ", output_path_file)), class = "tokenization and transformation"))
}



#' token statistics
#'
#'
#' @param x_vec either NULL or a string character vector
#' @param path_2folder either NULL or a valid path to a folder (each file in the folder should include words separated by a delimiter)
#' @param path_2file either NULL or a valid path to a file
#' @param file_delimiter either NULL or a character string specifying the file delimiter
#' @param n_gram_delimiter either NULL or a character string specifying the n-gram delimiter. It is used in the \emph{collocation_words} function
#' @param subset either NULL or a vector specifying the subset of data to keep (number of rows of the \emph{print_frequency} function)
#' @param number a numeric value for the \emph{print_count_character} function. All words with number of characters equal to the \emph{number} parameter will be returned.
#' @param word a character string for the \emph{print_collocations} and \emph{print_prob_next} functions
#' @param dice_n_gram a numeric value specifying the n-gram for the dice method of the \emph{string_dissimilarity_matrix} function
#' @param method a character string specifying the method to use in the \emph{string_dissimilarity_matrix} function. One of \emph{dice}, \emph{levenshtein} or \emph{cosine}.
#' @param split_separator a character string specifying the string split separator if method equal \emph{cosine} in the \emph{string_dissimilarity_matrix} function. The \emph{cosine} method uses sentences, so for a sentence : "this_is_a_word_sentence" the \emph{split_separator} should be "_"
#' @param dice_thresh a float number to use to threshold the data if method is \emph{dice} in the \emph{string_dissimilarity_matrix} function. It takes values between 0.0 and 1.0. The closer the thresh is to 0.0 the more values of the dissimilarity matrix will take the value of 1.0.
#' @param upper either TRUE or FALSE. If TRUE then both lower and upper parts of the dissimilarity matrix of the \emph{string_dissimilarity_matrix} function will be shown. Otherwise the upper part will be filled with NA's
#' @param diagonal either TRUE or FALSE. If TRUE then the diagonal of the dissimilarity matrix of the \emph{string_dissimilarity_matrix} function will be shown. Otherwise the diagonal will be filled with NA's
#' @param threads a numeric value specifying the number of cores to use in parallel in the \emph{string_dissimilarity_matrix} function
#' @param n_grams a numeric value specifying the n-grams in the \emph{look_up_table} function
#' @param n_gram a character string specifying the n-gram to use in the \emph{print_words_lookup_tbl} function
#' @export
#' @details
#'
#' the \emph{path_2vector} function returns the words of a \emph{folder} or \emph{file} to a vector ( using the \emph{file_delimiter} to input the data ). Usage: read a vocabulary from a text file
#'
#' the \emph{freq_distribution} function returns a named-unsorted vector frequency_distribution in R for EITHER a \emph{folder}, a \emph{file} OR a character string \emph{vector}. A specific subset of the result can be retrieved using the \emph{print_frequency} function
#'
#' the \emph{count_character} function returns the number of characters for each word of the corpus for EITHER a \emph{folder}, a \emph{file} OR a character string \emph{vector}. A specific number of character words can be retrieved using the \emph{print_count_character} function
#'
#' the \emph{collocation_words} function returns a co-occurence frequency table for n-grams for EITHER a \emph{folder}, a \emph{file} OR a character string \emph{vector}. A collocation is defined as a sequence of two or more consecutive words, that has characteristics of a syntactic and semantic unit, and whose exact and unambiguous meaning or connotation cannot be derived directly from the meaning or connotation of its components ( \url{http://nlp.stanford.edu/fsnlp/promo/colloc.pdf}, page 172 ). The input to the function should be text n-grams separated by a delimiter (for instance 3- or 4-ngrams ). I can retrieve a specific frequency table by using the \emph{print_collocations} function
#'
#' the \emph{string_dissimilarity_matrix} function returns a string-dissimilarity-matrix using either the \emph{dice}, \emph{levenshtein} or \emph{cosine} distance. The input can be a character string \emph{vector} only. In case that the method is \emph{dice} then the dice-coefficient (similarity) is calculated between two strings for a specific number of character n-grams ( \emph{dice_n_gram} ).
#'
#' the \emph{look_up_table} returns a look-up-list where the list-names are the n-grams and the list-vectors are the words associated with those n-grams. The words for each n-gram can be retrieved using the \emph{print_words_lookup_tbl} function. The input can be a character string \emph{vector} only.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @section Methods:
#'
#' \describe{
#'  \item{\code{token_stats$new(x_vec = NULL, path_2folder = NULL, path_2file = NULL, file_delimiter = ' ', n_gram_delimiter = "_")}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{path_2vector()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{freq_distribution()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{print_frequency(subset = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{count_character()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{print_count_character(number = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{collocation_words()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{print_collocations(word = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{string_dissimilarity_matrix(dice_n_gram = 2, method = "dice", split_separator = " ", dice_thresh = 1.0, upper = TRUE, diagonal = TRUE, threads = 1)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{look_up_table(n_grams = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{print_words_lookup_tbl(n_gram = NULL)}}{}
#'  }
#'
#' @usage # utl <- token_stats$new(x_vec = NULL, path_2folder = NULL, path_2file = NULL,
#'
#' #                               file_delimiter = ' ', n_gram_delimiter = "_")
#' @examples
#'
#'
#' library(textTinyR)
#'
#' expl = c('one_word_token', 'two_words_token', 'three_words_token', 'four_words_token')
#'
#' tk <- token_stats$new(x_vec = expl, path_2folder = NULL, path_2file = NULL)
#'
#' #-------------------------
#' # frequency distribution:
#' #-------------------------
#'
#' tk$freq_distribution()
#'
#' # tk$print_frequency()
#'
#'
#' #------------------
#' # count characters:
#' #------------------
#'
#' cnt <- tk$count_character()
#'
#' # tk$print_count_character(number = 4)
#'
#'
#' #----------------------
#' # collocation of words:
#' #----------------------
#'
#' col <- tk$collocation_words()
#'
#' # tk$print_collocations(word = 'five')
#'
#'
#' #-----------------------------
#' # string dissimilarity matrix:
#' #-----------------------------
#'
#' dism <- tk$string_dissimilarity_matrix(method = 'levenshtein')
#'
#'
#' #---------------------
#' # build a look-up-table:
#' #---------------------
#'
#' lut <- tk$look_up_table(n_grams = 3)
#'
#' # tk$print_words_lookup_tbl(n_gram = 'e_w')



token_stats <- R6::R6Class("token_stats",

                           public = list(

                             x_vec = NULL,

                             path_2folder = NULL,

                             path_2file = NULL,

                             file_delimiter = NULL,

                             n_gram_delimiter = NULL,


                             #----------------
                             # initialization
                             #----------------

                             initialize = function(x_vec = NULL, path_2folder = NULL, path_2file = NULL, file_delimiter = '\n', n_gram_delimiter = "_") {

                               self$x_vec <- x_vec

                               self$path_2folder <- path_2folder

                               self$path_2file <- path_2file

                               self$file_delimiter <- file_delimiter

                               self$n_gram_delimiter <- n_gram_delimiter

                               if (!is.null(self$x_vec)) {
                                 if (!inherits(self$x_vec, c('character', 'vector')) || length(self$x_vec) < 2) stop("the x_vec parameter should be a character string vector of length greater than 1")}
                               if (!is.null(self$path_2folder)) {
                                 if (!is.character(self$path_2folder)) stop("the path_2folder parameter should be a valid character string path")
                                 str_SPL = strsplit(self$path_2folder, "")[[1]]
                                 if (!str_SPL[nchar(self$path_2folder)] %in% c("/", "\\")) stop('the path_2folder path should end in slash')}
                               if (!is.null(self$path_2file)) {
                                 try_err_file_input = inherits(tryCatch(normalizePath(self$path_2file, mustWork = T), error = function(e) e), "error")
                                 if (try_err_file_input) stop("the path_2file parameter should be a non-null valid path to a file")}
                               if (!is.character(self$file_delimiter)) stop("the file_delimiter parameter should be a character string")
                               if (!is.character(self$n_gram_delimiter)) stop("the n_gram_delimiter parameter should be a character string")
                               },


                             #----------------------------
                             # return a vector from a path
                             #----------------------------

                             path_2vector = function() {

                               if (is.null(self$path_2folder)) { self$path_2folder = "" }

                               if (is.null(self$path_2file)) { self$path_2file = "" }

                               return(Path_2vector(self$path_2folder, self$path_2file))
                             },


                             #-----------------------------------
                             # return the frequency distribution
                             #-----------------------------------

                             freq_distribution = function() {

                               if (is.null(self$x_vec)) { self$x_vec = character(0) }

                               if (is.null(self$path_2folder)) { self$path_2folder = "" }

                               if (is.null(self$path_2file)) { self$path_2file = "" }

                               tmp_freq = Frequency_distribution(self$x_vec, self$path_2folder, self$path_2file, self$file_delimiter)

                               df = cbind(data.table::data.table(words = as.vector(names(tmp_freq))), data.table::data.table(freq = as.vector(tmp_freq)))

                               private$save_frequency = df[order(-rank(freq), words)]

                               invisible()
                             },


                             #------------------------------------------------
                             # print specific subset of frequency distribution
                             #------------------------------------------------

                             print_frequency = function(subset = NULL) {

                               if (!is.null(subset)) {
                                 if (!inherits(subset, c("integer", "numeric"))) stop("the subset parameter should be a numeric value or a sequence of numbers")
                               }

                               if (is.null(subset)) {

                                 print(private$save_frequency)}

                               else {

                                 print(private$save_frequency[subset, ])
                               }
                             },


                             #------------------------------------
                             # return the counts of the characters
                             #------------------------------------

                             count_character = function() {

                               if (is.null(self$x_vec)) { self$x_vec = character(0) }

                               if (is.null(self$path_2folder)) { self$path_2folder = "" }

                               if (is.null(self$path_2file)) { self$path_2file = "" }

                               tmp_count = Count_characters(self$x_vec, self$path_2folder, self$path_2file, self$file_delimiter)

                               private$save_counts = tmp_count

                               return(sort(as.numeric(names(tmp_count))))
                             },


                             #--------------------------------------
                             # print counts for a specific character
                             #--------------------------------------

                             print_count_character = function(number = NULL) {

                               if (!inherits(number, c("integer", "numeric"))) stop("the number parameter should be a numeric value")
                               if (length(number) > 1) stop("the number parameter should be a numeric value of length 1")

                               return(private$save_counts[[as.character(number)]])
                             },


                             #------------------------------------------
                             # returns the keys of the collocation-words
                             #------------------------------------------

                             collocation_words = function() {

                               if (is.null(self$x_vec)) { self$x_vec = character(0) }

                               if (is.null(self$path_2folder)) { self$path_2folder = "" }

                               if (is.null(self$path_2file)) { self$path_2file = "" }

                               private$save_collocations = Collocations_ngrams(self$x_vec, self$path_2folder, self$path_2file, self$file_delimiter, self$n_gram_delimiter)

                               return(sort(names(private$save_collocations)))
                             },


                             #--------------------------------------------------
                             # print the co-occurence words for a specific word
                             #--------------------------------------------------

                             print_collocations = function(word = NULL) {

                               if (!is.character(word)) stop("the word parameter should be a character string")

                               tmp_value = private$save_collocations[[word]]

                               return(round(sort(tmp_value, decreasing = T)/sum(tmp_value), 3))
                             },


                             #-----------------------------------------------------------------------------------------------
                             # returns dissimilarity matrix of character strings for the dice, levenshtein and cosine metrics
                             #-----------------------------------------------------------------------------------------------

                             string_dissimilarity_matrix = function(dice_n_gram = 2, method = "dice", split_separator = " ", dice_thresh = 1.0, upper = TRUE, diagonal = TRUE, threads = 1) {

                               if (!inherits(dice_n_gram, c("integer", "numeric"))) stop("the dice_n_gram parameter should be a numeric value")
                               if (length(dice_n_gram) > 1) stop("the dice_n_gram parameter should be a numeric value of length 1")
                               if (dice_n_gram < 1) stop("the dice_n_gram parameter should be greater than 0")
                               if (!method %in% c("dice", "levenshtein", "cosine")) stop("the method parameter should be one of 'dice', 'levenshtein' or 'cosine'")
                               if (!is.character(split_separator)) stop("the split_separator parameter should be a character string")
                               if (dice_thresh > 1.0 || dice_thresh < 0.0) stop("the dice_thresh parameter should be a float number between 0.0 and 1.0")
                               if (!is.logical(upper)) stop("the upper parameter should be either TRUE or FALSE")
                               if (!is.logical(diagonal)) stop("the diagonal parameter should be either TRUE or FALSE")
                               if (threads < 1) stop("the threads parameter should be greater than 0")

                               tmp_dism = Dissimilarity_mat(self$x_vec, dice_n_gram, method, split_separator, dice_thresh, upper, diagonal, threads)

                               colnames(tmp_dism) = self$x_vec

                               rownames(tmp_dism) = self$x_vec

                               return(tmp_dism)
                             },


                             #--------------------------------------------------------------
                             # returns a look-up table using n-grams for a vector of strings
                             #--------------------------------------------------------------

                             look_up_table = function(n_grams = NULL) {

                               if (!inherits(n_grams, c("integer", "numeric"))) stop("the n_grams parameter should be a numeric value")
                               if (length(n_grams) > 1) stop("the n_grams parameter should be a numeric value of length 1")
                               if (n_grams < 1) stop("the n_grams parameter should be greater than 0")

                               private$save_look_up_table = Look_up_tbl(self$x_vec, n_grams)

                               return(sort(names(private$save_look_up_table)))
                             },


                             #--------------------------------------------------------
                             # print words associated to n-grams in the look-up-table
                             #--------------------------------------------------------

                             print_words_lookup_tbl = function(n_gram = NULL) {

                               if (!is.character(n_gram)) stop("the n_gram parameter should be a character string")
                               if (length(n_gram) > 1) stop("the n_gram parameter should be a numeric value of length 1")

                               return(private$save_look_up_table[[n_gram]])
                             }
                           ),

                           private = list(

                             save_frequency = NULL,

                             save_counts = NULL,

                             save_collocations = NULL,

                             save_next_words = NULL,

                             save_look_up_table = NULL)
)




#' dice similarity of words using n-grams
#'
#'
#' @param word1 a character string
#' @param word2 a character string
#' @param n_grams a value specifying the consecutive n-grams of the words
#' @return a float number
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' word1 = 'one_word'
#'
#' word2 = 'two_words'
#'
#' dts = dice_distance(word1, word2, n_grams = 2)


dice_distance = function(word1, word2, n_grams = 2) {

  if (!inherits(word1, 'character')) stop("the word1 parameter should be a character string")
  if (!inherits(word2, 'character')) stop("the word2 parameter should be a character string")
  if (!inherits(n_grams, c('integer', 'numeric'))) stop("the n_grams parameter should be a numeric value")
  if (n_grams < 1) stop("the n_grams parameter should be at least 1")

  return(Dice_similarity(word1, word2, n_grams))
}


#' levenshtein distance of two words
#'
#'
#' @param word1 a character string
#' @param word2 a character string
#' @return a float number
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' word1 = 'one_word'
#'
#' word2 = 'two_words'
#'
#' lvs = levenshtein_distance(word1, word2)

levenshtein_distance = function(word1, word2) {

  if (!inherits(word1, 'character')) stop("the word1 parameter should be a character string")
  if (!inherits(word2, 'character')) stop("the word2 parameter should be a character string")

  return(Levenshtein_dist(word1, word2))
}


#' cosine distance of two character strings (each string consists of more than one words)
#'
#'
#' @param sentence1 a character string consisting of multiple words
#' @param sentence2 a character string consisting of multiple words
#' @param split_separator a character string specifying the delimiter(s) to split the sentence
#' @return a float number
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' sentence1 = 'this is one sentence'
#'
#' sentence2 = 'this is a similar sentence'
#'
#' cds = cosine_distance(sentence1, sentence2)

cosine_distance = function(sentence1, sentence2, split_separator = " ") {

  if (!inherits(sentence1, 'character')) stop("the sentence1 parameter should be a character string")
  if (!inherits(sentence2, 'character')) stop("the sentence2 parameter should be a character string")
  if (!inherits(split_separator, 'character')) stop("the split_separator parameter should be a character string")

  return(Cosine_dist(sentence1, sentence2, split_separator))
}





#' Term matrices and statistics ( document-term-matrix, term-document-matrix)
#'
#'
#' @param vector_data either NULL or a character vector of documents
#' @param file_data either NULL or a valid character path to a text file
#' @param document_term_matrix either TRUE or FALSE. If TRUE then a document-term-matrix will be returned, otherwise a term-document-matrix
#' @param sort_terms either TRUE or FALSE specifying if the initial terms should be sorted ( so that the output sparse matrix is sorted in alphabetical order )
#' @param to_lower either TRUE or FALSE. If TRUE the character string will be converted to lower case
#' @param remove_char a string specifying the specific characters that should be removed from a text file. If the \emph{remove_char} is "" then no removal of characters take place
#' @param to_upper either TRUE or FALSE. If TRUE the character string will be converted to upper case
#' @param utf_locale the language specific locale to use in case that either the \emph{to_lower} or the \emph{to_upper} parameter is TRUE and the text file language is other than english. For instance if the language of a text file is greek then the \emph{utf_locale} parameter should be \emph{'el_GR.UTF-8'} ( \emph{language_country.encoding} ). A wrong utf-locale does not raise an error, however the runtime of the function increases.
#' @param remove_punctuation_string either TRUE or FALSE. If TRUE then the punctuation of the character string will be removed (applies before the split function)
#' @param remove_punctuation_vector either TRUE or FALSE. If TRUE then the punctuation of the vector of the character strings will be removed  (after the string split has taken place)
#' @param remove_numbers either TRUE or FALSE. If TRUE then any numbers in the character string will be removed
#' @param trim_token either TRUE or FALSE. If TRUE then the string will be trimmed (left and/or right)
#' @param split_string either TRUE or FALSE. If TRUE then the character string will be split using the \emph{split_separator} as delimiter. The user can also specify multiple delimiters.
#' @param split_separator a character string specifying the character delimiter(s)
#' @param remove_stopwords either TRUE, FALSE or a character vector of user defined stop words. If TRUE then by using the \emph{language} parameter the corresponding stop words vector will be uploaded.
#' @param language a character string which defaults to english. If the \emph{remove_stopwords} parameter is TRUE then the corresponding stop words vector will be uploaded. Available languages
#' are \emph{afrikaans}, \emph{arabic}, \emph{armenian}, \emph{basque}, \emph{bengali}, \emph{breton}, \emph{bulgarian}, \emph{catalan},
#' \emph{croatian}, \emph{czech}, \emph{danish}, \emph{dutch}, \emph{english}, \emph{estonian},
#' \emph{finnish}, \emph{french}, \emph{galician}, \emph{german}, \emph{greek}, \emph{hausa}, \emph{hebrew}, \emph{hindi}, \emph{hungarian},
#' \emph{indonesian}, \emph{irish}, \emph{italian}, \emph{latvian}, \emph{marathi},
#' \emph{norwegian}, \emph{persian}, \emph{polish}, \emph{portuguese}, \emph{romanian}, \emph{russian}, \emph{slovak}, \emph{slovenian},
#' \emph{somalia}, \emph{spanish}, \emph{swahili}, \emph{swedish}, \emph{turkish}, \emph{yoruba}, \emph{zulu}
#' @param min_num_char an integer specifying the minimum number of characters to keep. If the \emph{min_num_char} is greater than 1 then character strings with more than 1 characters will be returned
#' @param max_num_char an integer specifying the maximum number of characters to keep. The \emph{max_num_char} should be less than or equal to \emph{Inf} (in this function the Inf value translates to a word-length of 1000000000)
#' @param stemmer a character string specifying the stemming method. Available method is the \emph{porter2_stemmer}. See details for more information.
#' @param min_n_gram an integer specifying the minimum number of n-grams. The minimum number of min_n_gram is 1.
#' @param max_n_gram an integer specifying the maximum number of n-grams. The minimum number of max_n_gram is 1.
#' @param skip_n_gram an integer specifying the number of skip-n-grams. The minimum number of skip_n_gram is 1. The skip_n_gram gives the (max.) n-grams using the \emph{skip_distance} parameter. If \emph{skip_n_gram} is greater than 1 then both \emph{min_n_gram} and \emph{max_n_gram} should be set to 1.
#' @param skip_distance an integer specifying the skip distance between the words. The minimum value for the skip distance is 0, in which case simple n-grams will be returned.
#' @param n_gram_delimiter a character string specifying the n-gram delimiter (applies to both n-gram and skip-n-gram cases)
#' @param print_every_rows a numeric value greater than 1 specifying the print intervals. Frequent output in the R session can slow down the function in case of big files.
#' @param normalize either NULL or one of 'l1' or 'l2' normalization.
#' @param tf_idf either TRUE or FALSE. If TRUE then the term-frequency-inverse-document-frequency will be returned
#' @param threads an integer specifying the number of cores to run in parallel
#' @param verbose either TRUE or FALSE. If TRUE then information will be printed out
#' @param sparsity_thresh a float number between 0.0 and 1.0 specifying the sparsity threshold in the \emph{Term_Matrix_Adjust} function
#' @param Terms a character vector specifying the character strings for which the associations will be calculated ( \emph{term_associations} function )
#' @param keep_terms either NULL or a numeric value specifying the number of terms to keep ( both in \emph{term_associations} and \emph{most_frequent_terms} functions )
#' @export
#' @details
#'
#' the \emph{Term_Matrix} function takes either a character vector of strings or a text file and after tokenization and transformation returns either a document-term-matrix or a term-document-matrix
#'
#' the \emph{Term_Matrix_Adjust} function removes sparse terms from a sparse matrix using a sparsity threshold
#'
#' the \emph{term_associations} function finds the associations between the given terms (Terms argument) and all the other terms in the corpus by calculating their correlation. There is also the option to keep a specific number of terms from the output table using the \emph{keep_terms} parameter.
#'
#' the \emph{most_frequent_terms} function returns the most frequent terms of the corpus using the output of the sparse matrix. The user has the option to keep a specific number of terms from the output table using the \emph{keep_terms} parameter.
#'
#' Stemming of the english language is done using the porter2-stemmer, for details see \url{https://github.com/smassung/porter2_stemmer}
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import Matrix
#' @importFrom data.table data.table
#' @section Methods:
#'
#' \describe{
#'  \item{\code{sparse_term_matrix$new(vector_data = NULL, file_data = NULL, document_term_matrix = TRUE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Term_Matrix(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE, remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " .,;:()?!", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", print_every_rows = 1000, normalize = NULL, tf_idf = FALSE, threads = 1, verbose = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Term_Matrix_Adjust(sparsity_thresh = 1.0)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{term_associations(Terms = NULL, keep_terms = NULL, threads = 1, verbose = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{most_frequent_terms(keep_terms = NULL, threads = 1, verbose = FALSE)}}{}
#'  }
#'
#' @usage # utl <- sparse_term_matrix$new(vector_data = NULL, file_data = NULL,
#'
#' #                                      document_term_matrix = TRUE)
#' @examples
#'
#' library(textTinyR)
#'
#'
#' # sm <- sparse_term_matrix$new(file_data = "/folder/my_data.txt",
#'
#' #                              document_term_matrix = TRUE)
#'
#' #--------------
#' # term matrix :
#' #--------------
#'
#' # sm$Term_Matrix(sort_terms = TRUE, to_lower = TRUE,
#'
#' #                trim_token = TRUE, split_string = TRUE,
#'
#' #                remove_stopwords = TRUE, normalize = 'l1',
#'
#' #                stemmer = 'porter2_stemmer', threads = 1 )
#'
#'
#' #-------------------------
#' # removal of sparse terms:
#' #-------------------------
#'
#' # sm$Term_Matrix_Adjust(sparsity_thresh = 0.995)
#'
#'
#' #-----------------------------------------------
#' # associations between terms of a sparse matrix:
#' #-----------------------------------------------
#'
#'
#' # sm$term_associations(Terms = c("word", "sentence"), keep_terms = 10)
#'
#'
#' #---------------------------------------------
#' # most frequent terms using the sparse matrix:
#' #---------------------------------------------
#'
#'
#' # sm$most_frequent_terms(keep_terms = 10, threads = 1)



sparse_term_matrix <- R6::R6Class("sparse_term_matrix",

                                  #lock_objects = FALSE,

                                   public = list(

                                    vector_data = NULL,

                                    file_data = NULL,

                                    document_term_matrix = TRUE,


                                    #----------------
                                    # initialization
                                    #----------------

                                    initialize = function(vector_data = NULL, file_data = NULL, document_term_matrix = TRUE) {

                                      self$vector_data <- vector_data

                                      self$file_data <- file_data

                                      self$document_term_matrix <- document_term_matrix

                                      if (is.null(self$vector_data) && is.null(self$file_data)) stop("either the vector_data or the file_data can be NULL but not both")
                                      if (!is.null(self$vector_data) && !is.null(self$file_data)) stop("either the vector_data or the file_data can be non-NULL but not both")
                                      if (!is.null(self$vector_data)) {
                                        if (!inherits(self$vector_data, c("vector", "character"))) stop("the vector_data parameter should be a character vector")}
                                      if (!is.null(self$file_data)) {
                                        try_err_file_input = inherits(tryCatch(normalizePath(self$file_data, mustWork = T), error = function(e) e), "error")
                                        if (try_err_file_input) stop("the file_data parameter should be a non-null valid path to a file")}
                                      if (!is.logical(self$document_term_matrix)) stop("the document_term_matrix parameter should be either TRUE or FALSE")
                                    },



                                    #---------------------------------------------
                                    # document-term-matrix OR term-document-matrix
                                    #---------------------------------------------

                                    Term_Matrix = function(sort_terms = FALSE, to_lower = FALSE, to_upper = FALSE, utf_locale = "", remove_char = "", remove_punctuation_string = FALSE, remove_punctuation_vector = FALSE,

                                                           remove_numbers = FALSE, trim_token = FALSE, split_string = FALSE, split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english",

                                                           min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1, max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ",

                                                           print_every_rows = 1000, normalize = NULL, tf_idf = FALSE, threads = 1, verbose = FALSE) {

                                      if (!is.logical(sort_terms)) stop("the sort_terms parameter should be either TRUE or FALSE")
                                      if (!is.logical(to_lower)) stop("the to_lower parameter should be either TRUE or FALSE")
                                      if (!is.logical(to_upper)) stop("the to_upper parameter should be either TRUE or FALSE")
                                      if (!inherits(utf_locale, 'character')) stop("the 'utf_locale' parameter should be a character string")
                                      if (!is.character(remove_char)) stop("the remove_char parameter should be a character string")
                                      if (!is.logical(remove_punctuation_string)) stop("the remove_punctuation_string parameter should be either TRUE or FALSE")
                                      if (!is.logical(remove_punctuation_vector)) stop("the remove_punctuation_vector parameter should be either TRUE or FALSE")
                                      if (!is.logical(remove_numbers)) stop("the remove_numbers parameter should be either TRUE or FALSE")
                                      if (!is.logical(trim_token)) stop("the trim_token parameter should be either TRUE or FALSE")
                                      if (!is.logical(split_string)) stop("the split_string parameter should be either TRUE or FALSE")
                                      if (!inherits(split_separator, 'character')) stop("the split_separator should be a character string consisting of one or more delimiters")
                                      if (!inherits(remove_stopwords, c('character', 'vector', 'logical'))) {
                                        stop("the 'remove_stopwords' parameter should be either TRUE, FALSE or a character vector of stop words")}
                                      if (!language %in% c("afrikaans", "arabic", "armenian", "basque", "bengali", "breton",
                                                           "bulgarian", "catalan", "croatian", "czech", "danish",
                                                           "dutch", "english", "estonian", "finnish", "french",
                                                           "galician", "german", "greek", "hausa", "hebrew", "hindi", "hungarian",
                                                           "indonesian", "irish", "italian", "latvian", "marathi", "norwegian",
                                                           "persian", "polish", "portuguese", "romanian", "russian", "slovak",
                                                           "slovenian", "somalia", "spanish", "swahili", "swedish", "turkish", "yoruba", "zulu"))
                                        stop("available languages in case of stop-word removal are 'afrikaans', 'arabic', 'armenian',
                                             'basque', 'bengali', 'breton', 'bulgarian', 'catalan', 'croatian', 'czech',
                                             'danish', 'dutch', 'english', 'estonian', 'finnish', 'french', 'galician',
                                             'german', 'greek', 'hausa', 'hebrew', 'hindi', 'hungarian', 'indonesian', 'irish',
                                             'italian', 'latvian', 'marathi', 'norwegian', 'persian', 'polish', 'portuguese',
                                             'romanian', 'russian', 'slovak', 'slovenian', 'somalia', 'spanish', 'swahili',
                                             'swedish', 'turkish', 'yoruba', 'zulu'.

                                             A user defined character vector of stop words can be used as input, if the target language is not included")
                                      if (min_num_char < 1) stop("the min_num_char parameter should be greater than 0")
                                      if (min_num_char >= max_num_char) stop("the max_num_char parameter should be greater than the min_num_char")
                                      if (max_num_char == Inf) max_num_char = 1000000000
                                      if (!is.null(stemmer)) {
                                        if (!stemmer %in% c("porter2_stemmer")) stop("valid stemming method is porter2_stemmer")
                                      }

                                      if (is.null(stemmer)) stemmer = "NULL"
                                      if (min_n_gram < 1) stop("the min_n_gram parameter should be greater than 0")
                                      if (max_n_gram < 1) stop("the max_n_gram parameter should be greater than 0")
                                      if (skip_n_gram < 1) stop("the skip_n_gram parameter should be greater than 0")
                                      if (skip_distance < 0) stop("the skip_distance parameter should be greater or equal to 0")
                                      if (min_n_gram > max_n_gram) stop("the min_n_gram parameter should be less than or equal to the max_n_gram parameter")
                                      if (!is.character(n_gram_delimiter)) stop("the n_gram_delimiter parameter should be a character string")

                                      if (print_every_rows < 1) stop("the minimum for the print_every_rows parameter is 1")

                                      if (!is.null(normalize)) {
                                        if (!normalize %in% c("l1", "l2")) {
                                          stop("valid normalize functions are 'l1' or 'l2'")
                                        }
                                      }
                                      if (is.null(normalize)) normalize = "NULL"

                                      private$normlz_tf = normalize                                                                    # private variable

                                      if (!is.logical(tf_idf)) stop("the tf_idf parameter should be either TRUE or FALSE")

                                      private$TF_idf = tf_idf                                                                           # private variable

                                      if (threads < 1) stop("the number of threads should be at least 1")
                                      if (!is.logical(verbose)) stop("the verbose parameter should be either TRUE or FALSE")
                                      if ((to_lower || to_upper) && language != "english") {          # THIS IS NOT THE ONLY EXCEPTION: in case of a user-defined stop-words list if the language is other than english and the default language stays the same ('english'), then the output is incorrect
                                        warning("if the 'language' parameter is not english and either a 'to_lower' or a 'to_upper' conversion takes place consider changing the 'utf_locale' parameter", call. = F)
                                      }

                                      if (is.logical(remove_stopwords)) {

                                        if (remove_stopwords) {

                                          language_path = system.file("stopwords", paste0(language, ".txt"), package = "textTinyR")

                                          language_stop_words = read.table(language_path, quote = "\"", comment.char = "", stringsAsFactors = F)

                                          language_stop_words[nrow(language_stop_words) + 1, ] = ""                  # add the empty character to the stopwords

                                          language_stop_words = as.vector(language_stop_words[, 1])}

                                        else {

                                          language_stop_words = character(0)
                                        }
                                      }

                                      if (inherits(remove_stopwords, c('character', 'vector'))) {

                                        language_stop_words = remove_stopwords

                                        remove_stopwords = T
                                      }

                                      gc();

                                      if (is.null(self$vector_data)) {

                                        tmp_VEC = character(0) }

                                      else {

                                        tmp_VEC = self$vector_data
                                      }

                                      if (is.null(self$file_data)) {

                                        tmp_2docfile = "NULL" }

                                      else {

                                        tmp_2docfile = self$file_data
                                      }

                                      stemmer_ngram_CONST = 4; stemmer_gamma_CONST = 0.0; stemmer_truncate_CONST = 3; stemmer_batches_CONST = 1;     # add constant values for stemming other than porter_2stemmer [ n-gram stemming applies to a whole corpus and not to single sub-vectors of documents ]

                                      tmp_res = res_term_matrix(vector_corpus = tmp_VEC, language_stop_words, language, utf_locale, max_num_char, self$document_term_matrix, path_2documents_file = tmp_2docfile, sort_terms, remove_char,

                                                                cpp_to_lower = to_lower, cpp_to_upper = to_upper, cpp_remove_punctuation = remove_punctuation_string, remove_punctuation_vector, cpp_remove_numbers = remove_numbers,

                                                                cpp_trim_token = trim_token, cpp_tokenization_function = split_string, cpp_string_separator = split_separator, cpp_remove_stopwords = remove_stopwords, min_num_char,

                                                                stemmer, min_n_gram, max_n_gram, skip_n_gram, skip_distance, n_gram_delimiter, stemmer_ngram_CONST, stemmer_gamma_CONST, stemmer_truncate_CONST, stemmer_batches_CONST,

                                                                threads, verbose, print_every_rows, normalize, tf_idf)

                                      private$tm_column_indices = as.vector(tmp_res$cols)

                                      private$tm_row_indices = as.vector(tmp_res$rows)

                                      private$tm_docs_counts = as.vector(tmp_res$counts)

                                      if (self$document_term_matrix) {

                                        res_mat = tmp_res$term_matrix

                                        private$save_terms = as.vector(tmp_res$terms)

                                        if (sum(c("", " ") %in% private$save_terms) > 0) {

                                          warning("empty character strings present in the column names they will be replaced with proper characters", call. = F)
                                        }

                                        mak_nams = enc2utf8(private$save_terms)           # UTF-8 encoding of terms of matrix

                                        private$save_terms = make.names(mak_nams)

                                        #private$save_terms = make.names(private$save_terms)

                                        colnames(res_mat) = private$save_terms

                                        private$save_sparse_mat = res_mat

                                        gc();

                                        return(res_mat)
                                      }

                                      if (!self$document_term_matrix) {

                                        res_mat = tmp_res$term_matrix

                                        private$save_terms = as.vector(tmp_res$terms)

                                        if (sum(c("", " ") %in% private$save_terms) > 0) {

                                          warning("empty character strings present in the row names they will be replaced with proper name characters", call. = F)
                                        }

                                        mak_nams = enc2utf8(private$save_terms)         # UTF-8 encoding of terms of matrix

                                        private$save_terms = make.names(mak_nams)

                                        #private$save_terms = make.names(private$save_terms)

                                        rownames(res_mat) = private$save_terms

                                        private$save_sparse_mat = res_mat

                                        gc();

                                        return(res_mat)
                                      }
                                    },



                                    #-----------------------------------------------------------------
                                    # subset the sparse matrix removing sparse terms using a threshold
                                    #-----------------------------------------------------------------

                                    Term_Matrix_Adjust = function(sparsity_thresh = 1.0) {

                                      if (is.null(private$tm_column_indices)) stop("first run the Term_Matrix method")
                                      if (!inherits(sparsity_thresh, c('numeric', 'integer'))) stop("the sparsity_thresh parameter should be of type numeric")
                                      if (sparsity_thresh > 1.0 || sparsity_thresh <= 0.0) stop("the sparsity_thresh parameter should be a float number greater than 0.0 and less than or equal to 1.0")

                                      private$flag_Adjust = T

                                      gc();

                                      res_adj = Adj_Sparsity(private$tm_column_indices, private$tm_row_indices, private$tm_docs_counts, private$save_terms, sparsity_thresh)

                                      if (length(res_adj) == 1) stop(paste(c("a sparsity_thresh of", sparsity_thresh, "returns an empty sparse matrix. Consider increasing the threshold"), collapse = " "))

                                      tmp_adj_mat = res_adj$sparse_matrix

                                      tmp_TERMS = res_adj$terms

                                      #-------------------------------------------------------
                                      # save the row-, column-indices, counts, terms and dimensions of the sparse-matrix, specifically for the 'term_associations' method, to avoid any indexing errors
                                      # especially if the sparse matrix includes zero-valued columns and the "tf_idf_exclude()" function is called
                                      # in case of zero-valued columns the 'correlation_assoc_single' function in the 'term_associations.h' header file gives a warning anyway

                                      private$associat_TERMS = tmp_TERMS
                                      private$associat_COL_IDX = res_adj$update_cols
                                      private$associat_ROW_IDX = res_adj$update_rows
                                      private$associat_COUNTS = res_adj$update_counts

                                      if (self$document_term_matrix) {

                                        private$associat_NROW_MATR = nrow(tmp_adj_mat)
                                        private$associat_NCOL_MATR = ncol(tmp_adj_mat)
                                      }

                                      if (!self$document_term_matrix) {

                                        private$associat_NROW_MATR = ncol(tmp_adj_mat)      # reverse for term-document-matrix
                                        private$associat_NCOL_MATR = nrow(tmp_adj_mat)
                                      }

                                      #-------------------------------------------------------

                                      if (private$TF_idf) {         # it is possible that after calculating the tf-idf some terms (along rows or columns) become zero-valued

                                        tmp_tf_idf_idx = tf_idf_exclude(tmp_adj_mat, T)

                                        tmp_adj_mat = tmp_adj_mat[, as.vector(tmp_tf_idf_idx) + 1, drop = F]

                                        tmp_TERMS = tmp_TERMS[as.vector(tmp_tf_idf_idx) + 1]
                                      }

                                      if (self$document_term_matrix) {

                                        colnames(tmp_adj_mat) = tmp_TERMS
                                      }

                                      if (!self$document_term_matrix) {

                                        tmp_adj_mat = t(tmp_adj_mat)

                                        rownames(tmp_adj_mat) = tmp_TERMS
                                      }

                                      private$save_terms_adjust = tmp_TERMS                         # update the private variables for the most_frequent_terms method

                                      private$save_sparse_mat_adjust = tmp_adj_mat

                                      gc();

                                      return(tmp_adj_mat)
                                    },


                                    #---------------------------------
                                    # find associations between terms
                                    #---------------------------------

                                    term_associations = function(Terms = NULL, keep_terms = NULL, threads = 1, verbose = FALSE) {

                                      if (is.null(private$tm_column_indices)) stop("first run the Term_Matrix method")
                                      if (!inherits(Terms, c('character', 'vector'))) stop("the Terms parameter should be a character vector")
                                      if (length(Terms) < 1) stop("the Terms parameter should be a non-NULL vector of length greater or equal to 1")
                                      if (!is.null(keep_terms)) {
                                        if (!inherits(keep_terms, c('numeric', 'integer'))) stop("the keep_terms parameter should be of type numeric")
                                        if (keep_terms < 1) stop("the minimum number of terms to keep is 1")}
                                      if (is.null(keep_terms)) keep_terms = 0
                                      if (threads < 1) stop("the number of threads should be greater or equal to 1")
                                      if (!is.logical(verbose)) stop("the verbose parameter should be either TRUE or FALSE")

                                      if (!private$flag_Adjust) {

                                        SAVE_TERMS = private$save_terms
                                        NROW_SP_MAT = nrow(private$save_sparse_mat)
                                        NCOL_SP_MAT = ncol(private$save_sparse_mat)
                                        COL_IDX = private$tm_column_indices
                                        ROW_IDX = private$tm_row_indices
                                        COUNTS = private$tm_docs_counts}

                                      else {

                                        SAVE_TERMS = private$associat_TERMS
                                        NROW_SP_MAT = private$associat_NROW_MATR
                                        NCOL_SP_MAT = private$associat_NCOL_MATR
                                        COL_IDX = private$associat_COL_IDX
                                        ROW_IDX = private$associat_ROW_IDX
                                        COUNTS = private$associat_COUNTS
                                      }

                                      single_trgt_idx = single_trgt_nam = list()

                                      count_add = 1

                                      for (item in 1:length(Terms)) {

                                        tmp_trm = which(SAVE_TERMS %in% Terms[item])

                                        if (length(tmp_trm) == 0) {

                                          cat("\n")
                                          cat("the '", Terms[item], "' term does not exist in the terms vector", "\n")}

                                        else {

                                          single_trgt_idx[[count_add]] = tmp_trm

                                          single_trgt_nam[[count_add]] = Terms[item]

                                          count_add = count_add + 1
                                        }
                                      }

                                      single_trgt_idx = unlist(single_trgt_idx)

                                      if (length(single_trgt_idx) == 0) {

                                        stop("none of the choosen Terms are present in the terms vector")
                                      }

                                      single_trgt_nam = unlist(single_trgt_nam)

                                      if (self$document_term_matrix) {

                                        trgt_size = NROW_SP_MAT}

                                      else {

                                        trgt_size = NCOL_SP_MAT
                                      }


                                      if (length(single_trgt_idx) == 1) {

                                        tmp_lst_r = Associations_Cpp(as.vector(COL_IDX + 1), as.vector(ROW_IDX), as.vector(COUNTS), trgt_size, as.vector(SAVE_TERMS),

                                                                     mult_target_var = numeric(0), keepTerms = keep_terms, target_var = single_trgt_idx,

                                                                     normalize_TF = private$normlz_tf, tf_IDF = private$TF_idf, threads, verbose)}

                                      else {

                                        tmp_lst_r = Associations_Cpp(as.vector(COL_IDX + 1), as.vector(ROW_IDX), as.vector(COUNTS), trgt_size, as.vector(SAVE_TERMS),

                                                                     mult_target_var = single_trgt_idx, keepTerms = keep_terms, target_var = -1,

                                                                     normalize_TF = private$normlz_tf, tf_IDF = private$TF_idf, threads, verbose)
                                      }


                                      if (length(single_trgt_nam) == 1) {

                                        tmp_lst_r = cbind(data.table::data.table(term = as.vector(tmp_lst_r$term)), data.table::data.table(correlation = as.vector(tmp_lst_r$correlation)))}

                                      else {

                                        tmp_lst_r = lapply(tmp_lst_r, function(x) cbind(data.table::data.table(term = as.vector(x$term)), data.table::data.table(correlation = as.vector(x$correlation))))

                                        names(tmp_lst_r) = single_trgt_nam
                                      }

                                      return(tmp_lst_r)
                                    },



                                    #-----------------------------
                                    # find the most frequent terms
                                    #-----------------------------

                                    most_frequent_terms = function(keep_terms = NULL, threads = 1, verbose = FALSE) {

                                      if (is.null(private$tm_column_indices)) stop("first run the Term_Matrix method")
                                      if ((private$normlz_tf %in% c("l1", "l2")) || private$TF_idf) stop("the most_frequent_terms method is invalid if the normalize parameter is not NULL or the tf_idf parameter is TRUE")
                                      if (!is.null(keep_terms)) {
                                        if (!inherits(keep_terms, c('numeric', 'integer'))) stop("the keep_terms parameter should be of type numeric")
                                        if (keep_terms < 1) stop("the minimum number of terms to keep is 1")}
                                      if (is.null(keep_terms)) keep_terms = 0
                                      if (threads < 1) stop("the number of threads should be greater or equal to 1")
                                      if (!is.logical(verbose)) stop("the verbose parameter should be either TRUE or FALSE")

                                      if (!private$flag_Adjust) {

                                        SAVE_TERMS = private$save_terms
                                        SAVE_SP_MAT = private$save_sparse_mat}

                                      else {

                                        SAVE_TERMS = private$save_terms_adjust
                                        SAVE_SP_MAT = private$save_sparse_mat_adjust
                                      }

                                      tmp_freq = Most_Freq_Terms(SAVE_SP_MAT, SAVE_TERMS, keep_terms, self$document_term_matrix, threads, verbose)

                                      tmp_lst_r = cbind(data.table::data.table(term = as.vector(tmp_freq$term)), data.table::data.table(frequency = as.vector(tmp_freq$frequency)))

                                      return(tmp_lst_r)
                                    }

                                  ),

                                  private = list(

                                    associat_TERMS = NULL,

                                    associat_COL_IDX = NULL,

                                    associat_ROW_IDX = NULL,

                                    associat_COUNTS = NULL,

                                    associat_NROW_MATR = NULL,

                                    associat_NCOL_MATR = NULL,

                                    save_terms = NULL,

                                    save_sparse_mat = NULL,

                                    tm_column_indices = NULL,

                                    tm_row_indices = NULL,

                                    tm_docs_counts = NULL,

                                    save_terms_adjust = NULL,

                                    save_sparse_mat_adjust = NULL,

                                    normlz_tf = NULL,

                                    flag_Adjust = FALSE,

                                    TF_idf = FALSE)
)




#' convert a dense matrix to a sparse matrix
#'
#'
#' @param dense_mat a dense matrix
#' @return a sparse matrix
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' tmp = matrix(sample(0:1, 100, replace = TRUE), 10, 10)
#'
#' sp_mat = dense_2sparse(tmp)


dense_2sparse = function(dense_mat) {

  if (!inherits(dense_mat, 'matrix')) stop("the dense_mat parameter should be a matrix")

  return(dense_2sparse_mat(dense_mat))
}



#' RowSums and colSums for a sparse matrix
#'
#'
#' @param sparse_matrix a sparse matrix
#' @param rowSums either TRUE or FALSE. If TRUE then the row-sums will be calculated, otherwise the column-sums
#' @return a vector with either the row- or the column-sums of the matrix
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' tmp = matrix(sample(0:1, 100, replace = TRUE), 10, 10)
#'
#' sp_mat = dense_2sparse(tmp)
#'
#' spsm = sparse_Sums(sp_mat, rowSums = FALSE)

sparse_Sums = function(sparse_matrix, rowSums = FALSE) {

  if (!inherits(sparse_matrix, 'dgCMatrix')) stop("the sparse_matrix parameter should be a 'dgCMatrix' sparse matrix")
  if (!is.logical(rowSums)) stop("the rowSums parameter should be either TRUE or FALSE")

  return(as.vector(sp_sums(sparse_matrix, rowSums)))
}



#' RowMens and colMeans for a sparse matrix
#'
#'
#' @param sparse_matrix a sparse matrix
#' @param rowMeans either TRUE or FALSE. If TRUE then the row-means will be calculated, otherwise the column-means
#' @return a vector with either the row- or the column-sums of the matrix
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' tmp = matrix(sample(0:1, 100, replace = TRUE), 10, 10)
#'
#' sp_mat = dense_2sparse(tmp)
#'
#' spsm = sparse_Means(sp_mat, rowMeans = FALSE)

sparse_Means = function(sparse_matrix, rowMeans = FALSE) {

  if (!inherits(sparse_matrix, 'dgCMatrix')) stop("the sparse_matrix parameter should be a 'dgCMatrix' sparse matrix")
  if (!is.logical(rowMeans)) stop("the rowMeans parameter should be either TRUE or FALSE")

  return(as.vector(sp_means(sparse_matrix, rowMeans)))
}




#' sparsity percentage of a sparse matrix
#'
#'
#' @param sparse_matrix a sparse matrix
#' @return a numeric value (percentage)
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' tmp = matrix(sample(0:1, 100, replace = TRUE), 10, 10)
#'
#' sp_mat = dense_2sparse(tmp)
#'
#' dbl = matrix_sparsity(sp_mat)


matrix_sparsity = function(sparse_matrix) {

  if (!inherits(sparse_matrix, 'dgCMatrix')) stop("the sparse_matrix parameter should be a 'dgCMatrix' sparse matrix")

  sparsity_float(sparse_matrix)
}



#' save a sparse matrix in binary format
#'
#'
#' @param sparse_matrix a sparse matrix
#' @param file_name a character string specifying the binary file
#' @return writes the sparse matrix to a file
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' tmp = matrix(sample(0:1, 100, replace = TRUE), 10, 10)
#'
#' sp_mat = dense_2sparse(tmp)
#'
#' # save_sparse_binary(sp_mat, file_name = "save_sparse.mat")


save_sparse_binary = function(sparse_matrix, file_name = "save_sparse.mat") {

  if (!inherits(sparse_matrix, 'dgCMatrix')) stop("the sparse_matrix parameter should be a 'dgCMatrix' sparse matrix")
  if (is.null(file_name)) stop("the file_name parameter should a non-null valid path to a file")
  if (!inherits(file_name, 'character')) stop("the file_name parameter should a character string specifying a valid file-path")

  save_sparse_(sparse_matrix, file_name)

  invisible()
}



#' load a sparse matrix in binary format
#'
#'
#' @param file_name a character string specifying the binary file
#' @return loads a sparse matrix from a file
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' # load_sparse_binary(file_name = "save_sparse.mat")


load_sparse_binary = function(file_name = "save_sparse.mat") {

  if (!inherits(file_name, 'character')) stop("the file_name parameter should a character string specifying a valid path to a file")

  tmp_mt = load_sparse_(file_name)

  return(tmp_mt)
}




#' read a specific number of characters from a text file
#'
#'
#' @param input_file a character string specifying a valid path to a text file
#' @param characters a numeric value specifying the number of characters to read
#' @param write_2file either an empty string ("") or a character string specifying a valid output file to write the subset of the input file
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' # txfl = read_characters(input_file = 'input.txt', characters = 100)


read_characters = function(input_file = NULL, characters = 100, write_2file = "") {

  try_err_file_input = inherits(tryCatch(normalizePath(input_file, mustWork = T), error = function(e) e), "error")
  if (try_err_file_input) stop("the input_file parameter should be a non-null valid path to a file")
  if (!inherits(write_2file, 'character')) stop("the write_2file parameter should be a character string specifying a valid path to a file")
  if (characters < 1) stop("the characters parameter should be at least 1")

  res = read_CHARS(input_file, characters, write_2file)

  tmp_lst = structure(list(data = res), class = "textTinyR read characters")

  if (write_2file != "") {

    tmp_lst[['file_data']] = paste0("the output-data is saved in : ", write_2file)
  }

  return(tmp_lst)
}




#' read a specific number of rows from a text file
#'
#'
#' @param input_file a character string specifying a valid path to a text file
#' @param read_delimiter a character string specifying the row delimiter of the text file
#' @param rows a numeric value specifying the number of rows to read
#' @param write_2file either "" or a character string specifying a valid output file to write the subset of the input file
#' @export
#' @examples
#'
#' library(textTinyR)
#'
#' # txfl = read_rows(input_file = 'input.txt', rows = 100)


read_rows = function(input_file = NULL, read_delimiter = "\n", rows = 100, write_2file = "") {

  try_err_file_input = inherits(tryCatch(normalizePath(input_file, mustWork = T), error = function(e) e), "error")
  if (try_err_file_input) stop("the input_file parameter should be a non-null valid path to a file")
  if (!inherits(read_delimiter, 'character')) stop("the read_delimiter parameter should be a valid character string")
  if (rows < 1) stop("the rows parameter should be at least 1")
  if (!inherits(write_2file, 'character')) stop("the write_2file parameter should be a character string specifying a valid path to a file")

  res = read_ROWS(input_file, write_2file, read_delimiter, rows)

  tmp_lst = structure(list(data = res), class = "textTinyR read rows")

  if (write_2file != "") {

    tmp_lst[['file_data']] = paste0("the output-data is saved in : ", write_2file)
  }

  return(tmp_lst)
}




