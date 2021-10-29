
#-------------------------
# global variable cnt_tsts
#-------------------------

cnt_tsts <- 1



#-----------------------------------------------------------------------------------------------------
# exception for the Linux-debian-clang distribution because I get an error in the following languages
# due to the fact that the input text I use to test these functions includes apostrophes or dashes.
# This cannot be changed, therefore I'll skip these tests for the debian distribution
#
#        greek, russian, bulgarian, arabic, armenian, bengali, estonian, hebrew, hindi, hungarian,
#        irish, marathi, persian, spanish
#
# SEE also the e-mail that I received from CRAN at the end of this file.
#-----------------------------------------------------------------------------------------------------


exception_latin1_locale_debian = function() {

  run_test = TRUE

  if (Sys.info()[['sysname']] == 'Linux') {

    extract_distribution = system2(command = "lsb_release", args = c("-i"), stdout = TRUE, stderr = TRUE)            # https://www.cyberciti.biz/faq/find-linux-distribution-name-version-number/

    find_debian = strsplit(extract_distribution, "\t")[[1]][2]

    if (find_debian == "Debian") {

      run_test = FALSE
    }
  }

  return(run_test)
}


#========================================================================================================================================================================================
# message I received from CRAN on 11th April 2019:
#=================================================
#
# Prof Brian Ripley
# 11 Apr 2019, 09:00 (3 days ago)
# to Jeffrey, Emmanuel, Max, Hadley, David, Jim, Eugene, Bob, Erik, Shinya, Oliver, Kirill, Philippe, Scott, Noam, Gábor, Elin, Malte, Lorenz, Lionel, Sean, me, Florian, Edzer, CRAN
#
# This concerns packages
#
# CHNOSZ SARP.moodle caret dplyr fpeek fs gestalt hyphenatr incadata
# jpndistrict lobstr lucr pillar postGIStools rbison rchie rcmdcheck
# rcrossref rdatacite rerddap rgbif rplos skimr snakecase styler svglite
# swirl swirlify taxize testthat textTinyR traitdataform units urltools
#
# which are failing their checks in a strict Latin-1 locale: see the
# debian-clang results.  (Several of these seem to stem from vcr.)
#
# On Linux, such a locale can be ensured via LC_CTYPE=en_US (which may
#                                                            need installing for distros that micro-package).  AFAWK it cannot be
# done on Windows.
#
# 1) The character in don't is an (ASCII) apostrophe, not a right quote:
#
# don’t
#
# (with a right quote) is used in packages CHNOSZ dplyr ptstem skimr
# styler text2vec textTinyR (and others not failing).
#
# 2) en and em dashes are not portable, found in packages
# CHNOSZ SARP.moodle caret dplyr gestalt ptstem quanteda rchie rcrossref
# rdatacite rerddap rgbif rplos taxize textTinyR traitdataform units .
#
# 3) Using \uxxxx coding for non-ASCII chars in R character strings should
# help in some cases (see 'Writing R Extensions').
#
# Please correct before May 10 to safely retain the package on CRAN.
#========================================================================================================================================================================================
# message I received from CRAN on 13th Oct 2021:
#===============================================
#
# Prof Brian Ripley
#
# Dear maintainer,
#
# Please see the problems shown on
# <https://cran.r-project.org/web/checks/check_results_textTinyR.html>.
#
# Please correct before 2021-10-27 to safely retain your package on CRAN.
#
# The CRAN Team
#................................................

# The error occurred in:
# .
# .
# test-doc2vec.R : test id 112
#
# *** caught segfault ***
#   address 28, cause 'memory not mapped'
#
# Traceback:
#   1: word_vectors_methods(private$pre_proc, self$token_list, self$word_vector_FILE, method, private$unq_tok, private$inp_wv_dims, gtw_terms, gtw_weights, self$print_every_rows, self$verbose, threads, self$copy_data)
# 2: INIT$doc2vec_methods(method = "sum_sqrt")
# 3: eval(code, test_env)
# 4: eval(code, test_env)
# 5: withCallingHandlers({ eval(code, test_env) if (!handled && !is.null(test)) { skip_empty() }}, expectation = handle_expectation, skip = handle_skip, warning = handle_warning, message = handle_message, error = handle_error)
# 6: doTryCatch(return(expr), name, parentenv, handler)
# 7: tryCatchOne(expr, names, parentenv, handlers[[1L]])
# 8: tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
# 9: doTryCatch(return(expr), name, parentenv, handler)
# 10: tryCatchOne(tryCatchList(expr, names[-nh], parentenv, handlers[-nh]), names[nh], parentenv, handlers[[nh]])
# 11: tryCatchList(expr, classes, parentenv, handlers)
# 12: tryCatch(withCallingHandlers({ eval(code, test_env) if (!handled && !is.null(test)) { skip_empty() }}, expectation = handle_expectation, skip = handle_skip, warning = handle_warning, message = handle_message, error = handle_error), error = handle_fatal, skip = function(e) { })
# 13: test_code(desc, code, env = parent.frame(), reporter = reporter)
# 14: testthat::test_that("in case that the method parameter is 'sum_sqrt', it returns the correct output [ copy_data = TRUE ]", { INIT = Doc2Vec$new(token_list = tok_text, word_vector_FILE = PATH, copy_data = T) res = INIT$doc2vec_methods(method = "sum_sqrt") cat("test-doc2vec.R : test id", cnt_tsts, "\n") cnt_tsts <<- cnt_tsts + 1 testthat::expect_true(is.matrix(res) && nrow(res) == length(tok_text)) })
# 15: eval(code, test_env)
# 16: eval(code, test_env)
# 17: withCallingHandlers({ eval(code, test_env) if (!handled && !is.null(test)) { skip_empty() }}, expectation = handle_expectation, skip = handle_skip, warning = handle_warning, message = handle_message, error = handle_error)
# 18: doTryCatch(return(expr), name, parentenv, handler)
# 19: tryCatchOne(expr, names, parentenv, handlers[[1L]])
# 20: tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
# 21: doTryCatch(return(expr), name, parentenv, handler)
# 22: tryCatchOne(tryCatchList(expr, names[-nh], parentenv, handlers[-nh]), names[nh], parentenv, handlers[[nh]])
# 23: tryCatchList(expr, classes, parentenv, handlers)
# 24: tryCatch(withCallingHandlers({ eval(code, test_env) if (!handled && !is.null(test)) { skip_empty() }}, expectation = handle_expectation, skip = handle_skip, warning = handle_warning, message = handle_message, error = handle_error), error = handle_fatal, skip = function(e) { })
# 25: test_code(NULL, exprs, env)
# 26: source_file(path, child_env(env), wrap = wrap)
# 27: FUN(X[[i]], ...)
# 28: lapply(test_paths, test_one_file, env = env, wrap = wrap)
# 29: doTryCatch(return(expr), name, parentenv, handler)
# 30: tryCatchOne(expr, names, parentenv, handlers[[1L]])
# 31: tryCatchList(expr, classes, parentenv, handlers)
# 32: tryCatch(code, testthat_abort_reporter = function(cnd) { cat(conditionMessage(cnd), "\n") NULL})
# 33: with_reporter(reporters$multi, lapply(test_paths, test_one_file, env = env, wrap = wrap))
# 34: test_files(test_dir = test_dir, test_package = test_package, test_paths = test_paths, load_helpers = load_helpers, reporter = reporter, env = env, stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning, wrap = wrap, load_package = load_package)
# 35: test_files(test_dir = path, test_paths = test_paths, test_package = package, reporter = reporter, load_helpers = load_helpers, env = env, stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning, wrap = wrap, load_package = load_package, parallel = parallel)
# 36: test_dir("testthat", package = package, reporter = reporter, ..., load_package = "installed")
# 37: test_check("textTinyR")
# An irrecoverable exception occurred. R is aborting now ...
# Flavor: r-patched-solaris-x86
#................................................

# I omitted the test with id 328 from solaris testing:

is_solaris = function() {
  return(grepl('SunOS', Sys.info()['sysname']))                    # see: https://stackoverflow.com/a/23840917
}

# ========================================================================================================================================================================================

