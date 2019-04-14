
#-------------------------
# languages for utf-locale
#-------------------------

languages = c("afrikaans", "arabic", "armenian", "basque", "bengali", "breton",
              "bulgarian", "catalan", "croatian", "czech", "danish",
              "dutch", "english", "estonian", "finnish", "french",
              "galician", "german", "greek", "hausa", "hebrew", "hindi", "hungarian",
              "indonesian", "irish", "italian", "latvian", "marathi", "norwegian", 
              "persian", "polish", "portuguese", "romanian", "russian", "slovak", 
              "slovenian", "somalia", "spanish", "swahili", "swedish", "turkish", "yoruba", "zulu")


exception_run_test = exception_latin1_locale_debian()

context('utf locale')


# cnt_tsts = 1


while(T) {
    
  #===============
  # error handling
  #===============
  
  testthat::test_that("in case that the language parameter is not a character string it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-utf_locale.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( utf_locale(language = NULL) )
  })
  
  
  testthat::test_that("in case that the language is not available it returns an error", {
    
    #-------------------------------------------------------------------- debug tests
    cat("test-utf_locale.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_error( utf_locale(language = "unknown") )
  })
  
  
  
  #====================
  # utf_locale function
  #====================
  
  testthat::test_that("all languages have a corresponding utf-encoding", {
    
    lst = list()
    
    for (i in 1:length(languages)) {
      
      lst[[i]] = utf_locale(language = languages[i])
    }
    
    #-------------------------------------------------------------------- debug tests
    cat("test-utf_locale.R : test id", cnt_tsts, "\n")
    
    cnt_tsts <<- cnt_tsts + 1
    #-------------------------------------------------------------------- 
    
    testthat::expect_true( length(unlist(lst)) == length(languages) && inherits(unlist(lst), c('character', 'vector')) )
  })
  
  
  
  
  
  #------------------------------
  # utf-8 works only on unix OS's
  #------------------------------
  
  
  
  if (.Platform$OS.type == "unix") {
    
    #===================================================
    # tokenization-transformation of different languages
    #===================================================
    
    
    # test utf-locale in different languages ( tokenization_transformation function )
    
    #--------
    # english
    #--------
    
    en_text = "The term planet is ancient, with ties to history, astrology, science, mythology, and religion. Several planets in the Solar System can be seen with the naked eye. 
    These were regarded by many early cultures as divine, or as emissaries of deities. As scientific knowledge advanced, human perception of the planets changed, incorporating a 
    number of disparate objects. In 2006, the International Astronomical Union (IAU) officially adopted a resolution defining planets within the Solar System. This definition is 
    controversial because it excludes many objects of planetary mass based on where or what they orbit. Although eight of the planetary bodies discovered before 1950 remain planets 
    under the modern definition, some celestial bodies, such as Ceres, Pallas, Juno and Vesta (each an object in the solar asteroid belt), and Pluto (the first trans-Neptunian object 
    discovered), that were once considered planets by the scientific community, are no longer viewed as such.The planets were thought by Ptolemy to orbit Earth in deferent and epicycle 
    motions. Although the idea that the planets orbited the Sun had been suggested many times, it was not until the 17th century that this view was supported by evidence from the first 
    telescopic astronomical observations, performed by Galileo Galilei. At about the same time, by careful analysis of pre-telescopic observation data collected by Tycho Brahe, Johannes 
    Kepler found the planets' orbits were not circular but elliptical. As observational tools improved, astronomers saw that, like Earth, the planets rotated around tilted axes, and some 
    shared such features as ice caps and seasons. Since the dawn of the Space Age, close observation by space probes has found that Earth and the other planets share characteristics such 
    as volcanism, hurricanes, tectonics, and even hydrology.Planets are generally divided into two main types: large low-density giant planets, and smaller rocky terrestrials. Under 
    IAU definitions, there are eight planets in the Solar System. In order of increasing distance from the Sun, they are the four terrestrials, Mercury, Venus, Earth, and Mars, then 
    the four giant planets, Jupiter, Saturn, Uranus, and Neptune. Six of the planets are orbited by one or more natural satellites."
    
    
    testthat::test_that("the function returns a vector of words if : the language is english, the utf_locale is en.UTF_8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = tokenize_transform_text(object = en_text, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "en.UTF_8", remove_char = "",
                               
                               remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                               
                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = FALSE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                               
                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                               
                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
      
      
      tmp_stopw = tokenize_transform_text(object = en_text, batches = NULL, read_file_delimiter = "\n", to_lower = TRUE, to_upper = FALSE, utf_locale = "en.UTF_8", remove_char = "",
                               
                               remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                               
                               split_separator = " \r\n\t.,;:()?!//", remove_stopwords = TRUE, language = "english", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                               
                               max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                               
                               stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE)
      
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "the" && tmp_stopw$token[1] == "term" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    #--------
    # greek
    #--------
  
    if (exception_run_test) {
    
      gr_text = "Ο όρος πλανήτης είναι αρχαία, με δεσμούς με την ιστορία, την αστρολογία, την επιστήμη, τη μυθολογία και τη θρησκεία. Αρκετοί πλανήτες στο Ηλιακό Σύστημα μπορεί να δει κανείς 
      με γυμνό μάτι. Αυτά θεωρήθηκαν από πολλούς νωρίς πολιτισμούς ως θεία, ή ως απεσταλμένοι των θεοτήτων. Όπως προηγμένες επιστημονικές γνώσεις, την ανθρώπινη αντίληψη των πλανητών άλλαξαν,
      ενσωματώνοντας μια σειρά ετερόκλητων αντικειμένων. Το 2006, η Διεθνής Αστρονομική Ένωση (IAU) ενέκρινε επίσημα ψήφισμα που ορίζει πλανήτες εντός του ηλιακού μας συστήματος. Ο ορισμός 
      αυτός είναι αμφιλεγόμενη, διότι αποκλείει πολλά αντικείμενα της πλανητικής μάζας με βάση το πού ή τι τροχιά. Παρά το γεγονός ότι οκτώ από τα πλανητικά σώματα ανακαλύφθηκαν πριν το 
      1950 παραμένουν πλανήτες κάτω από το σύγχρονο ορισμό, μερικοί ουράνια σώματα, όπως Ceres, Παλλάς, Juno και Vesta (κάθε ένα αντικείμενο στο ηλιακό αστεροειδή ζώνη), και τον Πλούτωνα 
      (το πρώτο trans-Neptunian αντικείμενο ανακαλύφθηκε), που κάποτε θεωρούνταν πλανήτες από την επιστημονική κοινότητα, δεν είναι πλέον αντιμετωπίζεται ως τέτοια. Οι πλανήτες θεωρήθηκαν
      από τον Πτολεμαίο σε τροχιά γύρω από τη Γη σε φέροντος και επικύκλου κινήσεις. Αν και η ιδέα ότι οι πλανήτες σε τροχιά γύρω από τον Ήλιο είχε προταθεί πολλές φορές, δεν ήταν μέχρι τον 
      17ο αιώνα ότι η άποψη αυτή υποστηρίζεται από αποδεικτικά στοιχεία από τις πρώτες τηλεσκοπικές αστρονομικές παρατηρήσεις, που εκτελούνται από Galileo Galilei. Την ίδια περίπου ώρα, με 
      προσεκτική ανάλυση των δεδομένων προ-τηλεσκοπικό παρατήρηση που συλλέγονται από Tycho Brahe, Johannes Kepler βρέθηκε τροχιές των πλανητών δεν ήταν κυκλική αλλά ελλειπτική. Όπως παρατήρησης 
      εργαλεία βελτιωθεί, οι αστρονόμοι είδαν ότι, όπως η Γη, οι πλανήτες περιστρέφονται γύρω από κλίση άξονες, και μερικά από κοινού χαρακτηριστικά όπως καλύμματα πάγου και εποχές. Από την αυγή
      της διαστημικής εποχής, στενή παρακολούθηση από διαστημοσυσκευές έχει βρεθεί ότι η Γη και οι άλλοι πλανήτες έχουν κοινά χαρακτηριστικά, όπως η ηφαιστειακή δραστηριότητα, τυφώνες, τεκτονική,
      και ακόμη και υδρολογίας. Οι πλανήτες σε γενικές γραμμές χωρίζονται σε δύο βασικές κατηγορίες: μεγάλα χαμηλής πυκνότητας γίγαντες πλανήτες, και μικρότερες βραχώδεις εξωγήινοι. Σύμφωνα με
      τους ορισμούς IAU, υπάρχουν οκτώ πλανήτες στο Ηλιακό Σύστημα. Για την αύξηση της απόστασης από τον Ήλιο, είναι οι τέσσερις εξωγήινοι, Ερμής, Αφροδίτη, Γη και τον Άρη, τότε οι τέσσερις
      γίγαντες πλανήτες, ο Δίας, Κρόνος, Ουρανός και Ποσειδώνας. Έξι από τα πλανήτες σε τροχιά γύρω από ένα ή περισσότερα φυσικά δορυφόρους."
      
      
      testthat::test_that("the function returns a vector of words if : the language is greek, the utf_locale is el_GR.UTF_8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = gr_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "el_GR.UTF_8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "greek", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = gr_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "el_GR.UTF_8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "greek", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "ο" && tmp_stopw$token[1] == "όρος" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
    }
    
    
    #--------
    # german
    #--------
    
    de_text = "Der Begriff Planet ist alt, mit Verbindungen zur Geschichte, Astrologie, Wissenschaft, Mythologie und Religion. Mehrere Planeten im Sonnensystem sind mit bloßem Auge zu sehen. 
    Diese wurden von vielen frühen Kulturen als göttlich oder als Emissäre von Gottheiten betrachtet. Mit fortschreitender wissenschaftlicher Erkenntnis veränderte sich die menschliche Wahrnehmung 
    der Planeten, die eine Anzahl unterschiedlicher Objekte enthielt. Im Jahr 2006 hat die Internationale Astronomische Union (IAU) offiziell eine Resolution angenommen, die Planeten innerhalb des 
    Sonnensystems definiert. Diese Definition ist umstritten, weil sie viele Objekte der planetaren Masse ausschließt, je nachdem, wo oder was sie umkreisen. Obwohl acht der vor 1950 entdeckten 
    Planeten nach der modernen Definition Planeten sind, sind einige Himmelskörper wie Ceres, Pallas, Juno und Vesta (jeweils ein Objekt im Solar-Asteroidengürtel) und Pluto (der erste trans-Neptunian
    Objekt entdeckt), die einst von der Wissenschaftsgemeinde als Planeten bezeichnet wurden, werden nicht mehr als solche betrachtet. Die Planeten wurden von Ptolemäus gedacht, um die Erde in deferenten
    und epicycle Bewegungen zu umkreisen. Obwohl die Vorstellung, dass die Planeten die Sonne umkreisen, schon viele Male vorgeschlagen worden war, wurde diese Ansicht erst im 17. Jahrhundert von den 
    ersten teleskopischen astronomischen Beobachtungen von Galileo Galilei untermauert. Zur gleichen Zeit, durch sorgfältige Analyse der vor teleskopischen Beobachtungsdaten, die von Tycho Brahe gesammelt 
    wurden, fanden Johannes Kepler, dass die Planetenbahnen nicht kreisförmig, sondern elliptisch waren. Als Beobachtungsinstrumente verbesserten, sahen Astronomen, daß, wie Erde, die Planeten um gekippte 
    Achsen gedreht wurden, und einige teilten solche Eigenschaften wie Eiskappen und Jahreszeiten. Seit dem Beginn des Weltraumzeitalters hat eine enge Beobachtung durch Raumsonden festgestellt, 
    dass die Erde und die anderen Planeten Eigenschaften wie Vulkanismus, Hurrikane, Tektonik und sogar Hydrologie teilen. Planeten sind in der Regel in zwei Hauptarten unterteilt: 
    große Low-Density-Riesenplaneten und kleinere felsige Landschaften. Unter IAU-Definitionen gibt es acht Planeten im Sonnensystem. In der Reihenfolge der zunehmenden Entfernung von der Sonne,
    sind sie die vier Terrestrischen, Merkur, Venus, Erde und Mars, dann die vier riesigen Planeten, Jupiter, Saturn, Uranus und Neptun. Sechs der Planeten werden von einem oder mehreren natürlichen
    Satelliten umkreist."
    
    
    
    testthat::test_that("the function returns a vector of words if : the language is german, the utf_locale is de_DE.UTF_8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = de_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "de_DE.UTF_8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "german", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = de_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "de_DE.UTF_8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "german", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "der" && tmp_stopw$token[1] == "begriff" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    #--------
    # french
    #--------
    
    fr_text = "Le terme planète est ancien, avec des liens à l'histoire, l'astrologie, la science, la mythologie et la religion. Plusieurs planètes dans le système solaire peuvent être vues à l'oeil nu.
    Celles-ci étaient considérées par beaucoup de premières cultures comme divines, ou comme émissaires des divinités. À mesure que les connaissances scientifiques avançaient, la perception humaine des 
    planètes changeait, incorporant un certain nombre d'objets disparates. En 2006, l'Union Astronomique Internationale (AIU) a officiellement adopté une résolution définissant des planètes dans le
    système solaire. Cette définition est controversée parce qu'elle exclut de nombreux objets de masse planétaire basés sur l'endroit ou ce qu'ils orbitent. Bien que huit des corps planétaires découverts 
    avant 1950 restent des «planètes» sous la définition moderne, certains corps célestes, tels que Ceres, Pallas, Juno et Vesta (chacun un objet dans la ceinture d'astéroïdes solaires) et Pluton 
    (le premier trans-néptunien Objet découvert), qui étaient autrefois considérés comme des planètes par la communauté scientifique, ne sont plus considérés comme tels. Les planètes ont été pensées
    par Ptolémée à l'orbite de la Terre dans les mouvements deferent et épicycle. Bien que l'idée que les planètes orbitent le Soleil ait été suggérée plusieurs fois, ce n'est qu'au 17ème siècle que 
    cette vue a été soutenue par la preuve des premières observations astronomiques télescopiques, réalisées par Galileo Galilei. À peu près en même temps, Johannes Kepler, par une analyse minutieuse 
    des données d'observation pré-télescopiques recueillies par Tycho Brahe, a constaté que les orbites des planètes n'étaient pas circulaires mais elliptiques. À mesure que les outils d'observation 
    se sont améliorés, les astronomes ont vu que, comme la Terre, les planètes tournaient autour d'axes inclinés, et certains partagent des caractéristiques telles que les calottes glaciaires et les 
    saisons. Depuis l'aube de l'ère spatiale, une observation étroite par des sondes spatiales a révélé que la Terre et les autres planètes partagent des caractéristiques telles que le volcanisme,
    les ouragans, la tectonique et même l'hydrologie. Les planètes sont généralement divisées en deux types principaux: les grandes planètes géantes à faible densité et les terres rocheuses les plus 
    petites. Selon les définitions de l'AIU, il existe huit planètes dans le système solaire. Dans l'ordre de la distance croissante du Soleil, ce sont les quatre terrestres, Mercure, Vénus, Terre et
    Mars, puis les quatre planètes géantes, Jupiter, Saturne, Uranus et Neptune. Six des planètes sont orbitées par un ou plusieurs satellites naturels."
    
    
    testthat::test_that("the function returns a vector of words if : the language is french, the utf_locale is fr_FR.UTF_8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = fr_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "fr_FR.UTF_8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "french", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = fr_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "fr_FR.UTF_8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "french", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "le" && tmp_stopw$token[1] == "terme" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    #--------
    # russian
    #--------
  
    if (exception_run_test) {
      
      rs_text = "Термин планета древней, со связями к истории, астрологии, науки, мифологии и религии. Несколько планет в Солнечной системе можно увидеть невооруженным глазом. Они были расценены многими 
      ранними культурами, как божественным, или как посланцы богов. По мере развития научных знаний, человеческое восприятие планет изменилось, включающая ряд разрозненных объектов. В 2006 году Международный 
      астрономический союз (IAU) официально принял резолюцию, определяющую планет в пределах Солнечной системы. Это определение является спорным, поскольку она исключает многие объекты планетарной массы, 
      основанные на том, где и что они вращаются. Хотя восемь планетарных тел, обнаруженных до 1950 года остаются «планеты» под современное определение, некоторые небесные тела, такие как Церера, Паллада, 
      Юнона и Веста (каждый объект в поясе астероидов солнечной) и Плутон (первая транснептунового объект обнаружен), которые были когда-то считались планеты со стороны научного сообщества, более не 
      рассматривается как таковой. Планеты были мысли Птолемея на орбиту Земли в эпицикл движений. Хотя идея, что планеты вокруг Солнца было предложено много раз, он не был до 17-го века, что эта точка 
      зрения была поддержана доказательствами из первых телескопических астрономических наблюдений, выполненных Галилео Галилеем. Примерно в то же время, путем тщательного анализа данных до телескопических 
      наблюдений, собранных Тихо Браге, Иоганн Кеплер нашел орбиты планет не были круглыми, но эллиптическая. Как наблюдательные инструменты улучшились, астрономы увидели, что, как и Земля, планеты вращается 
      вокруг осей наклона, а также некоторые общие такие признаки, как ледяных шапок и сезонов. С самого начала космической эры, тщательное наблюдение с помощью космических зондов обнаружил, что Земля и другие
      планеты разделяют такие характеристики, как вулканизм, ураганы, тектоники и даже гидрология. Планеты, как правило, делятся на два основных типа: большой низкой плотности планет-гигантов, и небольших скалистых 
      землянами. Под определениями IAU, есть восемь планет в Солнечной системе. В порядке возрастания расстояния от Солнца, они четыре землянами, Меркурий, Венера, Земля и Марс, то четыре планеты-гиганты, Юпитер,
      Сатурн, Уран и Нептун. Шесть планет вращался с помощью одного или более естественных спутников."
      
      
      
      testthat::test_that("the function returns a vector of words if : the language is russian, the utf_locale is ru_RU.UTF_8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = rs_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "ru_RU.UTF_8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "russian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = rs_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "ru_RU.UTF_8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "russian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "термин" && tmp_stopw$token[1] == "термин" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
    }
    
    
    #----------
    # bulgarian
    #----------
  
    if (exception_run_test) {
      
      bg_text = "Терминът планетата е древен, с връзки с история, астрология, наука, митология и религия. Няколко планети в Слънчевата система може да се види с невъоръжено око. Те са били считани от много ранни култури 
      като божествена, или като емисари на божества. Както научни знания напредна, човешкото възприятие на планетите променили, включващи редица разнородни предмети. През 2006 г., на Международния астрономически съюз (IAU)
      официално прие резолюция, определяща планети в Слънчевата система. Това определение е спорно, защото тя изключва много обекти на планетарна маса на базата на къде или какво те орбита. Въпреки, че осем от планетарни
      тела, открити преди 1950 остава планети в съвременната дефиниция, някои небесни тела, като Церера, Палада, Юнона и Веста (всеки един обект в Слънчевата астероидния пояс) и Плутон (първата транс-Нептун обект открил),
      които някога са били счита планети от научната общност, вече не се разглежда като такава. Планетите били смятани от Птолемей в орбита на Земята в епицикъл движения. Въпреки, че идеята, че планетите обикалят около
      Слънцето са били предложени много пъти, това не е до 17-ти век, че това мнение беше подкрепено с доказателства от първите телескопични астрономически наблюдения, извършени от Галилео Галилей. Горе-долу по същото 
      време, чрез внимателен анализ на данните от наблюдение на предварително телескопични, събрани от Тихо Брахе, Йоханес Кеплер намери орбити на планетите не са кръгли, но елипсовидни. Както наблюдателни инструменти 
      подобрени, астрономи видяха, че, подобно на Земята, планетите се въртят около наклонени оси, както и някои общи такива характеристики като ледените шапки и сезони. От зората на космическата ера, непосредствено 
      наблюдение от космически сонди е установил, че Земята и другите планети споделят характеристики като вулканизъм, урагани, тектониката, а дори и хидрология. Планетите са обикновено се разделят на два основни вида:
      големи ниска плътност гигантски планети, и по-малки скалисти земните. Съгласно определенията на МАС, има осем планети в Слънчевата система. С цел увеличаване на разстояние от Слънцето, те са четирите земни, 
      Меркурий, Венера, Земята и Марс, а след това на четири гигантски планети, Юпитер, Сатурн, Уран и Нептун. Шест от планетите са в орбита от един или повече естествени спътници."
      
      
      testthat::test_that("the function returns a vector of words if : the language is bulgarian, the utf_locale is bg_BG.UTF_8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = bg_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "bg_BG.UTF_8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "bulgarian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = bg_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "bg_BG.UTF_8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "bulgarian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "терминът" && tmp_stopw$token[1] == "терминът" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
    }
    
    
    #--------
    # turkish
    #--------
    
    trk_text = "Gezegen terimi, tarih, astroloji, bilim, mitoloji ve din bağlarıyla eski haldedir. Güneş Sistemi'ndeki birkaç gezegen çıplak gözle görülebilir. Bunlar birçok erken kültür tarafından tanrısal
    olarak veya tanrıların elçileri olarak görülüyordu. Bilimsel bilgi ilerledikçe, gezegenlerin insan algısı değişti ve bir takım farklı nesneler bir araya getirildi. 2006 yılında Uluslararası Astronomi Birliği 
    (IAU), Güneş Sistemi içindeki gezegenleri tanımlayan bir kararı resmi olarak onayladı. Bu tanım tartışmalıdır çünkü gezegensel kütlenin birçoğunun yörüngede nerede veya ne olduklarına dayanan 
    birçok nesne dışındadır. Her ne kadar 1950'den önce keşfedilen sekiz gezegen cismi modern tanımın altında gezegenler olarak kalmasına rağmen Ceres, Pallas, Juno ve Vesta (her biri güneş göktaşı 
    kemerinde bir nesne) gibi bazı gök cisimleri ve Pluto (ilk trans-Neptünyen Nesne keşfedildi), bilimsel topluluk tarafından bir zamanlar gezegen olarak kabul edildi, artık böyle görülmüyor. Gezegenler,
    Ptolemy tarafından, çılgın ve epik bisiklet hareketlerinde Dünya'nın etrafında yörüngedir diye düşünülüyordu. Güneş'le çevrili gezegenlerin defalarca önerildiği fikri olsa da, bu görüşün Galileo Galilei 
    tarafından gerçekleştirilen ilk teleskopik astronomik gözlemlerin kanıtlarıyla desteklendiği 17. yüzyıla kadar değildi. Aynı zamanda, Tycho Brahe tarafından toplanan ön-teleskopik gözlem verilerinin dikkatle 
    analizi ile Johannes Kepler gezegenlerin yörüngelerinin dairesel değil eliptik olduğunu keşfetti. Gözlem araçları gelişmişken, gökbilimciler Dünya gibi gezegenlerin eğik eksenler çevresinde döndüğünü 
    ve bazılarının buzullar ve mevsimler gibi özellikleri paylaştıklarını gördüler. Uzay Çağı'nın başlangıcından bu yana, uzay araştırmalarıyla yakın gözlem, Dünya'nın ve diğer gezegenlerin volkanizma, kasırgalar, 
    tektonik ve hatta hidroloji gibi özellikleri paylaştıklarını buldu. Gezegenler genel olarak iki ana gruba ayrılır: büyük düşük yoğunluklu dev gezegenler ve daha küçük kayalık bölgeler. IAU tanımları uyarınca 
    Güneş Sistemi'nde sekiz gezegen var. Güneş'ten uzaklaşabilmek için dört aklı, Merkür, Venüs, Dünya ve Mars, sonra dört dev gezegen, Jüpiter, Satürn, Uranüs ve Neptün'dür. Gezegenlerin altısı bir veya daha fazla 
    doğal uydunun yörüngesinde."
    
    
    testthat::test_that("the function returns a vector of words if : the language is turkish, the utf_locale is tr_TR.UTF_8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = trk_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "tr_TR.UTF_8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "turkish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = trk_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "tr_TR.UTF_8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = TRUE, language = "turkish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "gezegen" && tmp_stopw$token[1] == "gezegen" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    #-----------
    # afrikaans
    #-----------
    
    
    af_text = "Die term planeet is ou, met bande met die geskiedenis, astrologie, wetenskap, mitologie en godsdiens. Verskeie planete in die Sonnestelsel kan gesien word met die blote oog. Hierdie is deur baie vroeë kulture as goddelike, of as gesante van gode. As wetenskaplike kennis gevorderde, menslike persepsie van die planete verander, waarin 'n aantal uiteenlopende voorwerpe. In 2006 het die Internasionale Astronomiese Unie (IAU) amptelik 'n resolusie te definieer planete binne die sonnestelsel aangeneem. Hierdie definisie is omstrede omdat dit sluit baie voorwerpe van planetêre massa gebaseer op waar of wat hulle wentel. Hoewel agt van die planetêre liggame ontdek voor 1950 bly planete onder die moderne definisie, 'n hemelse liggame soos Ceres, Pallas, Juno en Vesta (elke 'n voorwerp in die son asteroïdegordel), en Pluto (die eerste Trans-Neptunus voorwerp ontdek), wat eens beskou planete deur die wetenskaplike gemeenskap, word nie meer beskou as such.The planete is deur Ptolemeus het gedink die Aarde wentel in uitscheidings en episikliese bewegings. Hoewel die idee dat die planete wentel die son het baie keer voorgestel, dit was nie tot die 17de eeu dat hierdie siening is deur bewyse uit die eerste teleskopiese sterrekundige waarnemings, wat uitgevoer word deur Galileo Galilei. Teen ongeveer dieselfde tyd, deur versigtige ontleding van die pre-teleskopiese waarneming data deur Tycho Brahe, Johannes ingesamel Kepler gevind die planete se wentelbane is nie omsendbrief maar elliptiese. Soos deur waarneming gereedskap verbeter, sterrekundiges sien dat, soos die aarde, die planete gedraai rondom gekantel byle, en 'n paar gedeel sulke eienskappe as yskappe en seisoene. Sedert die begin van die ruimte Age, het noukeurige waarneming deur ruimte sondes het bevind dat die aarde en die ander planete deel eienskappe soos soos vulkanisme, orkane, tektoniek, en selfs hydrology.Planets is oor die algemeen verdeel in twee hoof tipes: groot lae-digtheid reuse-planete, en kleiner rotsagtige terrestrials. onder IAU definisies, is daar agt planete in die Sonnestelsel. In volgorde van toenemende afstand vanaf die Son, hulle is die vier terrestrials, Mercurius, Venus, Aarde, en Mars, dan die vier reuse-planete, Jupiter, Saturnus, Uranus en Neptunus. Ses van die planete wentel deur een of meer natuurlike satelliete."
    
    
    testthat::test_that("the function returns a vector of words if : the language is afrikaans, the utf_locale is af_ZA.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = af_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "af_ZA.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "afrikaans", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = af_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "af_ZA.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "afrikaans", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "die" && tmp_stopw$token[1] == "term" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    #--------
    # arabic
    #--------
    
    
    if (exception_run_test) {
      
      ar_text = "الكوكب المصطلح القديم، التي لها علاقات مع التاريخ، وعلم التنجيم، والعلوم، والأساطير، والدين. ويمكن رؤية العديد من الكواكب في النظام الشمسي بالع
      ين المجرد"
      
      
      
      testthat::test_that("the function returns a vector of words if : the language is arabic, the utf_locale is ar_SA.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = ar_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "ar_SA.UTF-8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "arabic", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = ar_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "ar_SA.UTF-8", remove_char = "",
                                                        
                                                        remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                        
                                                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "arabic", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                        
                                                        max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                        
                                                        stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "الكوكب" && tmp_stopw$token[1] == "الكوكب" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
    }
    
    
    
    
    #----------
    # armenian
    #----------
  
    if (exception_run_test) {
      
      arm_text = "Տերմինը մոլորակը հին է, ինչպես կապերի պատմության, աստղագուշակության, գիտության, դիցաբանության եւ կրոնի: Մի քանի մոլորակները է արեգակնային համակարգի կարելի է տեսնել անզեն աչքով"
      
      
      testthat::test_that("the function returns a vector of words if : the language is armenian, the utf_locale is tr_TR.UTF_8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = arm_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "Hy-AM.UTF-8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "armenian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = arm_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "Hy-AM.UTF-8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "armenian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "տերմինը" && tmp_stopw$token[1] == "տերմինը" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
    }
    
    
    #--------
    # basque
    #--------
    
    
    bsk_text = "Epe Planeta zahar da, historia, hogei, zientzia, mitologia eta erlijioa loturak. Eguzki Sisteman Hainbat planeta izango da begi hutsez ikusi ahal. Hauek ziren hasieran kultura askotan jainkozko bezala, edo jainko ordezkariak benetan jotzen. ezagutza zientifiko aurreratu bezala, giza planeten pertzepzioa aldatu, bat sartuz objektu desberdinak kopurua. 2006an, Nazioarteko Astronomia Elkarteak (IAU) ofizialki ebazpen bat planetak definitzeko Eguzki Sistemaren barruan onartu. Definizio hau da polemikoa oinarritutako dute non edo zer orbitan on masa planeta objektu asko kanpoan geratzen delako. 1950 baino lehenago aurkitutako planeten gorputzen zortzi geratzen planetak arren the definizio modernoaren pean, zenbait erakunde zeruko, hala nola, Ceres, Palas, Juno eta Vesta (bakoitzak eguzki asteroide gerrikoan aurkitutako objektu bat), eta Pluton bezala (lehen trans-Neptunoz objektu aurkitu), ziren behin jotzen planeta komunitate zientifikoaren arabera, daude jada ez bezala such.The planetak ziren Ptolomeo pentsatu Earth orbitan deferent eta Epizikloa bisitaldiak mozioak. ideia hori planetak orbitatzen Eguzkia izan dira proposatutako hainbat aldiz izan arren, ez zen 17. mendean ikuspegi honetan frogak onartzen zen lehen arte teleskopikoak astronomikoa behaketa, Galileo Galileik egin. buruz bera pre-teleskopikoak behaketa datuen azterketa ibili Tycho Brahe, Johannes ek jasotzen dituen, denbora berean Kepler aurkitutako planeten orbitak ez ziren zirkular baina eliptikoak. behaketa-tresnak hobetu ahala, astronomoek ikusi, Lurraren antzera, planetak okertuta biratu inguruan ardatzak, eta zenbait partekatutako ezaugarri, hala nola izotz eta urtaroak bezala. Space Age egunsentian geroztik, hurbil behaketa espazio zundak arabera aurkitu ditu Earth eta beste planeta partekatzen duten ezaugarri, hala nola, handi-dentsitate txikiko planeta erraldoi, eta harritsu terrestrials txikiagoak: bolkanismoa, urakanak, tektonika, eta are hydrology.Planets bezala, oro har, bi mota nagusitan banatuta. Under IAU definizioak, Eguzki Sistemako zortzi planetak daude. distantzia handituz Eguzkiaren ordenatuta, lau terrestrials, Merkurio, Artizarra, Lurra, Marte eta, ondoren, ez dira lau planeta erraldoien, Jupiter, Saturno, Urano eta Neptuno. planeten Sei sateliteak natural bat edo gehiago orbitatzen daude."
    
    
    testthat::test_that("the function returns a vector of words if : the language is basque, the utf_locale is eu_ES.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = bsk_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "eu_ES.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "basque", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = bsk_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "eu_ES.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "basque", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "epe" && tmp_stopw$token[1] == "epe" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # bengali
    #--------
    
    if (exception_run_test) {
      
      bngl_text = "মেয়াদ গ্রহের প্রাচীন, ইতিহাস, জ্যোতিষশাস্ত্র, বিজ্ঞান, পুরাণ, এবং ধর্মের বন্ধন সঙ্গে. সৌরজগতের বেশ গ্রহ খালি চোখে দেখা যায়. এই অনেক তাড়াতাড়ি সংস্কৃতির ঐশ্বরিক হিসাবে, বা দেব-দেবীর প্রতিনিধিদের দ্বারা হিসাবে গণ্য করা হয়. বৈজ্ঞানিক জ্ঞান উন্নত হিসাবে, গ্রহের মানুষের উপলব্ধি পরিবর্তন, একটি একত্রিত অসম অবজেক্টের সংখ্যা. 2006 সালে, ইন্টারন্যাশনাল অ্যাস্ট্রোনমিক্যাল ইউনিয়ন (IAU) আনু"
      
      testthat::test_that("the function returns a vector of words if : the language is bengali, the utf_locale is bn_IN.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = bngl_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "bn_IN.UTF-8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "bengali", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = bngl_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "bn_IN.UTF-8", remove_char = "",
                                                        
                                                        remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                        
                                                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "bengali", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                        
                                                        max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                        
                                                        stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "মেয়াদ" && tmp_stopw$token[1] == "মেয়াদ" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
    }
    
    
    
    
    
    
    #--------
    # catalan
    #--------
    
    
    ctl_text = "El terme planeta és antic, amb vincles amb la història, l'astrologia, la ciència, la mitologia i la religió. Diversos planetes en el sistema solar es poden veure a simple vista. Aquests eren considerats per moltes cultures primerenques com divina, o com a emissaris de deïtats. A mesura que avança el coneixement científic, la percepció humana dels planetes va canviar, la incorporació d'una nombre d'objectes dispars. El 2006, la Unió Astronòmica Internacional (UAI) va adoptar oficialment una resolució que defineix els planetes en el sistema solar. Aquesta definició és controvertit, ja que exclou molts objectes de massa planetària en funció d'on o el que orbiten. Encara que vuit dels cossos planetaris descoberts abans de 1950 segueixen sent els planetes sota la definició moderna, alguns cossos celestes, com Ceres, Pal·las, Juno i Vesta (cadascuna un objecte al cinturó d'asteroides solar) i Plutó (el primer objecte transneptunià descobert), que una vegada van ser considerats planetes per la comunitat científica, ja no són considerats com planetes such.The es pensava per Ptolemeu en orbitar la Terra en epiciclo mocions. Tot i que la idea que els planetes giraven al voltant del Sol havia estat suggerit moltes vegades, no va ser fins al segle 17 que aquesta opinió va ser recolzada per l'evidència de la primera observacions astronòmiques telescòpiques, realitzades per Galileu Galilei. Gairebé al mateix temps, mitjançant l'anàlisi acurat de les dades d'observació pre-telescòpics recollits per Tycho Brahe, Johannes Kepler va trobar òrbites dels planetes no eren circulars sinó el·líptiques. Com a eines d'observació millorats, els astrònoms van veure que, com la Terra, els planetes girar al voltant d'eixos inclinats, i alguns compartida característiques tals com les capes de gel i les estacions. Des dels inicis de l'era espacial, l'observació propera per les sondes espacials ha trobat que la Terra i els altres planetes comparteixen característiques tals com el vulcanisme, tectònica, huracans, i fins i tot hydrology.Planets generalment es divideixen en dos tipus principals: els grans planetes gegants de baixa densitat, i els terrestres rocosos més petits. Sota les definicions de la UAI, hi ha vuit planetes del Sistema Solar. Per tal d'augmentar la distància des del Sol, que són els quatre terrestres, Mercuri, Venus, Terra i Mart, a continuació, els quatre planetes gegants, Júpiter, Saturn, Urà i Neptú. Sis dels planetes estan en òrbita pels un o més satèl·lits naturals."
    
    
    testthat::test_that("the function returns a vector of words if : the language is catalan, the utf_locale is ca_ES.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = ctl_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "ca_ES.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "catalan", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = ctl_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "ca_ES.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "catalan", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "el" && tmp_stopw$token[1] == "terme" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # croatian
    #--------
    
    
    kr_text = "Pojam planet je drevni, s vezama u povijesti, astrologije, znanosti, mitologije i religije. Nekoliko planeta u Sunčevom sustavu može se vidjeti golim okom. To su po mnogima ranih kultura božanske, ili kao poslanici božanstava. Kao što je znanstveno znanje napredovala, ljudska percepcija planeta promijenila, koji uključuje broj različitih objekata. U 2006. godini, Međunarodna astronomska unija (IAU) i službeno usvojio rezoluciju definira planeta unutar Sunčevog sustava. Ova definicija je sporno jer ne uključuje mnoge predmete planetarne mase na temelju toga gdje i što su u orbiti. Iako osam od planetarnih tijela otkrivena prije 1950. godine i dalje planete pod modernoj definiciji, neki nebeska tijela, poput Ceresa Pallas, Juno i Vesta (svaki objekt u asteroidnom pojasu solarne) i Pluton (prvi trans-neptunski objekt Otkrio), koje su se nekoć smatrala planete od strane znanstvene zajednice, više se ne gleda kao such.The planeti su mislili Ptolemej u orbitu Zemlje u popustljiv i putanja unutrašnjeg kruga prijedloga. Iako je ideja da se planeta kruži oko Sunca je predložio mnogo puta, to nije bilo sve do 17. stoljeća da je to pogled bio je poduprijeti dokazima iz prve teleskopske astronomska promatranja, u izvedbi Galileo Galilei. Otprilike u isto vrijeme, po pažljivoj analizi prethodno teleskopskih podacima promatranja prikupljenih Tycho Brahe, Johannes Kepler pronašao orbite planeta nisu bile kružne, ali eliptična. Kao opservacijskih alati poboljšana, astronomi su vidjeli da je, poput Zemlje, planete oko okretati naginje sjekirama, a neki dijeli takve značajke kao ledene kape i godišnja doba. Od zore svemirskog doba, u blizini promatranje od strane svemirskih sondi utvrđeno je da Zemlja i drugi planeti dijele karakteristike takvih kao vulkana, uragana, tektonike, pa čak i hydrology.Planets se općenito mogu podijeliti u dvije glavne vrste: velike niske gustoće divovskih planeta i manjih stjenovitih terrestrials. Pod IAU definicije, ima osam planeta u Sunčevom sustavu. U cilju povećanja udaljenosti od Sunca, oni su četiri terrestrials, Merkur, Venera, Zemlja i Mars, a zatim četiri divovskih planeta, Jupiter, Saturn, Uran i Neptun. Šest planeta su kruži jednog ili više prirodnih satelita."
    
    
    testthat::test_that("the function returns a vector of words if : the language is croatian, the utf_locale is hr_HR.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = kr_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "hr_HR.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "croatian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = kr_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "hr_HR.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "croatian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "pojam" && tmp_stopw$token[1] == "pojam" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # czech
    #--------
    
    
    cz_text = "Pod pojmem planeta je starobylý, s vazbami na historii, astrologie, vědy, mytologie a náboženství. Několik planet ve sluneční soustavě je možné vidět pouhým okem. Ty byly považovány mnoha časných kulturách jako boží, nebo jako vyslanci božstev. Jako vědecké poznatky postupovala, lidské vnímání planet změnil, zahrnujícími řada různých objektů. V roce 2006 Mezinárodní astronomická unie (IAU) oficiálně přijala rezoluci definující planety uvnitř sluneční soustavy. Tato definice je kontroverzní, protože to vylučuje mnohé objekty planetární hmotnosti podle toho, kde a co oni obíhají. Ačkoli osm planet objevených před rokem 1950 zůstává planety v moderní definici, někteří nebeská tělesa, jako je Ceres, Pallas, Juno a Vesta (každý objekt ve slunečním pásu asteroidů) a Pluto (první transneptunické těleso objevili), které byly kdysi považovány za planety vědeckou komunitou, již nejsou vnímány jako such.The planety byly považovány Ptolemaios na oběžnou dráhu Země v deferent a epicycle pohyby. I když představa, že planety obíhají kolem Slunce bylo navrženo mnohokrát, to nebylo až do 17. století, že tento názor byl podpořen důkazy z první teleskopická astronomická pozorování, provedené Galileo Galilei. Zhruba ve stejné době, podle pečlivé analýze pre-teleskopických údajů z pozorování shromážděných Tycho Brahe, Johannes Kepler našel obíhá planet byly kruhové, ale eliptické. Jako lepší pozorovací nástroje, astronomové viděli, že stejně jako Země, planety rotují kolem nakloněné osy a některé sdílel takové rysy jako ledové čepice a roční období. Od úsvitu kosmického věku, v blízkosti pozorování kosmických sond bylo zjištěno, že Země a ostatní planety má takové vlastnosti, jak sopečná činnost, hurikány, tektoniky, a dokonce i hydrology.Planets jsou obecně rozděleny do dvou hlavních typů: velké nízkou hustotou obřích planet, a menších skalních pozemšťanů. Pod definice IAU, je jich tam osm planet ve sluneční soustavě. V pořadí rostoucí vzdáleností od Slunce, jsou to čtyři pozemšťané, Merkur, Venuše, Země, Mars a poté čtyři obří planety, Jupiter, Saturn, Uran a Neptun. Šest z těchto planet obíhá jedna nebo více fyzických satelitů."
    
    
    testthat::test_that("the function returns a vector of words if : the language is czech, the utf_locale is cs_CZ.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = cz_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "cs_CZ.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "czech", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = cz_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "cs_CZ.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "czech", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "pod" && tmp_stopw$token[1] == "pojmem" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # danish
    #--------
    
    
    dns_text = "Udtrykket planet er gammelt, med bånd til historie, astrologi, videnskab, mytologi og religion. Adskillige planeter i Solsystemet kan ses med det blotte øje. Disse blev betragtet af mange tidlige kulturer som guddommelig, eller som udsendinge af guder. Som videnskabelig viden avancerede, menneskets opfattelse af planeterne ændret, indbygget antal forskellige objekter. I 2006 Den Internationale Astronomiske Union (IAU) vedtog officielt en resolution definerer planeter i solsystemet. Denne definition er kontroversielt, fordi det udelukker mange genstande af planetariske masse baseret på, hvor eller hvad de kredser. Selvom otte af de planetariske organer opdaget før 1950 forbliver planeter under den moderne definition, nogle himmellegemer, såsom Ceres, Pallas, Juno og Vesta (hver en genstand i sol asteroidebæltet), og Pluto (den første trans-neptunske objekt opdaget), der engang blev anset planeter af det videnskabelige samfund, ikke længere ses som such.The planeter var tænkt af Ptolemæus at kredse Jorden i epicykel bevægelser. Selv om tanken om, at planeterne kredsede Solen var blevet foreslået mange gange, det var først i det 17. århundrede, at dette synspunkt blev støttet af dokumentation fra den første teleskop astronomiske observationer, udført af Galileo Galilei. På nogenlunde samme tid, ved omhyggelig analyse af præ-teleskopiske observationsdata indsamlet af Tycho Brahe, Johannes Kepler fundet planeternes baner ikke var cirkulære men elliptisk. Som observationelle værktøjer forbedret, astronomer så, at, ligesom Jorden, roteres planeterne rundt vippes akser, og nogle delt sådanne funktioner som iskapper og årstider. Siden begyndelsen af Space Age, har tæt observation af rumsonder fundet, at Jorden og de andre planeter deler karakteristika sådanne som vulkanisme, orkaner, tektonik, og selv hydrology.Planets er generelt opdelt i to hovedtyper: store lav densitet gigantiske planeter, og mindre stenede terrestrials. Under IAU definitioner, der er otte planeter i solsystemet. For med stigende afstand fra Solen, de er de fire terrestrials, Merkur, Venus, Jorden og Mars, så de fire gigantiske planeter, Jupiter, Saturn, Uranus og Neptun. Seks af de planeter kredsede af en eller flere måne."
    
    
    testthat::test_that("the function returns a vector of words if : the language is danish, the utf_locale is da_DK.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = dns_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "da_DK.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "danish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = dns_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "da_DK.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "danish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "udtrykket" && tmp_stopw$token[1] == "udtrykket" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # dutch
    #--------
    
    
    dtc_text = "De term planeet is oud, met banden met de geschiedenis, astrologie, wetenschap, mythologie en religie. Aantal planeten in het Zonnestelsel kan worden gezien met het blote oog. Deze werden door velen beschouwd vroege culturen als goddelijk of als afgezanten van de goden. Als wetenschappelijke kennis gevorderd, de menselijke waarneming van de planeten veranderd, voorzien van een aantal verschillende voorwerpen. In 2006 heeft de Internationale Astronomische Unie (IAU) officieel een resolutie definiëren planeten in het zonnestelsel aangenomen. Deze definitie controversieel omdat het sluit allerlei voorwerpen van de planetaire massa op basis van waar of wat ze cirkelen. Hoewel acht van de planetaire lichamen ontdekt vóór 1950 blijven planeten onder de moderne definitie, sommige hemellichamen, zoals Ceres, Pallas, Juno en Vesta (elk een object in het zonnestelsel asteroïdengordel) en Pluto (de eerste transneptunisch object ontdekten), die ooit werden beschouwd als planeten door de wetenschappelijke gemeenschap, worden niet langer gezien als such.The planeten werden door Ptolemaeus dacht dat de aarde cirkelen in epicykels bewegingen. Hoewel het idee dat de planeten draaiden de Zon was vele malen gesuggereerd, het was pas in de 17e eeuw dat dit standpunt werd ondersteund door bewijs uit de eerste telescopische astronomische waarnemingen, uitgevoerd door Galileo Galilei. Op ongeveer hetzelfde moment, door een zorgvuldige analyse van de pre-telescopische observatiegegevens door Tycho Brahe, Johannes verzameld Kepler gevonden banen van de planeten waren niet rond, maar elliptisch. Als observationele instrumenten verbeterd, astronomen zagen dat, zoals de aarde, de planeten gedraaid rond gekanteld assen, en een aantal gedeelde functies als ijskappen en de seizoenen. Sinds het begin van de Space Age, heeft nauwe waarneming door ruimtesondes gevonden dat de aarde en de andere planeten te delen kenmerken zoals vulkanisme, orkanen, tektoniek en zelfs hydrology.Planets zijn over het algemeen onderverdeeld in twee hoofdtypen: grote low-density reuzenplaneten, en kleinere rotsachtige wezens. Onder IAU definities, zijn er acht planeten in het zonnestelsel. In volgorde van toenemende afstand tot de zon, zij zijn de vier wezens, Mercurius, Venus, Aarde en Mars, dan de vier grote planeten, Jupiter, Saturnus, Uranus en Neptunus. Zes van de planeten orbited door één of meer natuurlijke maan."
    
    
    testthat::test_that("the function returns a vector of words if : the language is dutch, the utf_locale is nl_NL.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = dtc_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "nl_NL.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "dutch", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = dtc_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "nl_NL.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "dutch", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "de" && tmp_stopw$token[1] == "term" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # estonian
    #--------
    
    if (exception_run_test) {
      
      est_text = "Mõiste planeedil on iidne, sidemed ajalugu, astroloogia, teaduse, mütoloogia ja religioon. Mitmed planeedid Päikesesüsteemi võib näha ka palja silmaga. Need peeti Paljudes varasemates kultuurides jumalik, või kui saadikud jumalused. Kuna teaduslikke teadmisi arenenud inimese taju planeedid muutunud, mis sisaldavad arvu erinevate objektide kohta. 2006. aastal Rahvusvahelise Astronoomia Liidu (IAU) ametlikult vastu resolutsiooni määratleda planeetide Päikesesüsteemi. See määratlus vastuoluline, sest see välistab palju objekte planeetide mass põhjal, kus või mida nad tiirlevad. Kuigi kaheksa planeetide organite avastas enne 1950. jäävad planeedid all tänapäeva määratlus, mõned taevakehad, nagu Ceres, Pallas, Juno ja Vesta (iga objekti Päikesesüsteemis asteroidi vöö) ja Pluto (esimene trans Neptuuni-objekti avastas), mis olid kord loetakse planeedid teadlaskonna poolt, ei ole enam vaadelda such.The planeedid arvati Ptolemaios orbiidil Maale Epitsükkel liikumisi. Kuigi idee, et planeedid orbited päike oli välja pakutud mitu korda, see oli alles 17. sajandil, et seda seisukohta toetavad tõendid esimene teleskoop astronoomilisi tähelepanekuid, läbi Galilei. Umbes samal ajal hoolika analüüsi eelnevalt teleskoop vaatlusandmeid on kogutud Tycho Brahe, Johannes Kepler leidnud planeetide orbiidid ei ole ringikujulised kuid elliptilised. Nagu vaatlusandmeid tööriistad paranenud, astronoomid näinud, et nii nagu Maal, planeedid pööratakse ümber kallutada telge, ja mõned jagatud selliseid funktsioone nagu jäämäed ja aastaaega. Kuna künnisel Space Age, järelvalve Kosmosesond leidis, et Maa ja teised planeedid jagada selliseid omadusi nagu vulkanism, orkaanid, tectonics ja isegi hydrology.Planets jagatakse tavaliselt kahte tüüpi: suured madala tihedusega hiidplaneedid ja väiksemad kivised terrestrials. alla IAU definitsioonid on kaheksa planeedid Päikesesüsteemi. Et suurendada kaugus Päikesest on need neli terrestrials, Merkuur, Veenus, Maa ja Mars, siis Nelja hiidplaneedid Jupiter, Saturn, Uraan ja Neptuun. Kuus planeedid orbited üks või mitu füüsilist satelliiti."
      
      
      testthat::test_that("the function returns a vector of words if : the language is estonian, the utf_locale is et_EE.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = est_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "et_EE.UTF-8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "estonian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = est_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "et_EE.UTF-8", remove_char = "",
                                                        
                                                        remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                        
                                                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "estonian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                        
                                                        max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                        
                                                        stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "mõiste" && tmp_stopw$token[1] == "mõiste" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
    }
    
    
    
    
    
    
    #--------
    # finnish
    #--------
    
    
    fns_text = "Termi planeetta on ikivanha, siteitä historia, astrologia, tiede, mytologia, ja uskonto. Useat planeettoja aurinkokunnan voidaan nähdä paljain silmin. Näitä pidettiin monet varhain kulttuureissa jumalallisena, tai lähettiläinä jumaluuksia. Tieteellistä tietoa kehittynyt, ihmisen käsitys planeettojen muuttui, joissa on useita keskenään esineitä. Vuonna 2006 Kansainvälinen tähtitieteellinen unioni (IAU) virallisesti päätöslauselman määritellään planeettoja aurinkokuntamme. Tämä määritelmä on kiistanalainen, koska se sulkee monet esineet planeetta massan mukaan, missä tai mitä he kiertävät. Vaikka kahdeksan planeettojen elinten löydetty ennen vuotta 1950 pysyvät planeetat alle moderni määritelmän, jotkut taivaankappaleiden kuten Ceres, Pallas, Juno ja Vesta (kukin objekti auringon asteroisivyöhyke) ja Pluto (ensimmäinen transneptuninen kohde löydettiin), jotka olivat aiemmin pidettiin planeettoja tiedeyhteisö, joita ei enää pidetä such.The planeetat arveltiin Ptolemaios on kiertänyt maapallon episykli liikkeet. Vaikka ajatus, että planeetat kiersi Auringon oli ehdotettu monta kertaa, se oli vasta 17-luvulla, että tämä näkemys tukivat todisteita ensimmäisestä teleskooppi tähtitieteellisiä havaintoja, suorittaa Galileo Galilei. Suurin piirtein samaan aikaan, jonka huolellinen analyysi ennalta teleskooppi havainto keräämiä Tycho Brahe, Johannes Kepler löytyi planeettojen kiertoradat eivät olleet pyöreitä, mutta elliptinen. Kuin havainnoivat välineet parantunut tähtitieteilijät näin, että, kuten Maa, planeetat kierretään kallistaa akselit ja jotkut yhteinen ominaisuuksia kuten jäätiköiden ja vuodenaikoina. Koska kynnyksellä Space Age, lähellä tarkkailu avaruusluotaimet on todennut, että Maan ja muut planeetat jakaa ominaisuuksia, kuten vulkanismi, hurrikaanit, tektoniikka, ja jopa hydrology.Planets ovat yleensä jaettu kahteen päätyyppiin: iso low-density jättiläisplaneetat, ja pienempiä kivinen terrestrials. Alla IAU määritelmät on kahdeksan planeettoja aurinkokunnan. Jotta lisätä etäisyys Auringosta, ne ovat neljä terrestrials, Merkurius, Venus, Maa ja Mars, sitten neljä jättiläisplaneettoihin, Jupiter, Saturnus, Uranus ja Neptunus. Kuusi planeetat kiertävät yhden tai useamman luonnollisen satelliitteja."
    
    
    testthat::test_that("the function returns a vector of words if : the language is finnish, the utf_locale is fi_FI.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = fns_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "fi_FI.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "finnish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = fns_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "fi_FI.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "finnish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "termi" && tmp_stopw$token[1] == "termi" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # galician
    #--------
    
    
    glc_text = "O termo planeta é antigo, con lazos coa historia, astroloxía, ciencia, mitoloxía e relixión. Varios planetas do Sistema Solar se pode ver a simple vista. Estas foron consideradas por moitas culturas precoces como divino, ou como emisarios de divindades. Como o coñecemento científico avanzado, a percepción humana dos planetas cambiou, incorporando un número de obxectos dispares. En 2006, a Unión Astronómica Internacional (IAU) adoptou oficialmente unha resolución que define planetas no Sistema Solar. Esta definición é controvertida porque exclúe moitos obxectos de masa planetaria en base a onde é o que orbitan. Aínda oito dos corpos planetarios descubertos antes de 1950 permanecen planetas baixo a definición moderna, algúns corpos celestes, como Ceres, Palas, Juno e Vesta (cada obxecto no cinto de asteroides solar) e Plutón (o primeiro obxecto trans-netuniano descuberto), que xa foron considerados planetas por parte da comunidade científica, xa non son vistos como planetas such.The foron pensados por Ptolomeo a orbitar a Terra Epiciclo movementos. Aínda que a idea de que os planetas orbitavam Sol fora suxerido moitas veces, non foi ata o século 17 que esta visión foi apoiada por evidencias desde o primeiro observacións astronómicas telescópicas, realizadas por Galileo Galilei. Máis ou menos ao mesmo tempo, unha análise coidadosa dos datos de observación condición telescópicos recollidos por Tycho Brahe, Johannes Kepler descubriu as órbitas dos planetas non eran circulares, pero elípticas. Como ferramentas observacionais mellorou, os astrónomos viron que, como a Terra, os planetas xirado en torno a eixes basculantes, e algúns compartido características tales como casquetes de xeo e as estacións. Dende o alvorecer do espazo, preto observación por sondas espaciais descubriu que a Terra e os outros planetas comparten características tales como vulcanismo, furacáns, tectónica, e mesmo hydrology.Planets son xeralmente divididos en dous tipos principais: grandes de baixa densidade planetas xigantes e pequenos terrestres rochosas. baixo configuración da UAI, hai oito planetas do Sistema Solar. En orde crecente de distancia do Sol, son os catro terrestres, Mercurio, Venus, Terra e Marte, logo os catro planetas xigantes, Xúpiter, Saturno, Urano e Neptuno. Seis dos planetas son orbitado por un ou máis satélites naturais."
    
    
    testthat::test_that("the function returns a vector of words if : the language is galician, the utf_locale is gl_ES.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = glc_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "gl_ES.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "galician", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = glc_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "gl_ES.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "galician", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "o" && tmp_stopw$token[1] == "termo" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # hausa
    #--------
    
    
    hsa_text = "Kalmar duniya ne tsoho, da dangantaka zuwa history, ilmin bokanci, kimiyya, mythology, kuma addini. Da dama taurari a cikin Solar System za a iya gani da ido tsirara. Wadannan da aka ɗauke shi daga mutane da yawa da wuri al'adu a matsayin allahntaka, ko kuma kamar yadda manzannin da abũbuwan bautãwa. Kamar yadda ilimin kimiyya ci gaba, mutum ji na taurari canza, kunsawa a yawan disparate abubuwa. A shekara ta 2006, International astronomical Union (IAU) hukuma soma wani ƙuduri fassara taurari a cikin Solar System. Wannan definition ne rigima domin zame yawa abubuwa na planetary taro bisa inda ko da abin da suka kewayewa. Ko da yake takwas na planetary jikin gano da 1950 zama taurari karkashin zamani definition, wasu wani sarari suKe jikinsu, kamar Ceres, Pallas, Juno da Vesta (kowane abu a cikin hasken rana asteroid bel), kuma Pluto (na farko trans-Neptunian abu gano), da aka taba gani, taurari da kimiyya al'umma, an daina kyan gani, kamar yadda such.The taurari da ake zaton da Talomi to kewayewa Duniya a deferent da epicycle motsi. Ko da yake da ra'ayin cewa taurari orbited Sun aka nuna sau da yawa, shi ne ba, har 17th karni da cewa wannan ra'ayi da aka goyan bayan shaida daga na farko telescopic astronomical lura, yi da Galileo Galilei. A game da wannan lokaci, da m bincike na pre-telescopic kallo data tattara zuwa Tycho Brahe, Johannes Kepler sami taurari 'falakinsu kasance ba madauwari amma elliptical. As observational kayayyakin inganta, Masana ilmin Taurari ga, kamar Duniya, da taurari juya a kusa da tilted gatura, da kuma wasu shared irin wannan fasali kamar kankara iyakoki da kuma yanayi. Tun da asuba na Space Age, kusa kallo da sarari bincike ya gano cewa, Duniya da kuma sauran taurari raba halaye irin kamar yadda volcanism, mahaukaciyar guguwa, tectonics, har ma hydrology.Planets suna kullum kasu kashi biyu main iri: m low-yawa giant taurari, da karami m terrestrials. A karkashin IAU ma'anar, akwai takwas taurari a cikin Solar System. Domin kara nisa daga Sun, su ne hudu terrestrials, Mercury, Venus, Duniya, da kuma Mars, to, hudu giant taurari, Jupiter, Rubin, Uranus, kuma Neptune. Shida daga cikin taurari suna orbited da daya ko fiye na halitta da tauraron dan adam."
    
    
    testthat::test_that("the function returns a vector of words if : the language is hausa, the utf_locale is ha_NG.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = hsa_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "ha_NG.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "hausa", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = hsa_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "ha_NG.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "hausa", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "kalmar" && tmp_stopw$token[1] == "kalmar" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # hebrew
    #--------
    
    if (exception_run_test) {
      
      hbr_text = "כוכב לכת הטווח הוא עתיק ימים, עם קשרים להיסטוריה, אסטרולוגיה, מדע, מיתולוגיה ודת. לכמה עולמות במערכת השמש ניתן לראות בעין בלתי מזוינת. אלה נחשבו על ידי תרבויות רבות כבר אלוהי, או כשליחים של אלים. ככל שידע מדעי מתקדם, תפיסה אנושית של כוכב לכת שינה, הכוללת"
      
      
      testthat::test_that("the function returns a vector of words if : the language is hebrew, the utf_locale is he_IL.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = hbr_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "he_IL.UTF-8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "hebrew", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = hbr_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "he_IL.UTF-8", remove_char = "",
                                                        
                                                        remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                        
                                                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "hebrew", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                        
                                                        max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                        
                                                        stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "כוכב" && tmp_stopw$token[1] == "כוכב" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
    
    
    
    
      #--------
      # hindi
      #--------
      
      
      hdi_text = "अवधि ग्रह प्राचीन है, इतिहास, ज्योतिष, विज्ञान, पुराण, और धर्म के संबंधों के साथ। सौर प्रणाली में कई ग्रहों नग्न आंखों से देखा जा सकता है। ये बहुत जल्दी संस्कृतियों परमात्मा के रूप में, या देवी-देवताओं के दूत के रूप में माना गया। वैज्ञानिक ज्ञान उन्नत, ग्रहों के मानव धारणा बदल गया है, एक को शामिल"
      
      
      testthat::test_that("the function returns a vector of words if : the language is hindi, the utf_locale is hi_IN.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = hdi_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "hi_IN.UTF-8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "hindi", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = hdi_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "hi_IN.UTF-8", remove_char = "",
                                                        
                                                        remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                        
                                                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "hindi", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                        
                                                        max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                        
                                                        stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "अवधि" && tmp_stopw$token[1] == "अवधि" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
      
      
      
      
      #--------
      # hungarian
      #--------
      
      
      hung_text = "A kifejezés bolygó ősi, a kapcsolat a történelem, az asztrológia, a tudomány, a mitológia és a vallás. Több bolygó a Naprendszer látható szabad szemmel. Ezeket sokak korai kultúrák, mint isteni, vagy küldötteinek istenségek. A tudományos ismereteket korszerű, emberi érzékelés a bolygók változott, amely magában foglalja a több különböző tárgyakat. 2006-ban a Nemzetközi Csillagászati Unió (IAU) hivatalosan elfogadott egy állásfoglalást meghatározó bolygó a Naprendszerben. Ez a meghatározás ellentmondásos, mert kizárja sok tárgyat a bolygó tömege alapján hol, mit körül keringenek. Bár nyolc bolygó szervek felfedezték 1950 előtt marad a bolygók alatt modern definíció egyes égitesteket, mint például Ceres, Pallas, Juno és Vesta (mindegyik tárgy a napenergia aszteroida öv), és a Plútó (az első Neptunuszon túli objektumok felfedezték), amelyek egykor a bolygók a tudományos közösség, az többé már nem tekinthető such.The bolygók gondolták Ptolemaiosz pályára Föld epiciklus mozgások. Bár az ötlet, hogy a bolygók körül kering a Nap is javasoltak többször, nem volt, amíg a 17. században, hogy ezt a nézetet támasztja alá bizonyítékokkal az első teleszkópos csillagászati megfigyelések által végzett Galileo Galilei. Körülbelül ugyanabban az időben, gondos elemzés előtti teleszkópos megfigyelés által összegyűjtött adatok Tycho Brahe, Johannes Kepler talált a bolygók pályája nem kör, hanem ellipszis alakú. Ahogy megfigyelési eszközök javult, a csillagászok látták, hogy, mint a Föld, a bolygók körül forog elforgatott tengelyek, és néhány megosztott olyan funkciók, mint jégsapkák és évszakok. Mivel a hajnal a Space Age, szoros megfigyelés által űrszondák megállapította, hogy a Föld és a többi bolygó hasonló jellemzőkkel, mint mint vulkáni, hurrikánok, tektonika, és még hydrology.Planets általában két fő típusa van: a nagy kis sűrűségű óriás bolygók, és a kisebb sziklás terrestrials. Alatt IAU meghatározások, nyolc bolygó a Naprendszerben. Növekvő sorrendjében a távolság a Nap, ők a négy terrestrials, Merkúr, Vénusz, Föld, Mars és, akkor a négy óriás bolygó, a Jupiter, a Szaturnusz, az Uránusz és a Neptunusz. Hat a bolygók körül kering egy vagy több természetes műholdak."
      
      
      testthat::test_that("the function returns a vector of words if : the language is hungarian, the utf_locale is hu.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = hung_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "hu.UTF-8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "hungarian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = hung_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "hu.UTF-8", remove_char = "",
                                                        
                                                        remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                        
                                                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "hungarian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                        
                                                        max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                        
                                                        stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "a" && tmp_stopw$token[1] == "kifejezés" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
    }
    
    
    
    
    #--------
    # indonesian
    #--------
    
    
    ind_text = "Planet istilah kuno, dengan ikatan sejarah, astrologi, ilmu pengetahuan, mitologi, dan agama. Beberapa planet di tata surya dapat dilihat dengan mata telanjang. Ini dianggap oleh banyak budaya awal ilahi, atau sebagai utusan dewa. Sebagai pengetahuan ilmiah maju, persepsi manusia dari planet berubah, menggabungkan jumlah objek yang berbeda. Pada tahun 2006, International Astronomical Union (IAU) secara resmi mengadopsi resolusi mendefinisikan planet dalam tata surya. Definisi ini kontroversial karena tidak termasuk banyak benda massa planet berdasarkan mana atau apa yang mereka mengorbit. Meskipun delapan dari tubuh planet ditemukan sebelum tahun 1950 tetap planet di bawah definisi modern, beberapa benda langit, seperti Ceres, Pallas, Juno dan Vesta (masing-masing objek di sabuk asteroid surya), dan Pluto (pertama trans-Neptunus objek ditemukan), yang pernah dianggap planet oleh komunitas ilmiah, tidak lagi dipandang sebagai such.The planet dianggap oleh Ptolemy mengorbit Bumi dalam relatif kecil dan epicycle gerakan. Meskipun ide bahwa planet-planet mengorbit Matahari telah menyarankan banyak kali, itu tidak sampai abad ke-17 bahwa pandangan ini didukung oleh bukti-bukti dari yang pertama pengamatan astronomi teleskopik, yang dilakukan oleh Galileo Galilei. Pada waktu yang sama, dengan analisis yang cermat dari data observasi pra-teleskopik dikumpulkan oleh Tycho Brahe, Johannes Kepler menemukan orbit planet-planet 'tidak melingkar tapi elips. Sebagai alat pengamatan ditingkatkan, astronom melihat bahwa, seperti Bumi, planet-planet diputar sekitar miring sumbu, dan beberapa bersama fitur seperti topi es dan musim. Sejak fajar Space Age, dekat observasi dengan pesawat antariksa telah menemukan bahwa bumi dan planet-planet lainnya memiliki karakteristik seperti sebagai vulkanisme, angin topan, tektonik, dan bahkan hydrology.Planets umumnya dibagi menjadi dua jenis utama: besar low-density planet raksasa, dan makhluk angkasa berbatu lebih kecil. Dibawah definisi IAU, ada delapan planet di Tata Surya. Dalam rangka peningkatan jarak dari Matahari, mereka adalah empat terrestrials, Merkurius, Venus, Bumi, dan Mars, maka empat planet raksasa, Jupiter, Saturnus, Uranus, dan Neptunus. Enam dari planet-planet yang mengorbit oleh satu atau lebih satelit alami."
    
    
    testthat::test_that("the function returns a vector of words if : the language is indonesian, the utf_locale is id_ID.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = ind_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "id_ID.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "indonesian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = ind_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "id_ID.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "indonesian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "planet" && tmp_stopw$token[1] == "planet" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # irish
    #--------
    
    if (exception_run_test) {
      
      irs_text = "Is é an téarma phláinéid ársa, le ceangail le stair, astrology, eolaíocht, miotaseolaíocht, agus reiligiún. Is féidir roinnt pláinéid sa Chóras Gréine a bheith le feiceáil leis an tsúil naked. Bhí siad seo a mheas ag cultúir luatha go leor mar Dhiaga, nó de réir mar emissaries de deities. Mar chun cinn eolas eolaíoch, dearcadh an duine ar na pláinéid athrú, a ionchorpraíonn líon na rudaí díchosúla. Sa bhliain 2006, ghlac an Astronomical Union Idirnáisiúnta (IAU) hoifigiúil rún pláinéid shainiú laistigh de Chóras Gréine. Tá an sainmhíniú conspóideach toisc eisiann sé go leor rudaí de mhais optional bunaithe ar an áit ina nó cad bhfithis acu. Cé ocht gcinn de na comhlachtaí optional amach roimh 1950 fós pláinéid faoin sainmhíniú nua-aimseartha, roinnt comhlachtaí celestial, ar nós Ceres, Pallas, Juno agus Vesta (gach rud sa crios astaróideach gréine ar), agus Plútón (an chéad rud tras-Neptunian fuair sé amach), go raibh pláinéid ag an bpobal eolaíoch a mheas aon uair amháin, a thuilleadh féachaint air mar a bhí cheap pláinéid such.The ag Ptolemy a bhfithis na Cruinne in deferent agus epicycle tairiscintí. Cé go raibh an smaoineamh go orbited na pláinéid an Ghrian ráite go minic, ní raibh sé go dtí go bhfuair an tuairim ó fhianaise ón gcéad an 17ú haois breathnuithe réalteolaíocha teileascópacha, le comhlíonadh ag Galileo Galilei. Ag thart ar an am céanna, trí anailís chúramach ar shonraí breathnadóireachta réamh-teileascópacha bhailigh Tycho Brahe, Johannes Chinn Kepler Ní raibh orbits na pláinéid 'ciorclach ach éilipseacha. Mar feabhsaithe uirlisí breathnóireachta, chonaic réalteolaithe sin, ar nós an Domhan, na pláinéid rothlú thart aiseanna tilted, agus roinnt roinnte gnéithe ar nós caipíní oighir agus séasúir. Ós rud é an tús an Aois Spás, tá gar breathnóireacht ag tóireadóirí spás fuarthas amach go roinnt ar an Domhan agus na pláinéid eile tréithe den sórt sin mar volcanism, hurricanes, theicteonaic, agus fiú hydrology.Planets a roinnt i dhá phríomhchineál: pláinéid íseal-dlúis mór ollmhór, agus terrestrials creagach lú. faoi sainmhínithe IAU, tá ocht pláinéid sa Chóras Gréine. Chun an bhfad ó láthair méadaitheach ón nGrian, tá siad na ceithre terrestrials, Mearcair, Véineas, Domhan, agus Mars, ansin na ceithre pláinéid fathach, Iúpatar, Satarn, Úránas, agus Neiptiún. Tá sé cinn de na pláinéid atá orbited amháin nó níos mó satailítí nádúrtha."
      
      
      testthat::test_that("the function returns a vector of words if : the language is irish, the utf_locale is ga_IE.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = irs_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "ga_IE.UTF-8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "irish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = irs_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "ga_IE.UTF-8", remove_char = "",
                                                        
                                                        remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                        
                                                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "irish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                        
                                                        max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                        
                                                        stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "is" && tmp_stopw$token[1] == "téarma" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
    }
    
    
    
    
    
    
    #--------
    # italian
    #--------
    
    
    itl_text = "Il termine pianeta è antica, con legami con la storia, l'astrologia, la scienza, la mitologia e la religione. Diversi pianeti del sistema solare può essere visto ad occhio nudo. Questi sono stati considerati da molte culture primi come divino, o come emissari di divinità. Come la conoscenza scientifica avanzata, la percezione umana dei pianeti è cambiato, che incorpora un numero di oggetti disparati. Nel 2006, l'International Astronomical Union (IAU) ha adottato ufficialmente una risoluzione che definisce pianeti nel Sistema Solare. Questa definizione è controverso perché esclude molti oggetti di massa planetaria in base a dove o ciò che orbita. Anche se otto dei corpi planetari scoperti prima del 1950 rimangono pianeti sotto la definizione moderna, alcuni corpi celesti, come Cerere, Pallade, Giunone e Vesta (ciascuno un oggetto nella fascia degli asteroidi solare), e Plutone (il primo oggetto trans-nettuniano scoperto), che una volta erano considerati pianeti da parte della comunità scientifica, sono visti non più come pianeti such.The sono stati pensati da Tolomeo a orbita terrestre in epiciclo e deferente movimenti. Anche se l'idea che i pianeti orbitavano il Sole era stato suggerito molte volte, non è stato fino al 17 ° secolo che questo punto di vista è stato sostenuto da prove a partire dal primo osservazioni astronomiche telescopiche, eseguite da Galileo Galilei. Più o meno nello stesso tempo, da un'attenta analisi dei dati di osservazione pre-telescopici raccolte da Tycho Brahe, Johannes Keplero ha trovato le orbite dei pianeti non erano circolare, ma ellittica. Come strumenti di osservazione migliorati, gli astronomi hanno visto che, come la Terra, i pianeti ruotato intorno assi inclinati, e un po ' condiviso tali caratteristiche come calotte di ghiaccio e le stagioni. Sin dagli albori dell'era spaziale, stretta osservazione da sonde spaziali ha scoperto che la Terra e gli altri pianeti condividono caratteristiche tali il vulcanismo, uragani, tettonica, e anche hydrology.Planets sono generalmente suddivisi in due tipi principali: grandi a bassa densità pianeti giganti, e più piccoli terrestri rocciose. Sotto definizioni IAU, ci sono otto pianeti del Sistema Solare. In ordine crescente di distanza dal Sole, sono i quattro terrestri, Mercurio, Venere, Terra e Marte, poi i quattro pianeti giganti, Giove, Saturno, Urano e Nettuno. Sei dei pianeti hanno in orbita uno o più satelliti naturali."
    
    
    testthat::test_that("the function returns a vector of words if : the language is italian, the utf_locale is it_IT.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = itl_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "it_IT.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "italian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = itl_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "it_IT.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "italian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "il" && tmp_stopw$token[1] == "termine" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # latvian
    #--------
    
    
    ltv_text = "Termins planēta ir sena, ar saitēm uz vēsturi, astroloģija, zinātnes, mitoloģiju un reliģiju. Vairākas planētas Saules sistēmā var redzēt ar neapbruņotu aci. Tie tika uzskatīti daudzi sākumā kultūru kā dievišķu, vai kā emisāri no dievības. Zinātniskas zināšanas uzlabotas, cilvēka uztvere no planētām mainīta, iekļaujot daudzām atšķirīgām objektiem. 2006. gadā Starptautiskā Astronomijas savienība (IAU) oficiāli pieņēma rezolūciju definē planētas ietvaros Saules sistēmā. Šī definīcija ir pretrunīga, jo tā izslēdz daudzus objektus planētu masu, pamatojoties uz to, kur un ko viņi orbītā. Lai gan astoņi no planētu iestāžu atklāti pirms 1950 paliek planētas atbilstoši mūsdienu definīciju, daži debess ķermeņi, piemēram, Cēres, Pallas, Juno un Vesta (katrs objekts Saules asteroīdu joslā), un Plutons (pirmais Transneptūna Objekts atklāja), kas reiz tika uzskatīts par planētu pēc zinātnieku aprindām, ir vairs uzskatīt par such.The planētas bija doma ar Ptolemaja orbītā Zemes izvadošs un epicikls kustības. Lai gan ideja, ka planētas orbited Sauli tika ierosināts daudzas reizes, tā nebija, līdz 17.gs., ka šis viedoklis ir pamatots ar pierādījumiem no pirmā teleskopiskie astronomijas novērojumi, ko Galileo Galilei veiktas. Aptuveni tajā pašā laikā, pēc rūpīgas analīzes iepriekš teleskopiskie novērojumu datiem Tycho Brahe, Johannes savākti Kepler atrastas planētas orbītas nebija apaļa, bet elipsveida. Uzlabota novērošanas instrumenti, astronomi ieraudzīja, ka, tāpat kā Zeme, planētas rotē ap noliekt asīm, un daži dalīta tādas funkcijas kā ledus cepures un gadalaikiem. Kopš dawn kosmosa laikmeta, netālu novērošana kosmosa zondes ir konstatējusi, ka Zeme un citas planētas dalīties iezīmes šādas kā volcanism, viesuļvētras, tektonika un pat hydrology.Planets parasti iedala divos galvenajos veidos: lieli zema blīvuma milzu planētām, un mazāku klinšu terrestrials. Zem IAU definīcijas, ir astoņi planētas Saules sistēmā. Lai palielinātu attālumu no Saules, tie ir četri terrestrials, Mercury, Venēra, Zeme un Marss, tad četri milzu planētas, Jupiters, Saturns, Urāns un Neptūns. Seši no planētām ir orbited ar vienas vai vairāku fizisko satelītiem."
    
    
    testthat::test_that("the function returns a vector of words if : the language is latvian, the utf_locale is lat.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = ltv_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "lat.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "latvian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = ltv_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "lat.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "latvian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "termins" && tmp_stopw$token[1] == "termins" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # marathi
    #--------
    
    
    if (exception_run_test) {
      
      mrt_text = "टर्म ग्रह इतिहास, फलज्योतिष, विज्ञान, पौराणिक आणि धर्म संबंध असलेल्या, पुरातन आहे. सूर्यमाला अनेक ग्रह उघड्या डोळा पाहिले जाऊ शकते. या अनेक लवकर संस्कृती दैवी, किंवा सर्व बाजूंना अनेक देवदेवतांच्या प्रेषित म्हणून ओळखले होते. वैज्ञानिक ज्ञान प्रगत म्हणून, ग्रह मानवी समज बदलला, एक अंतर्भूत पूर्णपणे वेगळया प्रकारचा वस्तुंची संख्या. 2006 मध्ये, आंतरराष्ट्रीय खगोलीय संघटना (इ.स.) अधिकृतपणे सूर्यमाला आत ग्रह व्याख्या ठराव दत्तक घेतले. या व्याख्या आहे"
      
      
      testthat::test_that("the function returns a vector of words if : the language is marathi, the utf_locale is mr_IN.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = mrt_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "mr_IN.UTF-8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "marathi", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = mrt_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "mr_IN.UTF-8", remove_char = "",
                                                        
                                                        remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                        
                                                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "marathi", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                        
                                                        max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                        
                                                        stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "टर्म" && tmp_stopw$token[1] == "टर्म" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
    }
    
    
    
    
    
    #--------
    # norwegian
    #--------
    
    
    nrv_text = "Begrepet planeten er gamle, med bånd til historie, astrologi, vitenskap, mytologi og religion. Flere planeter i solsystemet kan ses med det blotte øye. Disse ble ansett av mange tidlige kulturer som guddommelig, eller som utsendinger av guddommer. Som vitenskapelig kunnskap avanserte, menneskets oppfatning av planetene forandret, som omfatter en antall ulike stedene. I 2006, Den internasjonale astronomiske union (IAU) offisielt vedtatt en resolusjon som definerer planeter i solsystemet. Denne definisjonen er kontroversiell fordi den ekskluderer mange objekter av planetmasse basert på hvor eller hva de bane. Selv om åtte av planet organer oppdaget før 1950 er fortsatt planeter under moderne definisjonen, noen himmellegemer, som Ceres, Pallas, Juno og Vesta (hver en gjenstand i solenergiasteroidebeltet) og Pluto (den første transneptunsk objekt oppdaget), som en gang ble ansett planeter ved det vitenskapelige miljøet, er ikke lenger sett på som such.The planeter ble antatt av Ptolemaios i bane rundt Jorden i episyklus bevegelser. Selv om ideen om at planetene gikk i bane rundt Solen hadde blitt foreslått mange ganger, det var ikke før på 17-tallet at dette synet ble støttet av bevis fra den første teleskopiske astronomiske observasjoner, utført av Galileo Galilei. På omtrent samme tid, etter grundig analyse av pre-teleskopobservasjonsdata samlet av Tycho Brahe, Johannes Kepler funnet planetenes baner var ikke sirkulær, men elliptisk. Som observasjonsverktøy forbedret, astronomer så at, i likhet med Jorden, roteres planetene rundt vippet akser, og noen delt slike funksjoner som iskapper og årstider. Siden begynnelsen av Space Age, har nøye observasjon av romsonder funnet ut at Jorden og de andre planetene dele egenskaper som gjør som vulkanisme, orkaner, tektonikk, og selv hydrology.Planets er vanligvis delt inn i to hovedtyper: store low-density store planetene, og mindre steinete jordiske. Under IAU definisjoner, er det åtte planeter i solsystemet. For å øke avstanden fra Solen, de er de fire jordiske, Merkur, Venus, Jorden og Mars, deretter de fire store planetene, Jupiter, Saturn, Uranus og Neptun. Seks av planetene kretset av en eller flere naturlige satellitter."
    
    
    testthat::test_that("the function returns a vector of words if : the language is norwegian, the utf_locale is no_NO.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = nrv_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "no_NO.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "norwegian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = nrv_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "no_NO.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "norwegian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "begrepet" && tmp_stopw$token[1] == "begrepet" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # persian
    #--------
    
    
    if (exception_run_test) {
      
      prs_text = "این سیاره مدت باستان است، با روابط به تاریخ، طالع بینی، علم، اسطوره و مذهب. چندین سیاره در منظومه شمسی را می توان با چشم غیر مسلح دیده می شود. این در بسیاری از فرهنگها در اوایل الهی، و یا به عنوان فرستادگان خدایان در نظر گرفته شد. به عنوان دانش علمی پیشرفته، درک انسان از سیارات تغییر، ترکیب"
      
      
      testthat::test_that("the function returns a vector of words if : the language is persian, the utf_locale is fa_IR.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = prs_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "fa_IR.UTF-8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "persian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = prs_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "fa_IR.UTF-8", remove_char = "",
                                                        
                                                        remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                        
                                                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "persian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                        
                                                        max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                        
                                                        stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "این" && tmp_stopw$token[1] == "این" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
    }
    
    
    
    
    
    #--------
    # polish
    #--------
    
    
    pls_text = "Termin planeta jest starożytny, z powiązaniami z historii, astrologia, nauki, mitologii i religii. Kilka planet w Układzie Słonecznym można dostrzec gołym okiem. Zostały one uznane przez wielu wczesnych kulturach jako boskie, lub jako emisariuszy bóstw. Jako zaawansowana wiedza naukowa, ludzka percepcja planet zmieniło, zawierające liczbą innych obiektów. W 2006 roku Międzynarodowa Unia Astronomiczna (IAU) oficjalnie przyjęła uchwałę określającą planet w Układzie Słonecznym. Definicja ta jest kontrowersyjne, ponieważ wyklucza wiele obiektów o masie planetarnej na podstawie gdzie i co orbicie. Chociaż osiem ciał planetarnych odkrytych przed 1950 pozostają planety pod nowoczesnej definicji, niektóre ciała niebieskie, takie jak Ceres, Pallas, Juno i Westy (każdy obiekt w pasie asteroid słonecznego) i Plutona (pierwszy obiekt transneptunowy odkryta), które były kiedyś uważane planety przez środowisko naukowe, nie są już postrzegane jako such.The planety były uważane przez Ptolemeusza na orbicie Ziemi w epicykl ruchy. Chociaż idea, że planety na orbicie wokół Słońca były wielokrotnie sugerował, że nie było aż do 17 wieku, że pogląd ten został poparty dowodami od pierwszego teleskopowe obserwacje astronomiczne, wykonywane przez Galileusza. Mniej więcej w tym samym czasie dokładnej analizie danych pochodzących z obserwacji teleskopowych wstępnie zebranych przez Tycho Brahe, Johannes Kepler znalazł orbity planet nie były okrągłe, ale eliptyczny. Jako narzędzia obserwacyjne poprawie, astronomowie zobaczyli, że, podobnie jak Ziemia, planety obracać wokół osi wahań, a niektóre wspólne takie cechy jak czap lodowych i pór roku. Od zarania Ery Kosmicznej, ścisła obserwacja przez sondy kosmiczne odkrył, że Ziemia i inne planety dzielić takie cechy jak wulkanizm, huragany, tektoniki, a nawet hydrology.Planets można ogólnie podzielić na dwa główne rodzaje: duże niskiej gęstości olbrzymich planet skalistych i mniejszych Ziemian. Pod definicji IAU istnieje osiem planet w Układzie Słonecznym. W celu zwiększenia odległości od Słońca, są cztery Ziemianie, Merkury, Wenus, Ziemia, Mars, a, a następnie Cztery olbrzymie planety, Jowisz, Saturn, Uran i Neptun. Sześć z planet orbicie przez jednego lub więcej naturalnych satelitów."
    
    
    testthat::test_that("the function returns a vector of words if : the language is polish, the utf_locale is pl.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = pls_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "pl.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "polish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = pls_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "pl.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "polish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "termin" && tmp_stopw$token[1] == "termin" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # portuguese
    #--------
    
    
    prt_text = "O termo planeta é antigo, com laços com a história, astrologia, ciência, mitologia e religião. Vários planetas no Sistema Solar podem ser vistos a olho nu. Estas eram consideradas por muitas culturas primitivas como divinas, ou como emissários de divindades. À medida que o conhecimento científico avançava, a percepção humana dos planetas Número de objetos diferentes. Em 2006, a União Astronômica Internacional (IAU) adotou oficialmente uma resolução definindo planetas dentro do Sistema Solar. Esta definição é Controverso porque exclui muitos objetos da massa planetária baseado em onde ou o que eles orbitam. Embora oito dos corpos planetários descobertos antes de 1950 permaneçam planetas Sob a definição moderna, alguns corpos celestes, como Ceres, Pallas, Juno e Vesta (cada um objeto no cinturão de asteróides solares), e Plutão (o primeiro objeto trans-Neptuniano Descobertos), que uma vez foram considerados planetas pela comunidade científica, já não são vistos como tal. Os planetas foram pensados por Ptolomeu para orbitar a Terra em deferente e epiciclo Moções. Embora a idéia de que os planetas orbitavam o Sol havia sido sugerida muitas vezes, não foi até o século XVII que essa visão foi apoiada por evidências da primeira Observações astronômicas telescópicas, realizadas por Galileo Galilei. Por volta da mesma altura, através de uma análise cuidadosa dos dados de observação pré-telescópicos recolhidos por Tycho Brahe, Johannes Kepler descobriu que as órbitas dos planetas não eram circulares, mas elípticas. À medida que as ferramentas de observação melhoraram, os astrónomos viram que, como a Terra, os planetas giravam em torno de eixos inclinados e alguns Compartilharam características tais como calotas de gelo e estações. Desde a aurora da Era Espacial, observação próxima por sondas espaciais descobriu que a Terra e os outros planetas compartilham características tais como Como vulcanismo, furacões, tectônica e até hidrologia. Os planetas são geralmente divididos em dois tipos principais: grandes planetas gigantes de baixa densidade e terrestrials rochosos menores. Debaixo IAU definições, existem oito planetas no Sistema Solar. Em ordem de distância crescente do Sol, eles são os quatro terrestres, Mercúrio, Vênus, Terra e Marte, então Os quatro planetas gigantes, Júpiter, Saturno, Urano e Netuno. Seis dos planetas são orbitados por um ou mais satélites naturais."
    
    
    testthat::test_that("the function returns a vector of words if : the language is portuguese, the utf_locale is pt_PT.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = prt_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "pt_PT.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "portuguese", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = prt_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "pt_PT.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "portuguese", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "o" && tmp_stopw$token[1] == "termo" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # romanian
    #--------
    
    
    rmn_text = "Termenul planeta este vechi, cu legături de istorie, astrologie, știință, mitologie și religie. Mai multe tipuri de planete din Sistemul Solar poate fi văzut cu ochiul liber. Acestea au fost considerate de multe culturi timpurii divin, sau ca emisari divinităților. Pe măsură ce cunoașterea științifică avansată, percepția umană a planetelor schimbat, care încorporează un numărul de obiecte disparate. În 2006, Uniunea Astronomică Internațională (IAU) a adoptat în mod oficial o rezoluție care definește planete în cadrul Sistemului Solar. Această definiție este controversat, deoarece exclude multe obiecte de masă planetare bazate pe sau în cazul în care ceea ce orbita. Cu toate că opt dintre corpurile planetare descoperite înainte de 1950 rămân planete în conformitate cu definiția modernă, unele corpuri cerești, cum ar fi Ceres, Pallas, Juno si Vesta (fiecare un obiect din centura de asteroizi solar), si Pluto (primul obiect trans-Neptuniene a descoperit), care erau considerate planete de către comunitatea științifică, nu mai sunt privite ca planete such.The s-au gândit de Ptolemeu pe orbita Pământului în deferent și epiciclu mișcări. Deși ideea că planetele orbitat Soarele a fost sugerat de multe ori, nu a fost până în secolul al 17-lea că acest punct de vedere a fost sustinuta de dovezi din prima observații astronomice telescopice, efectuate de Galileo Galilei. Aproximativ în același timp, prin analiza atentă a datelor de observare pre-telescopice colectate de Tycho Brahe, Johannes Kepler a găsit orbita planetelor care nu erau circulare, ci eliptice. Ca instrumente de observație îmbunătățit, astronomii au văzut că, la fel ca Pamantul, planetele rotit în jurul axelor înclinat, iar unele partajat caracteristici cum ar fi capace de gheață și anotimpuri. De la începutul erei spațiale, aproape de observare de sonde spațiale a constatat că Pământul și celelalte planete împărtășesc caracteristici astfel ca vulcanismul, uragane, tectonica, și chiar hydrology.Planets sunt în general împărțite în două tipuri principale: cu densitate mică planete gigantice mari și pământeni stâncoase mai mici. Sub definiții IAU, există opt planete din Sistemul Solar. În ordinea crescătoare distanta de la Soare, acestea sunt cele patru pământeni, Mercur, Venus, Pământ și Marte, atunci cele patru planete gigantice, Jupiter, Saturn, Uranus și Neptun. Șase dintre planete sunt orbitat de către unul sau mai mulți sateliți naturali."
    
    
    testthat::test_that("the function returns a vector of words if : the language is romanian, the utf_locale is ro_RO.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = rmn_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "ro_RO.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "romanian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = rmn_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "ro_RO.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "romanian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "termenul" && tmp_stopw$token[1] == "termenul" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # slovak
    #--------
    
    
    svk_text = "Pod pojmom planéta je starobylý, s väzbami na históriu, astrológia, vedy, mytológie a náboženstva. Niekoľko planét v slnečnej sústave je možné vidieť voľným okom. Tie boli považované mnohými skorých kultúrach ako boží, alebo ako vyslanci božstiev. Ako vedecké poznatky postupovala, ľudské vnímanie planét zmenil, zahŕňajúcimi rad rôznych objektov. V roku 2006 Medzinárodná astronomická únia (IAU) oficiálne prijala rezolúciu definujúce planéty vnútri slnečnej sústavy. Táto definícia je kontroverzné, pretože to vylučuje mnohé objekty planetárne hmotnosti podľa toho, kde a čo oni obiehajú. Hoci osem planét objavených pred rokom 1950 zostáva planéty v modernej definícii, niektorí nebeské telesá, ako je Ceres, Pallas, Juno a Vesta (každý objekt v slnečnom pásu asteroidov) a Pluto (prvý transneptúnsky objekt objavili), ktoré boli kedysi považované za planéty vedeckou komunitou, už nie sú vnímané ako such.The planéty boli považované Ptolemaios na obežnú dráhu Zeme v deferentoch a epicycle pohyby. Aj keď predstava, že planéty obiehajú okolo Slnka bolo navrhnuté mnohokrát, to nebolo až do 17. storočia, že tento názor bol podporený dôkazy z prvej teleskopická astronomické pozorovania, vykonané Galileo Galilei. Zhruba v rovnakej dobe, podľa starostlivej analýze pre-teleskopických údajov z pozorovaní zhromaždených Tycho Brahe, Johannes Kepler našiel obieha planét boli kruhové, ale eliptické. Ako lepšie pozorovacie nástroje, astronómovia videli, že rovnako ako Zem, planéty rotujú okolo naklonenej osi a niektoré zdieľal také rysy ako ľadové čiapky a ročné obdobie. Od úsvitu kozmického veku, v blízkosti pozorovanie kozmických sond bolo zistené, že Zem a ostatné planéty má také vlastnosti, ako sopečná činnosť, hurikány, tektoniky, a dokonca aj hydrology.Planets sú všeobecne rozdelené do dvoch hlavných typov: veľké nízkou hustotou obrích planét, a menších skalných pozemšťanov. pod definícia IAU, je ich tam osem planét v slnečnej sústave. V poradí rastúcou vzdialenosťou od Slnka, sú to štyri pozemšťania, Merkúr, Venuša, Zem, Mars a potom štyri obrie planéty, Jupiter, Saturn, Urán a Neptún. Šesť z týchto planét obieha jedna alebo viac fyzických satelitov."
    
    
    testthat::test_that("the function returns a vector of words if : the language is slovak, the utf_locale is sk_SK.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = svk_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "sk_SK.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "slovak", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = svk_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "sk_SK.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "slovak", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "pod" && tmp_stopw$token[1] == "pojmom" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # slovenian
    #--------
    
    
    slvn_text = "Izraz planet je starodavno, z vezi z zgodovino, astrologijo, znanosti, mitologije in religije. Več planetov v Osončju je mogoče videti s prostim očesom. Ti so bili po mnenju mnogih zgodnjih kultur, kot božansko, ali so odposlanci iz božanstev. Kot je napredovala znanost, človeško dojemanje planetov spremenila, z vgrajeno Število neenakih predmetov. Leta 2006 je Mednarodna astronomska zveza (IAU) je uradno sprejel resolucijo, ki opredeljuje planetov v Osončju. Ta opredelitev je sporen, ker izključuje številne predmete planetarne mase, ki temeljijo na to, kje in kaj so orbito. Čeprav je osem od planetarnih teles odkrili pred letom 1950 ostali planeti v okviru sodobne definicije, nekateri nebesna telesa, kot so Ceres, Pallas, Juno in Vesta (vsak predmet v sončnem asteroidnem pasu), in Plutonom (prvi čezneptunsko telo odkril), ki so bile nekoč veljala za planete s strani znanstvene skupnosti, se ne obravnavajo več kot such.The planeti so mislili s Ptolemeja na orbiti Zemlje v Ustrežljiv in Pot notranjega kroga gibi. Čeprav je bila ideja, da se planeti krožili okoli Sonca predlagal večkrat, ni bilo do 17. stoletja, ki je bila ta pogled podprta z dokazi iz prvega teleskopski astronomska opazovanja, ki jih je Galileo Galilei opravljene. Ob približno istem času, po temeljiti analizi podatkov iz opazovanj pred teleskopski jih Tycho Brahe, Johannes zbranih Kepler našel orbite planetov niso krožna ampak eliptična. Kot izboljšalo opazovalne orodja, astronomi videli, da, tako kot Zemlja, zasukali planeti okoli nagiba osi, in nekateri skupna takšne značilnosti kot ledene kape in letnih časih. Ker zori vesoljske dobe, je natančno opazovanje vesoljske sonde je pokazala, da Zemlja in drugi planeti delijo značilnosti teh kot vulkanizem, orkani, tektonike in celo hydrology.Planets se na splošno delijo v dve glavni vrsti: velika nizke gostote velikih planetov, in manjše skalnate bitji. Spodaj opredelitve IAU obstaja osem planetov v Osončju. Da bi povečali razdaljo od Sonca, so štirje bitji, Merkur, Venera, Zemlja in Mars, nato štirje veliki planeti, Jupiter, Saturn, Uran in Neptun. Šest planetov so krožili z enim ali več naravnih satelitov."
    
    
    testthat::test_that("the function returns a vector of words if : the language is slovenian, the utf_locale is sl_SI.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = slvn_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "sl_SI.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "slovenian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = slvn_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "sl_SI.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "slovenian", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "izraz" && tmp_stopw$token[1] == "izraz" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # somalia
    #--------
    
    
    sml_text = "meeraha Erayga waa hore, la xiriir in taariikhda, falaga, sayniska, caddooyinka, iyo diinta. Dhowr meerayaasha ee System Solar ah waxaa lagu arki karaa isha oo qaawan. Kuwan waxaa loo arkaa by dhaqamo badan oo hore sida rabaani ah, ama sida ergo ka sokow. Sida cilmiga sayniska hormartay, aragtida aadanaha ee meerayaasha way beddeleen, daraysa a tirada walxaha kala duwan. Sannadkii 2006, Midowga ubadan Caalamiga ah (IAU) si rasmi ah qaatay qaraar qeexaya meerayaasha gudahood Nidaamka Solar. Taas macnaheedu waa muran, sababtoo ah waxaa laga saaray alaab badan oo mass Planetary ku salaysan meesha ama waxa ay falagiisuu. Inkasta oo sideed ka mid ah meydadka Planetary helay ka hor inta uusan 1950 ku sii meerayaasha hoos qeexidda casriga ah, jidhadhka qaar ka mid ah, sida Ceres, Pallas, Juno oo Vesta (kasta shay in suunka asteriyoodh qoraxda), iyo Pluto (shayga ugu horeeya trans-Neptunian helay), in mar loo arkaa meerayaasha bulshada sayniska, waxaa mar dambe ma arko meerayaasha such.The ayaa loo maleynayay by Blotemigii si kastana falagiisuu dhex Earth ee deferent iyo epicycle damacyadii. Inkastoo fikradda ah in meerayaasha ku orbited Sun ayaa la soo jeediyay marar badan, waxa aan ahaa ilaa qarnigii 17aad in view this waxaa caawinayso caddayntu hore indha-indhaynta ah sumalka uguma telescopic, sameeyaa by Galileo Galilei. Ugu ku saabsan waqti isku mid ah, by falanqaynta taxadir xogta daawashada pre-telescopic ururiyey by Tycho Brahe, Johannes Kepler helay Galaatiya ku meerayaasha 'ma ahaayeen wareegtada laakiin elliptical. Sida qalabka dheehidda hagaagtay, cirbixiyeyaasha arkay in, sida Earth, meerayaasha nasiisay agagaarka janjeerin faasas, iyo qaar ka mid ah wadaago sifooyinka sida saftay baraf iyo xilliyo. Tan iyo markii waagu of Age Space ah, daawashada dhow probes meel ayaa lagu ogaaday in Earth iyo meerayaasha kale wadaaga astaamo sida sida volcanism, duufaanada, tectonics, iyo xataa hydrology.Planets guud ahaan loo qaybiyaa laba nooc oo waaweyn: low-cufnaanta meerayaasha Rafaa waaweyn, iyo terrestrials dhagax yar. Under Macnaynta IAU, waxaa jira siddeed meerayaasha ee Nidaamka Solar. Si masaafada kordhaya ka Sun, oo iyagu waa afarta terrestrials, Mercury, Venus, Earth, iyo Mars, ka dibna afarta meerayaasha Rafaa, Jupiter, Saturn, Uranus, iyo Neptune. Lix ka mid ah meerayaasha waxaa orbited by hal ama in ka badan satalayt dabiiciga ah."
    
    
    testthat::test_that("the function returns a vector of words if : the language is somalia, the utf_locale is so_SO.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = sml_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "so_SO.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "somalia", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = sml_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "so_SO.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "somalia", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "meeraha" && tmp_stopw$token[1] == "meeraha" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # spanish
    #--------
    
    if (exception_run_test) {
      
      spn_text = "El término planeta es antiguo, con vínculos con la historia, la astrología, la ciencia, la mitología y la religión. Varios planetas del Sistema Solar pueden ser vistos a simple vista. Estas fueron consideradas por muchas culturas tempranas como divinas, o como emisarios de deidades. A medida que avanzaba el conocimiento científico, la percepción humana de los planetas cambiaba, Número de objetos dispares. En 2006, la Unión Astronómica Internacional (IAU) adoptó oficialmente una resolución que define planetas dentro del Sistema Solar. Esta definición es Controvertido porque excluye muchos objetos de masa planetaria basados en dónde o lo que orbitan. Aunque ocho de los cuerpos planetarios descubiertos antes de 1950 siguen siendo planetas Bajo la definición moderna, algunos cuerpos celestes, como Ceres, Pallas, Juno y Vesta (cada uno un objeto en el cinturón de asteroides solares), y Plutón (el primer objeto trans-neptuniano Descubiertos), que una vez fueron considerados planetas por la comunidad científica, ya no son vistos como tales. Los planetas fueron pensados por Ptolomeo para orbitar la Tierra en deferente y epiciclo Mociones Aunque la idea de que los planetas orbitaban al Sol había sido sugerida muchas veces, no fue hasta el siglo XVII que esta opinión fue apoyada por la evidencia de la primera Observaciones telescópicas astronómicas, realizadas por Galileo Galilei. Aproximadamente al mismo tiempo, mediante el análisis cuidadoso de los datos de observación pre-telescópicos recogidos por Tycho Brahe, Johannes Kepler encontró que las órbitas de los planetas no eran circulares sino elípticas. A medida que las herramientas de observación mejoraron, los astrónomos vieron que, al igual que la Tierra, los planetas giraban alrededor de ejes inclinados, y algunos Compartieron rasgos tales como casquillos de hielo y estaciones. Desde los albores de la era espacial, la observación cercana por las sondas espaciales ha encontrado que la tierra y los otros planetas comparten características tales Como el volcanismo, los huracanes, la tectónica, e incluso la hidrología. Los planetas se dividen generalmente en dos tipos principales: grandes planetas gigantes de baja densidad y terrestrials rocosos más pequeños. Debajo IAU definiciones, hay ocho planetas en el Sistema Solar. En orden de distancia cada vez mayor desde el Sol, son los cuatro terrestres, Mercurio, Venus, Tierra y Marte, entonces Los cuatro planetas gigantes, Júpiter, Saturno, Urano y Neptuno. Seis de los planetas están orbitados por uno o más satélites naturales."
      
      
      testthat::test_that("the function returns a vector of words if : the language is spanish, the utf_locale is es_ES.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
        
        tmp = suppressWarnings(tokenize_transform_text(object = spn_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "es_ES.UTF-8", remove_char = "",
                                                  
                                                  remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                  
                                                  split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "spanish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                  
                                                  max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                  
                                                  stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        tmp_stopw = suppressWarnings(tokenize_transform_text(object = spn_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "es_ES.UTF-8", remove_char = "",
                                                        
                                                        remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                        
                                                        split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "spanish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                        
                                                        max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                        
                                                        stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
        
        res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "el" && tmp_stopw$token[1] == "término" && 
          
          length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
        
        #-------------------------------------------------------------------- debug tests
        cat("test-utf_locale.R : test id", cnt_tsts, "\n")
        
        cnt_tsts <<- cnt_tsts + 1
        #-------------------------------------------------------------------- 
        
        testthat::expect_true( res_tes  )
      })
    }
    
    
    
    
    #--------
    # swahili
    #--------
    
    
    swh_text = "mrefu sayari ni ya kale, yenye uhusiano na historia, unajimu, sayansi, mythology na dini. sayari kadhaa katika mfumo wa jua inaweza kuonekana kwa jicho uchi. Hizi zilichukuliwa na tamaduni nyingi mapema kama Mungu, au kama wajumbe wa miungu. Kama elimu ya kisayansi juu, mtazamo wa binadamu wa sayari iliyopita, kuchanganya idadi ya vitu mbalimbali. Mwaka 2006, Astronomical Union International (IAU) rasmi iliyopitishwa azimio kufafanua sayari ndani ya mfumo wa jua. Ufafanuzi huu utata kwa sababu haihusishi vitu vingi wa habari dunia kulingana na pale au nini obiti. Ingawa nane ya miili ya dunia aligundua kabla ya 1950 kubaki sayari chini ya ufafanuzi wa kisasa, baadhi ya miili ya mbinguni, kama vile Ceres, Pallas, Juno na Vesta (kila kitu katika jua ukanda asteroid), na Pluto (kwanza trans-Neptunian kitu aligundua), kwamba walikuwa mara moja kuchukuliwa sayari na jamii ya kisayansi, ni tena kutazamwa kama sayari such.The walikuwa walidhani na Ptolemy obiti duniani katika deferent na epicycle mwendo. Ingawa wazo kwamba sayari orbited Sun alikuwa alipendekeza mara nyingi, haikuwa mpaka karne ya 17 kwamba mtazamo huu mara kwa msaada mkono ushahidi kutoka kwanza telescopic angani uchunguzi, walifanya kwa Galileo Galilei. Wakati huohuo, na uchambuzi makini wa data kabla ya telescopic uchunguzi zilizokusanywa na Tycho Brahe, Johannes Kepler kupatikana sayari 'orbits hawakuwa mviringo lakini elliptical. Kama zana za uchunguzi kuboreshwa, wanaanga aliona kwamba, kama Dunia, sayari kuzungushwa kuzunguka tilted shoka, na baadhi pamoja na makala kama vile zikienda na majira. Tangu asubuhi ya Nafasi Umri, uangalizi wa karibu na probes nafasi umegundua kwamba ardhi na sayari nyingine kushiriki tabia kama kama volcanism, vimbunga, tectonics, na hata hydrology.Planets ujumla kugawanywa katika aina mbili kuu: kubwa chini wiani sayari kubwa, na ndogo terrestrials miamba. chini ya IAU ufafanuzi, kuna sayari nane katika mfumo wa jua. Katika utaratibu wa kuongeza umbali kutoka jua, wao ni terrestrials nne, Mercury, Venus, Dunia, na Mars, kisha nne kubwa sayari, Jupiter, Saturn, Uranus, Neptune na. Six wa sayari ni orbited na moja au zaidi satelaiti ya asili."
    
    
    testthat::test_that("the function returns a vector of words if : the language is swahili, the utf_locale is sw_KE.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = swh_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "sw_KE.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "swahili", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = swh_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "sw_KE.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "swahili", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "mrefu" && tmp_stopw$token[1] == "mrefu" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # swedish
    #--------
    
    
    swd_text = "Termen planet är gamla, med anknytning till historia, astrologi, vetenskap, mytologi och religion. Flera planeter i solsystemet kan ses med blotta ögat. Dessa betraktades av många tidiga kulturer som gudomlig, eller som sändebud av gudar. Som vetenskaplig kunskap avancerade, mänsklig perception av planeterna förändrats, med en antal disparata objekt. År 2006 antog Internationella astronomiska unionen (IAU) officiellt en resolution som definierar planeter i solsystemet. Denna definition är kontroversiell eftersom den utesluter många föremål av planet massa beroende på var eller vad de kretsar. Även åtta av de planetariska kroppar upptäcktes före 1950 kvarstår planeter under modern definition, några himlakroppar, såsom Ceres, Pallas, Juno och Vesta (vardera ett objekt i solens asteroidbältet) och Pluto (den första transneptunskt objekt upptäckt), som en gång ansågs planeter av det vetenskapliga samfundet, inte längre ses som such.The planeter ansågs av Ptolemaios att kretsa kring jorden i epicykel rörelser. Även om idén att planeterna kretsade solen hade föreslagits många gånger, var det inte förrän på 17-talet att denna uppfattning stöddes av bevis från den första teleskop astronomiska observationer, som utförs av Galileo Galilei. Vid ungefär samma tid, genom noggrann analys av pre-teleskopobservationsdata som samlats in av Tycho Brahe, Johannes Kepler hittade planeternas banor inte cirkulära utan elliptisk. Som observations verktyg förbättrats, såg astronomer att som jorden roterade planeterna runt lutas axlar, och en del delade sådana funktioner som istäckena och årstider. Sedan början av rymdåldern, har noggrann observation av rymdsonder funnit att jorden och de andra planeterna dela sådana egenskaper som vulkanism, orkaner, tektonik, och även hydrology.Planets generellt delas in i två huvudtyper: stora låg densitet jätteplaneter, och mindre steniga jordingar. Under IAU definitioner finns det åtta planeter i solsystemet. För att öka avståndet från solen, de är fyra jordingar, Merkurius, Venus, Jorden och Mars, sedan de fyra jätteplaneter, Jupiter, Saturnus, Uranus och Neptunus. Sex av planeterna kretsade av en eller flera fysiska satelliter."
    
    
    testthat::test_that("the function returns a vector of words if : the language is swedish, the utf_locale is sv_SE.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = swd_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "sv_SE.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "swedish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = swd_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "sv_SE.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "swedish", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "termen" && tmp_stopw$token[1] == "termen" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # yoruba
    #--------
    
    
    yrb_text = "Ni oro aye ni atijọ, pẹlu seése to itan, Afirawọ, Imọ, atijọ, ati esin. Orisirisi awọn aye ni oorun System le ri pẹlu ni ihooho oju. Awọn wọnyi ni won kasi nipa ọpọlọpọ awọn tete asa bi Ibawi, tabi bi iranß ti deities. Bi ijinle sayensi imo ni ilọsiwaju, eniyan Iro ti awọn aye yi pada, palapapo a nọmba ti disparate ohun. Ni 2006, awọn International astronomical Union (IAU) ifowosi gba kan ti o ga asọye aye laarin awọn oorun System. Yi definition ni ti ariyanjiyan nitori ti o excludes ọpọlọpọ awọn ohun ti Planetary ibi-da lori ibi ti tabi ohun ti won yipo. Biotilejepe mẹjọ ninu awọn ti Planetary ara awari ki o to 1950 wa aye labẹ awọn igbalode definition, diẹ ninu awọn celestial ara, gẹgẹ bi awọn Ceres, Pallas, Juno ati Vesta (kọọkan ohun ni oorun asteroid igbanu), ati Pluto (akọkọ kabo-Neptunian ohun awari), ti won ni kete ti kà aye nipa awọn ijinle sayensi awujo, ti wa ni ko si ohun to bojuwo bi such.The aye won ro nipa Ptolemy to yipo Earth ni deferent ati epicycle ìsépo. Biotilejepe awọn agutan ti awọn aye orbited awọn Sun ti a ti daba ọpọlọpọ igba, o je ko titi ti 17th orundun ti yi view ti a ni atilẹyin nipasẹ eri lati akọkọ telescopic astronomical akiyesi, nipasẹ ošišẹ ti Galileo Galilei. Ni nipa akoko kanna, nipa ṣọra igbekale ami-telescopic akiyesi data gbà nipa Tycho Brahe, Johannes Kepler ri awọn aye 'orbits wà ko ipin sugbon elliptical. Bi observational irinṣẹ dara si, astronomers ri pe, bi Earth, nyí n ni ayika tilted ãke, ati diẹ ninu awọn pín iru ẹya ara ẹrọ bi yinyin bọtini ati ki akoko. Niwon awọn Asaale ti awọn ti Space-ori, sunmọ akiyesi nipa aaye wadi ti ri pe Earth ati awọn miiran aye orun pin abuda iru bi volcanism, hurricanes, tectonics, ati paapa hydrology.Planets ti wa ni gbogbo pin si meji akọkọ orisi: tobi kekere-iwuwo omiran aye orun, ati ki o kere Rocky terrestrials. labẹ IAU itumo, nibẹ ni o wa mẹjọ aye ni oorun System. Ni ibere ti jijẹ ijinna lati Sun, won ni o wa mẹrin terrestrials, Mercury, Venus, Earth, ati Mars, ki o si awọn mẹrin omiran aye orun, Jupiter, Satouni, Uranus, ati Neptune. Mefa ti awọn aye ti wa ni orbited nipasẹ ọkan tabi diẹ ẹ sii adayeba satẹlaiti."
    
    
    testthat::test_that("the function returns a vector of words if : the language is yoruba, the utf_locale is yo_NG.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = yrb_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "yo_NG.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "yoruba", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = yrb_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "yo_NG.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "yoruba", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "ni" && tmp_stopw$token[1] == "oro" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
    
    
    
    
    
    
    #--------
    # zulu
    #--------
    
    
    zl_text = "Iplanethi gama yasendulo, ne izibopho umlando, ukubhula ngezinkanyezi, isayensi, izinganekwane, futhi inkolo. amaplanethi amaningana Solar System kuyabonakala ngeso lenyama. Lezi zazibhekwa namasiko eminingi yakudala njengoba yaphezulu, noma njengoba izithunywa onkulunkulu. Njengoba ulwazi lwesayensi isihambile, bezibona wobuntu amaplanethi washintsha, ufake lezo a Inani lezinto disparate. Ngo-2006, i-International Astronomical Union (IAU) ngokusemthethweni lathatha isinqumo belinganisa amaplanethi ngaphakathi System Solar. Lenchazelo impikiswano ngoba akufaki izinto eziningi mass kwamaplanethi esekelwe kuphi noma ukuthi orbit. Nakuba eziyisishiyagalombili izidumbu kwamaplanethi bathola phambi 1950 ahlale namaplanethi ngaphansi kwencazelo zanamuhla, abanye zasemkhathini, ezifana Ceres, uPallas, Juno futhi Vesta (ngamunye intfo solar nedwala elisemkhathini Bopha ibhande), kanye Pluto (the object lokuqala trans-Neptunian wathola), eyake kubhekwe amaplanethi by the abasemkhakheni wezesayensi, behlala ndawonye bengashadile akusashiwo ibhekwa njengevela such.The amaplanethi kwakucatshangwa by uPtolemy ukuzungeza Emhlabeni in deferent futhi epicycle iminyakazo. Nakuba lo mqondo wokuthi amaplanethi orbited ilanga lase kwasikiselwa izikhathi eziningi, kwaze kwaba ngekhulu le-17 ukuthi lo mbono okusekelwe ebufakazini kwasekuqaleni oyiphawulile telescopic yezinkanyezi, eyenziwa yi uGalileo Galilei. Cishe ngaso leso sikhathi esifanayo, by nokuhlaziya ngokucophelela idatha observation pre-telescopic eqoqwe by Tycho Brahe, Johannes Kepler ezitholakele omise amaplanethi bakaJesu babengenzi isiyingi kodwa okweqanda. Njengoba amathuluzi elibukelayo yathuthuka, izazi zezinkanyezi wabona ukuthi, efana Earth, amaplanethi izungeziswe ngokuvumelana nxazonke esitshekile izimbazo, futhi abanye wabelane izici ezifana yiqhwa nezinkathi. Njengoba sekuzoqala Space Age, observation close by isikhala iphenya uye wathola ukuthi Umhlaba kwamanye amaplanethi ukwabelana izici ezinjalo njengoba mlilo, izivunguvungu, nokunyakaza kwezingqimba zomhlaba, ngisho hydrology.Planets ngokuvamile ihlukaniswe izinhlobo ezimbili eziyinhloko: low-inhlanganiso amaplanethi ezinkulu giant, futhi terrestrials ezincane rocky. Ngaphansi IAU izincazelo, kukhona amaplanethi eyisishiyagalombili ohlelweni Solar. Ukuze okwandayo ibanga eLangeni, bayizithunywa terrestrials ezine, Mercury, Venus, uMhlaba ne-Mars, khona-ke amaplanethi ezine giant, Jupiter, Saturn, i-Uranus ne-Neptune. Abayisithupha amaplanethi orbited ngu eyodwa noma ngaphezulu iziphuphutheki zemvelo."
    
    
    testthat::test_that("the function returns a vector of words if : the language is zulu, the utf_locale is zu_ZA.UTF-8, the string split is TRUE, the to_upper is TRUE and the remove_stopwords parameter is TRUE", {
      
      tmp = suppressWarnings(tokenize_transform_text(object = zl_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "zu_ZA.UTF-8", remove_char = "",
                                                
                                                remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                
                                                split_separator = " \r\n\t.,;:()?!//", remove_stopwords = F, language = "zulu", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                
                                                max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                
                                                stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      tmp_stopw = suppressWarnings(tokenize_transform_text(object = zl_text, batches = NULL, read_file_delimiter = "\n", to_lower = T, to_upper = F, utf_locale = "zu_ZA.UTF-8", remove_char = "",
                                                      
                                                      remove_punctuation_string = FALSE, remove_punctuation_vector = TRUE, remove_numbers = FALSE, trim_token = TRUE, split_string = TRUE, 
                                                      
                                                      split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T, language = "zulu", min_num_char = 1, max_num_char = Inf, stemmer = NULL, min_n_gram = 1,
                                                      
                                                      max_n_gram = 1, skip_n_gram = 1, skip_distance = 0, n_gram_delimiter = " ", concat_delimiter = NULL, path_2folder = "", stemmer_ngram = 4, stemmer_gamma = 0.0, 
                                                      
                                                      stemmer_truncate = 3, stemmer_batches = 1, threads = 1, vocabulary_path_file = NULL, verbose = FALSE))
      
      res_tes = names(tmp) == 'token'  && length(tmp) == 1 && is.vector(tmp$token) && length(tmp$token) > 1 && tmp$token[1] == "iplanethi" && tmp_stopw$token[1] == "iplanethi" && 
        
        length(tmp$token) > length(tmp_stopw$token) && is.character(tmp$token) && class(tmp) == "tokenization and transformation"
      
      #-------------------------------------------------------------------- debug tests
      cat("test-utf_locale.R : test id", cnt_tsts, "\n")
      
      cnt_tsts <<- cnt_tsts + 1
      #-------------------------------------------------------------------- 
      
      testthat::expect_true( res_tes  )
    })
  
  }
  
  break    # exit loop for tests ( count iterations / tests for debugging )
}
