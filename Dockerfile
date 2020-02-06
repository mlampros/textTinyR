FROM rocker/rstudio:devel 

 
LABEL maintainer='Lampros Mouselimis' 

 
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update && \ 
 apt-get install -y zlib1g-dev git-core pandoc pandoc-citeproc libcurl4-openssl-dev libssl-dev && \ 
 apt-get install -y sudo && \ 
 apt-get install -y libarmadillo-dev && \ 
 R -e "install.packages(c( 'Rcpp', 'R6', 'data.table', 'utils', 'RcppArmadillo', 'BH', 'testthat', 'covr', 'knitr', 'rmarkdown', 'remotes' ), repos =  'https://cloud.r-project.org/' )" && \ 
 R -e "remotes::install_github('mlampros/textTinyR', upgrade = 'always', dependencies = TRUE, repos = 'https://cloud.r-project.org/')" && \ 
 apt-get autoremove -y && \ 
 apt-get clean 

 
ENV USER rstudio 

 
