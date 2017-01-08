
## textTinyR
<br>

The textTinyR package consists of text processing functions for small or big data files. More details on the functionality of the textTinyR can be found in the package Vignette. The R package can be installed, in the following OS's: Linux, Mac and Windows. However, there are some limitations :

* there is no support for chinese, japanese, korean, thai or languages with ambiguous word boundaries.
* there is no support functions for utf-locale on windows, meaning only english character strings or files can be input and pre-processed.

<br>


On a Unix Operating System (OS) the boost library and especially the boost-locale are necessary,

* **Debian OS** : sudo apt-get install libboost-all-dev
* **Fedora OS** : yum install boost-devel
* **Mac OS** : brew install boost

<br>

To install the package from CRAN use, 

```R

install.packages("textTinyR")


```
<br>

and to download the latest version from Github use the *install_github* function of the devtools package,
<br><br>

```R

devtools::install_github('mlampros/textTinyR')


```
<br>

Use the following link to report bugs/issues,
<br><br>

