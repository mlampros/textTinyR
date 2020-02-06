
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/textTinyR)](http://cran.r-project.org/package=textTinyR)
[![Travis-CI Build Status](https://travis-ci.org/mlampros/textTinyR.svg?branch=master)](https://travis-ci.org/mlampros/textTinyR)
[![codecov.io](https://codecov.io/github/mlampros/textTinyR/coverage.svg?branch=master)](https://codecov.io/github/mlampros/textTinyR?branch=master)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mlampros/textTinyR?branch=master&svg=true)](https://ci.appveyor.com/project/mlampros/textTinyR/branch/master)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/textTinyR?color=blue)](http://www.r-pkg.org/pkg/textTinyR)
<a href="https://www.buymeacoffee.com/VY0x8snyh" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" height="21px" ></a>
[![Dependencies](https://tinyverse.netlify.com/badge/textTinyR)](https://cran.r-project.org/package=textTinyR)
[![](https://img.shields.io/docker/automated/mlampros/texttinyr.svg)](https://hub.docker.com/r/mlampros/texttinyr)


## textTinyR
<br>

The *textTinyR* package consists of text processing functions for small or big data files. More details on the functionality of textTinyR can be found in [blog-post1](http://mlampros.github.io/2017/01/05/textTinyR_package/) and [blog-post2](http://mlampros.github.io/2018/04/04/extending_textTinyR_package/). The R package can be installed, in the following Operating Systems: Linux, Mac and Windows. However, there is one limitation : *chinese*, *japanese*, *korean*, *thai* or *languages with ambiguous word boundaries* are not supported.


<br>


**UPDATE 01-04-2018** : *boost-locale* is no longer a system requirement for the textTinyR package.


<br>


### **Installation of the textTinyR package (CRAN, Github)**

<br>

To install the package from CRAN use, 

```R

install.packages('textTinyR')


```
<br>

and to download the latest version from Github use the *install_github* function of the devtools package,
<br><br>

```R

devtools::install_github(repo = 'mlampros/textTinyR')


```
<br>
Use the following link to report bugs/issues,
<br>

[https://github.com/mlampros/textTinyR/issues](https://github.com/mlampros/textTinyR/issues)

<br><br>


**UPDATE 06-02-2020**

<br>

**Docker images** of the *textTinyR* package are available to download from my [dockerhub](https://hub.docker.com/r/mlampros/texttinyr) account. The images come with *Rstudio* and the *R-development* version (latest) installed. The whole process was tested on Ubuntu 18.04. To **pull** & **run** the image do the following,

<br>

```R

docker pull mlampros/texttinyr:rstudiodev

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 mlampros/texttinyr:rstudiodev

```

<br>

The user can also **bind** a home directory / folder to the image to use its files by specifying the **-v** command,

<br>

```R

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 -v /home/YOUR_DIR:/home/rstudio/YOUR_DIR mlampros/texttinyr:rstudiodev


```

<br>

In the latter case you might have first give permission privileges for write access to **YOUR_DIR** directory (not necessarily) using,

<br>

```R

chmod -R 777 /home/YOUR_DIR


```

<br>

The **USER** defaults to *rstudio* but you have to give your **PASSWORD** of preference (see [www.rocker-project.org](https://www.rocker-project.org/) for more information).

<br>

Open your web-browser and depending where the docker image was *build / run* give, 

<br>

**1st. Option** on your personal computer,

<br>

```R
http://0.0.0.0:8787 

```

<br>

**2nd. Option** on a cloud instance, 

<br>

```R
http://Public DNS:8787

```

<br>

to access the Rstudio console in order to give your username and password.

<br>

