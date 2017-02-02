[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/textTinyR)](http://cran.r-project.org/package=textTinyR)
[![Travis-CI Build Status](https://travis-ci.org/mlampros/textTinyR.svg?branch=master)](https://travis-ci.org/mlampros/textTinyR)
[![codecov.io](https://codecov.io/github/mlampros/textTinyR/coverage.svg?branch=master)](https://codecov.io/github/mlampros/textTinyR?branch=master)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mlampros/textTinyR?branch=master&svg=true)](https://ci.appveyor.com/project/mlampros/textTinyR/branch/master)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/textTinyR?color=blue)](http://www.r-pkg.org/pkg/textTinyR)


## textTinyR
<br>

The textTinyR package consists of text pre-processing functions for small or big data files. More details on the functionality of the textTinyR can be found in the [blog-post](http://mlampros.github.io/2017/01/05/textTinyR_package/) and in the package Vignette. The R package can be installed, in the following OS's: Linux, Mac and Windows. However, there are some limitations :

* there is no support for chinese, japanese, korean, thai or languages with ambiguous word boundaries.
* there is no support functions for utf-locale on windows, meaning only english character strings or files can be input and pre-processed.

<br>


### **System Requirements ( for unix OS's )**

<br>

#### **Debian/Ubuntu**


sudo apt-get install libboost-all-dev

sudo apt-get update

sudo apt-get install libboost-locale-dev

<br>

#### **Fedora**


yum install boost-devel

<br>

#### **Macintosh OSX/brew**


The boost library will be installed on the Macintosh OSx using the *Homebrew package manager*, 

If the boost library is already installed using **brew install boost** then it must be removed using the following command, 

<br>

**brew uninstall boost**

<br>


Then the **formula** for the boost library should be modified using a text editor (TextEdit, TextMate, etc). The formula on a Macintosh OS Sierra is saved in:

<br>

**/usr/local/Homebrew/Library/Taps/homebrew/homebrew-core/Formula/boost.rb**

<br>

The user should open the **boost.rb** formula and replace the following code chunk beginning from (approx.) line 71,

<br>

```R

# layout should be synchronized with boost-python
args = ["--prefix=#{prefix}",
        "--libdir=#{lib}",
        "-d2",
        "-j#{ENV.make_jobs}",
        "--layout=tagged",
        "--user-config=user-config.jam",
        "install"]

if build.with? "single"
  args << "threading=multi,single"
else
  args << "threading=multi"
end

```

<br>

with the following code chunk, 

<br>

```R

# layout should be synchronized with boost-python
args = ["--prefix=#{prefix}",
        "--libdir=#{lib}",
        "-d2",
        "-j#{ENV.make_jobs}",
        "--layout=system", 
        "--user-config=user-config.jam",
        "threading=multi",
        "install"]

#if build.with? "single"
#  args << "threading=multi,single"
#else
#  args << "threading=multi"
#end

```

<br>

Then the user should save the changes, close the file and run,

<br>

**brew update**

<br>

to apply the changes.

<br>

Then he/she should open a new terminal (console) and type the following command, which installs the boost library using the modified formula from source, (**warning**: there are two dashes before : build-from-source)

<br>

**brew install /usr/local/Homebrew/Library/Taps/homebrew/homebrew-core/Formula/boost.rb --build-from-source**

<br>

That's it.

<br>

### **Installation of the textTinyR package (CRAN, Github)**

<br>

To install the package from CRAN use, 

```R

install.packages('textTinyR', clean = TRUE)


```
<br>

and to download the latest version from Github use the *install_github* function of the devtools package,
<br><br>

```R

devtools::install_github(repo = 'mlampros/textTinyR', clean = TRUE)


```
<br>
Use the following link to report bugs/issues,
<br><br>

[https://github.com/mlampros/textTinyR/issues](https://github.com/mlampros/textTinyR/issues)
