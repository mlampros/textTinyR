
#---------------------------------------------------------------------------------------------------------------------------------------
# the "configure.ac" file is a modification of the following file : https://github.com/eddelbuettel/rcppmlpack2/blob/master/configure.ac
#---------------------------------------------------------------------------------------------------------------------------------------



#-------------------------------
# require at least autoconf 2.61
#-------------------------------

AC_PREREQ([2.61])

AC_INIT([textTinyR], [1.0.0], [mouselimislampros@gmail.com])


#-----------------
# We are using C++
#-----------------

AC_LANG(C++)
AC_REQUIRE_CPP
AC_PROG_CC
AC_PROG_CXX


#----------------
# compiling in R
#----------------

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`


#-----------------------------
# Add-on macro for C++11 check
#-----------------------------

m4_include([tools/m4/ax_cxx_compile_stdcxx.m4])
AX_CXX_COMPILE_STDCXX([11], [noext], [])


#----------------------
## check for pkg-config
#----------------------

AC_DEFUN([AC_PROG_PKGCONFIG], [AC_CHECK_PROG(PKGCONFIG,pkg-config,yes)])
AC_PROG_PKGCONFIG


#---------------------
# manual configuration
#---------------------

if test x"${PKGCONFIG}" == x"yes" && pkg-config --atleast-version=1.0.0 lboost_locale; then
	LOCALE_LIBS=`pkg-config --libs lboost_locale`

else
	#-------------------------------------------------------------
	# Add-on macro for library check in case pkg-config test fails
	#-------------------------------------------------------------

	m4_include([tools/m4/ax_check_library.m4])

	AC_LANG_PUSH([C++])

	#------------------------------
	# check if boost locale present
	#------------------------------

	AX_CHECK_LIBRARY([LOCALE], [boost/locale.hpp], [boost_locale], [LOCALE_LIBS="-lboost_locale"], 
			 [AC_MSG_ERROR([Unable to find the boost-locale. In DEBIAN/UBUNTU: sudo apt-get install libboost-all-dev; sudo apt-get install libboost-locale-dev, FEDORA : yum install boost-devel,
					OSX/brew : brew install boost])])
	AC_LANG_POP
fi


AC_SUBST(LOCALE_LIBS)

AC_CONFIG_FILES([src/Makevars])

AC_OUTPUT


