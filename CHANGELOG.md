# Changelog

## [1.1.5] - 2025 April 12
* Removed any `std::cerr` from leidenalg C++ source
* Removed other symbols like putchar(), exit(), etc. in the igraph source
* Used R internal random number generator for the leiden algorithm C++ code

## [1.1.4] - 2024 October 17
* Update rigraph/src/vendor/arpack Fortran files
* Fix 'Obsolescent feature' warning associated with Fortan, CRAN warning, cf https://fortranwiki.org/fortran/show/Modernizing+Old+Fortran

## [1.1.3] - 2024 February 27
* Fixed Makevars file
* Fix "format string is not a string literal" error, '-Wformat-security' warning

## [1.1.2] - 2023 September 3
* README changes, documentation changes

## [1.1.1] - 2023 August 31
* Yichen Wang (U. Michigan) added functions `find_partition_with_rep()` and `find_partition_with_rep_rcpp()`

## [1.1.0] - 2023 July 1
* Fixed bugs due to release of igraph verison 1.5.0. Refer to https://github.com/kharchenkolab/leidenAlg/issues/13

## [1.0.5] - 2022 September 29
* Fixed `/src/leidenalg/include/MutableVertexPartition.h`, Fedora-related 'GLIBCXX_ASSERTIONS' error

## [1.0.4] - 2022 September 25
* Remove 'Matrix.utils' as dependency

## [1.0.3] - 2022 April 08
* Install rigraph directly in `/src`. Remove previous linking to `igraph` R package in Makevars.

## [1.0.2] - 2022 March 03
* Fixed Makevars to use `install_name_tool -change`.

## [1.0.1] - 2021 Dec 03
* Modified the Makevars to use SHLIB_EXT to account for both shared library extensions on Mac OS (either *.so or *dylib)

## [1.0.0] - 2021-11-19
* `leidenAlg` has gone through no major revisions in over a year. In order to avoid any confusion, this should be released with a major version.

## [0.1.1] - 2021-03-02
* Fixed issue with unweighted graph. In this case, we set all edge weights to 1. 

## [0.1.0] - 2020-11-11
* Version published on CRAN: https://cran.r-project.org/web/packages/leidenAlg/index.html
* Tagged version on github released on 2 Jan 2021: https://github.com/kharchenkolab/leidenAlg/releases/tag/0.1.0
