## Test environments
* local OS X install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1
* ubuntu 16.04 (on my own machine), R 3.6.1
* win-builder (win-builder.r-project.org)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.


02.10.2019

changed https://cran.rstudio.com/web/packages/geometr/index.html to https://CRAN.R-project.org/package=geometr at line 23 in README.Rmd
fiyed some more typos

07.10.2019

adapt package description
using \donttest{} instead of \dontrun{}, testing the functions manually (and fixing an issue in gc_grob() I saw due to this) 

11.10.2019

include Dan Sunday as copyright holder for the point-in-polygon algorithm in DESCRIPTION and geometr.Rd
