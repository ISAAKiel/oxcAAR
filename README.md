[![Travis-CI Build
Status](https://travis-ci.org/ISAAKiel/oxcAAR.svg?branch=master)](https://travis-ci.org/ISAAKiel/oxcAAR)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ISAAKiel/oxcAAR/master.svg)](https://codecov.io/github/ISAAKiel/oxcAAR?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/oxcAAR)](http://cran.r-project.org/package=oxcAAR)
[![license](https://img.shields.io/badge/license-GPL%203-B50B82.svg)](https://www.r-project.org/Licenses/GPL-2)
[![rstudio mirror downloads
monthly](http://cranlogs.r-pkg.org/badges/oxcAAR)](http://cran.rstudio.com/web/packages/oxcAAR/index.html)
[![rstudio mirror downloads
total](http://cranlogs.r-pkg.org/badges/grand-total/oxcAAR)](http://cran.rstudio.com/web/packages/oxcAAR/index.html) [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- README.md is generated from README.Rmd. Please edit that file -->
oxcAAR
======

`oxcAAR` (previously named `roxcal`) is a collection of functions that
can be used to execute [OxCal](https://c14.arch.ox.ac.uk) from within R.

Please note that there is a [`roxcal`
package](https://github.com/gavinsimpson/roxcal) package on Github,
created by [Gavin Simpson](https://github.com/gavinsimpson). If you just
need to import OxCal result files, you may consider using his package.

Licence
-------

`oxcAAR` is released under the [GNU General Public Licence, version
2](http://www.r-project.org/Licenses/GPL-2). Comments and feedback are
welcome, as are code contributions.

Installation
------------

`oxcAAR` is available on [CRAN](https://cran.r-project.org/web/packages/oxcAAR/index.html) and can be installed through `install.packages("oxcAAR")`. You can use [devtools](https://CRAN.R-project.org/package=devtools) to install the development version. To do so:

    if(!require('devtools')) install.packages('devtools')
    library(devtools)
    install_github('ISAAKiel/oxcAAR')

Get started
-----------

To get started, just run:

    quickSetupOxcal()

This will download and unzip
[Oxcal](https://c14.arch.ox.ac.uk/oxcal.html) into tempdir(). You can
change the destination path of Oxcal by setting the `path` parameter.

Afterwards you can start calibrating your dates. Check the
[Vignette](vignettes/basic-usage.Rmd) to learn how to do this.
