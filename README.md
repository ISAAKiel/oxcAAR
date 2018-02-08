[![Travis-CI Build Status](https://travis-ci.org/ISAAKiel/oxcAAR.svg?branch=master)](https://travis-ci.org/ISAAKiel/oxcAAR) [![Coverage Status](https://img.shields.io/codecov/c/github/ISAAKiel/oxcAAR/master.svg)](https://codecov.io/github/ISAAKiel/oxcAAR?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/oxcAAR)](https://CRAN.R-project.org/package=oxcAAR) [![license](https://img.shields.io/badge/license-GPL%203-B50B82.svg)](https://www.R-project.org/Licenses/GPL-2)

<!-- README.md is generated from README.Rmd. Please edit that file -->
oxcAAR
======

`oxcAAR` (previously named `roxcal`) is a collection of functions that can be used to execute [OxCal](https://c14.arch.ox.ac.uk) from within R.

Please note that there is a [`roxcal` package](https://github.com/gavinsimpson/roxcal) package on Github, created by [Gavin Simpson](https://github.com/gavinsimpson). If you just need to import OxCal result files, you may consider using his package.

Licence
-------

`oxcAAR` is released under the [GNU General Public Licence, version 2](https://www.R-project.org/Licenses/GPL-2). Comments and feedback are welcome, as are code contributions.

Installation
------------

`oxcAAR` is currently not on [CRAN](https://CRAN.R-project.org/), but you can use [devtools](https://CRAN.R-project.org/package=devtools) to install the development version. To do so:

    if(!require('devtools')) install.packages('devtools')
    library(devtools)
    install_github('ISAAKiel/oxcAAR')

Get started
-----------

To get started, just run:

    quickSetupOxcal()

This will download and unzip [Oxcal](https://c14.arch.ox.ac.uk/oxcal.html) into the current working directory. You can change the destination path of Oxcal by setting `path` parameter.

Afterwards you can start calibrating your dates. Check the [Vignette](vignettes/basic-usage.Rmd) to learn how to do this.
