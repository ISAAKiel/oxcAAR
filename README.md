<!-- README.md is generated from README.Rmd. Please edit that file -->
roxcal
======

`roxcal` is a collection of functions that can be used to execute [OxCal](https://c14.arch.ox.ac.uk) from within R.

Please note that there is a [second `roxcal` package](https://github.com/gavinsimpson/roxcal) package on Github, created by [Gavin Simpson](https://github.com/gavinsimpson). If you just need to import OxCal result files, you may consider using his package.

Licence
-------

`roxcal` is released under the [GNU General Public Licence, version 2](http://www.r-project.org/Licenses/GPL-2). Comments and feedback are welcome, as are code contributions.

Installation
------------

`roxcal` is currently not on [CRAN](http://cran.r-project.org/), but you can use [devtools](http://cran.r-project.org/web/packages/devtools/index.html) to install the development version. To do so:

    if(!require('devtools')) install.packages('devtools')
    library(devtools)
    install_github('MartinHinz/roxcal')
