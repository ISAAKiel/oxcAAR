## Test environments
* local OS X install, R 3.4.3
* local arch linux install, R 3.4.3
* ubuntu 14.04.5 LTS (on travis-ci), R 3.4.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* The Title field should be in title case, current version then in title case:
  'R Interface to OxCal 14C Calibration'
  'R Interface to OxCal 14c Calibration'

  14C is referring to one isotope of carbon, which by convention is spelled with a capital C.

## Downstream dependencies
I have also run R CMD check on downstream dependencies via revdep. All packages passed.
