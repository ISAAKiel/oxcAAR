## Test environments
* GitHub Action (macOS), R 4.0.3
* GitHub Action (ubuntu 20.04, release and devel), R 4.0.3
* GitHub Action (windows), R 4.0.3

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Rhub check failed
### Windows Build
oxcAAR 1.1.1: ERROR

Build ID:	oxcAAR_1.1.1.tar.gz-5e9bc81babc548cd87e38a689e2bef32
Platform:	Windows Server 2008 R2 SP1, R-devel, 32/64 bit

Error: package or namespace load failed for 'oxcAAR' in library.dynam(lib, package, package.lib):
 DLL 'jsonlite' not found: maybe not installed for this architecture?
Error: loading failed
likely an local error at Rhub?

### Linux Build
oxcAAR 1.1.1: OK

Build ID:	oxcAAR_1.1.1.tar.gz-8a10376201e540e2ad55d0d615b8f8ae
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC

oxcAAR 1.1.1: OK

Build ID:	oxcAAR_1.1.1.tar.gz-1b66cade49114ef19f2ef477f888ad77
Platform:	Fedora Linux, R-devel, clang, gfortran

## winbuilder note:
Status: OK
R version 4.1.0 (2021-05-18)

Status: OK
R Under development (unstable) (2021-07-03 r80596)

Maintainer: 'Hinz Martin <martin.hinz@iaw.unibe.ch>'

## Downstream dependencies
I have also run R CMD check on downstream dependencies via revdep. All packages passed.
