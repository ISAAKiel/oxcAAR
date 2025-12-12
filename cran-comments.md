## Resubmission
This is a maintenance release.

**Changes in this version:**

- Updated the test suite to replace deprecated/defunct `testthat::with_mock()` usage with `testthat::with_mocked_bindings()`, restoring compatibility with recent versions of testthat.
- Updated ggplot-based plotting code to comply with current ggplot2 (≥ 4.0.0), removing deprecated functionality.
- Addressed R CMD check NOTES related to non-standard evaluation by explicitly declaring variables used in ggplot2 aesthetics.
- Minor robustness improvements in argument validation and vignette rebuilding.

No user-facing API changes were introduced.

**Additional fixes since the initial resubmission**

The initial resubmission produced a NOTE due to a permanently redirected URL:

- Updated the Codecov URL in README.md from https://codecov.io/github/ISAAKiel/oxcAAR to https://app.codecov.io/github/ISAAKiel/oxcAAR, resolving the CRAN incoming NOTE related to a 301 redirect.

The Date field in DESCRIPTION has now also been updated accordingly.

## Test environments
- GitHub Actions (macOS), R-release
- GitHub Actions (Ubuntu 22.04, R-release and R-devel)
- GitHub Actions (Windows), R-release
- Local: macOS (Apple Silicon), R 4.5.1
- R-hub: Ubuntu 24.04.3 LTS, R-devel (2025-12-10 r89137) and additional R-hub platforms (Windows R-devel, macOS-arm64 R-devel, linux R-devel variants incl. sanitizers)

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## R-hub results
All checks reported Status: OK on the tested platforms for oxcAAR 1.1.2.

## Maintainer change

The maintainer email address has been updated:

- New maintainer: Hinz Martin martin.hinz@ufg.uni-kiel.de￼
- Old maintainer: Hinz Martin martin.hinz@iaw.unibe.ch￼

## Closing remark

This resubmission fully resolves the previously reported CRAN issues.
Thank you for your time and for maintaining CRAN.
