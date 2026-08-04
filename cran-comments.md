## Test environments

* local: Ubuntu 24.04, R 4.5.1

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

## Notes

This is a resubmission after addressing the incoming check issues:

* removed the generated `README_files` artifact from the source package by tightening `.Rbuildignore`
* added `inst/WORDLIST` for the proper nouns `Ghosh` and `Strehl` flagged in `DESCRIPTION`
* made the complete vignette HTML-only so the source tarball no longer ships a vignette PDF

The local `R CMD check --no-manual --as-cran` run completed with no errors or warnings; the remaining NOTE is the standard new-submission/current-time note.
