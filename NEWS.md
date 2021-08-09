# roclang 0.1.2

## Enhancements:

* Enforce R CMD check and code coverage workflows and add corresponding badges.

* Add more tests to cover all code.

## Bug fixes:

* Fix a bug that fails to cite a parameter if the function is from a loaded/attached package but not explicitly designated, e.g. `paste` instead of `base::paste`.


# roclang 0.1.1

## Enhancements:

* `extract_roc_text()` now defaults its `capitalize` argument to `NA`, leaving the function to keep the first letter of the return as is. Previously the default was `TRUE`, which would capitalize the first letter anyways if missing.

* Add package documentation.

## Bug fixes:

* Fix a few typos in README.Rmd and function documentations.

* Add single quotes to non-standard words.

* Update release status on CRAN and installation methods.

* Handle parsing of `fun` in `extract_roc_text()` in a safer way using `get()` with `mode = "function"`; previously it used `rlang::parse_expr()` with `eval()`, which is risky for possible code injection.


# roclang 0.1.0

* Initial release of the package.
