# roclang 0.2.2

## Enhancements:

* Change package loading in `README.Rmd` to `pkgload` so that the developmental version can be correctly loaded.

* Add badge of total downloads to `README.Rmd`.


# roclang 0.2.1

## Enhancements:

* Improve parameter matching of `type = "dot_params"`. Now the negative selection of non-matched parameters also results in meaningful error messages.

* Remove the restriction on selecting `...` when `type = "param"`. For example, `...` is documented as a meaningful parameter in `order`. Try `extract_roc_text(order, type = "param", select = "...")`. However, `...` cannot be inherited with `@inheritDotParams` tag, which is intrinsically not supported by `roxygen2` to date.


# roclang 0.2.0

## Enhancements:

* Enable selection of multiple parameters when using `type = "param"`, in accordance with the update from `roxygen2 > 7.1.2` that co-documented parameters should be selected in a whole set when extracting their shared documentation. However, selecting multiple independently documented parameters using `type = "param"` is still not allowed, since the documentation to diffuse should keep to a single entry per call. To diffuse documentation from multiple independently documented parameters, please call the function on each of them.

* Add check on whether the extracted documentation entry is invalid (`NA_character_`) or multiple. These two cases will now result in error, since in most cases such results are not expected. The documentation entry can be multiple if selecting multiple independently documented parameters using `type = "param"` (not allowed).

* Keep backward compatibility with `roxygen2 <= 7.1.2` while accommodating to the changes (tested under `roxygen2 = 7.1.1`).


# roclang 0.1.4

## Enhancements:

* Add check on whether a function without specifying package (e.g. `filter`) appears in multiple packages (e.g. `dplyr` and `stats`). This will now result in an error.

## Bug fixes:

* Change the test of a function from base package from `base::paste` function to `base::library`, since the former will result in `<NA>` when extracting the documentation for `sep` on MKL platform, as reported by CRAN.


# roclang 0.1.3

## Bug fixes:

* Update string matches in `extract_roc_text.R` because of an update in `stats::lm()` documentation: from `\\\\code\{lm\} is used to fit linear models\\.` to `\\\\code\{lm\} is used to fit linear models, including multivariate ones\\.`.

* Fix some typos in comments.


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
