
<!-- README.md is generated from README.Rmd. Please edit that file -->

# roclang

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/roclang)](https://CRAN.R-project.org/package=roclang)
<!-- badges: end -->

The goal of roclang is to diffuse content to facilitate more efficient
programming. But unlike rlang, which works on diffusing R code, roclang
works on diffusing roxygen2 documentations. Sections, parameters or dot
parameters are extracted from function documentations and turned into
valid Rd character strings, which are ready to diffuse into the roxygen2
documentation of another function by inserting inline code.

## Installation

You can install the released version of roclang from
[bitbucket](https://CRAN.R-project.org) with:

``` r
remotes::install_bitbucket("mprobe_xiurui/roclang")
```

## Examples of extracting and diffusing content from other functions’ documentions

These are some basic examples which show you how to diffuse sections
(both general and self-defined) or parameters (including dot-parameters)
from the documentation of another function, e.g. `stats::lm`:

``` r
library(roclang)
library(magrittr)
# Inherit a standard section, and leave the first letter as is
extract_roc_text(stats::lm, type = "general", select = "description", capitalize = NA) %>%
  cat()
#> \code{lm} is used to fit linear models.
#>   It can be used to carry out regression,
#>   single stratum analysis of variance and
#>   analysis of covariance (although \code{\link[stats]{aov}} may provide a more
#>   convenient interface for these).

# Inherit a self-defined section, and capitalize the first letter
extract_roc_text(stats::lm, type = "section", select = "Using time series", capitalize = TRUE) %>%
  cat()
#> Considerable care is needed when using \code{lm} with time series.
#> 
#>   Unless \code{na.action = NULL}, the time series attributes are
#>   stripped from the variables before the regression is done.  (This is
#>   necessary as omitting \code{NA}s would invalidate the time series
#>   attributes, and if \code{NA}s are omitted in the middle of the series
#>   the result would no longer be a regular time series.)
#> 
#>   Even if the time series attributes are retained, they are not used to
#>   line up series, so that the time shift of a lagged or differenced
#>   regressor would be ignored.  It is good practice to prepare a
#>   \code{data} argument by \code{\link[stats]{ts.intersect}(\dots, dframe = TRUE)},
#>   then apply a suitable \code{na.action} to that data frame and call
#>   \code{lm} with \code{na.action = NULL} so that residuals and fitted
#>   values are time series.

# Inherit a parameter, and diffuse it into text
paste0(
  "Here is the `formula` argument of `stats::lm`, defined as: ",
  extract_roc_text(stats::lm, type = "param", select = "formula", capitalize = FALSE)
) %>%
  cat()
#> Here is the `formula` argument of `stats::lm`, defined as: an object of class \code{"\link[stats]{formula}"} (or one that
#>     can be coerced to that class): a symbolic description of the
#>     model to be fitted.  The details of model specification are given
#>     under \sQuote{Details}.

# Inherit a set of dot params, and diffuse it into text
paste0(
  "`lm_arg` is a named list of ",
  extract_roc_text(stats::lm, type = "dot_params", select = c("-formula", "-data"), capitalize = FALSE)
) %>%
  cat()
#> `lm_arg` is a named list of arguments passed on to \code{\link[stats:lm]{stats::lm}}
#>   \describe{
#>     \item{\code{subset}}{an optional vector specifying a subset of observations
#>     to be used in the fitting process.}
#>     \item{\code{weights}}{an optional vector of weights to be used in the fitting
#>     process.  Should be \code{NULL} or a numeric vector.
#>     If non-NULL, weighted least squares is used with weights
#>     \code{weights} (that is, minimizing \code{sum(w*e^2)}); otherwise
#>     ordinary least squares is used.  See also \sQuote{Details},}
#>     \item{\code{na.action}}{a function which indicates what should happen
#>     when the data contain \code{NA}s.  The default is set by
#>     the \code{na.action} setting of \code{\link{options}}, and is
#>     \code{\link[stats]{na.fail}} if that is unset.  The \sQuote{factory-fresh}
#>     default is \code{\link[stats]{na.omit}}.  Another possible value is
#>     \code{NULL}, no action.  Value \code{\link[stats]{na.exclude}} can be useful.}
#>     \item{\code{method}}{the method to be used; for fitting, currently only
#>     \code{method = "qr"} is supported; \code{method = "model.frame"} returns
#>     the model frame (the same as with \code{model = TRUE}, see below).}
#>     \item{\code{model}}{logicals.  If \code{TRUE} the corresponding
#>     components of the fit (the model frame, the model matrix, the
#>     response, the QR decomposition) are returned.
#>   }
#>     \item{\code{x}}{logicals.  If \code{TRUE} the corresponding
#>     components of the fit (the model frame, the model matrix, the
#>     response, the QR decomposition) are returned.
#>   }
#>     \item{\code{y}}{logicals.  If \code{TRUE} the corresponding
#>     components of the fit (the model frame, the model matrix, the
#>     response, the QR decomposition) are returned.
#>   }
#>     \item{\code{qr}}{logicals.  If \code{TRUE} the corresponding
#>     components of the fit (the model frame, the model matrix, the
#>     response, the QR decomposition) are returned.
#>   }
#>     \item{\code{singular.ok}}{logical. If \code{FALSE} (the default in S but
#>     not in \R) a singular fit is an error.}
#>     \item{\code{contrasts}}{an optional list. See the \code{contrasts.arg}
#>     of \code{\link[stats]{model.matrix.default}}.}
#>     \item{\code{offset}}{this can be used to specify an \emph{a priori} known
#>     component to be included in the linear predictor during fitting.
#>     This should be \code{NULL} or a numeric vector or matrix of extents
#>     matching those of the response.  One or more \code{\link[stats]{offset}} terms can be
#>     included in the formula instead or as well, and if more than one are
#>     specified their sum is used.  See \code{\link[stats]{model.offset}}.}
#>   }
```

## Examples of use in roxygen2 comments

In `roxygen2` comments, one can use inline code to diffuse the extracted
contents into his or her own documentations:

``` r
#' Cited Version of \code{stats::lm} for
#' `r roclang::extract_roc_text(stats::lm, type = "general", select = "title", capitalize = TRUE)`
#'
#' Cited from \code{\link[stats]{lm}}:
#' `r roclang::extract_roc_text(stats::lm, type = "general", select = "description", capitalize = FALSE)`
#'
#' @param formula Cited from \code{\link[stats]{lm}} with the same argument ame:
#' `r roclang::extract_roc_text(stats::lm, type = "param", select = "formula", capitalize = NA)`
#' @param df Cited from the argument `data` of \code{\link[stats]{lm}}:
#' `r roclang::extract_roc_text(stats::lm, type = "param", select = "data", capitalize = NA)`
#' @param lm_arg Named list of
#' `r roclang::extract_roc_text(stats::lm, type = "dot_params", select = c("-formula", "-data"), capitalize = FALSE)`
#'
#' @return `r roclang::extract_roc_text(stats::lm, type = "general", select = "return", capitalize = TRUE)`
#'
#' @note Copied by: Xiurui Zhu
#'
#' @examples
#' `r roclang::extract_roc_text(stats::lm, type = "general", select = "examples", capitalize = NA)`
lm_copy <- function(formula, df, lm_arg) {
  library(rlang)
  rlang::exec(stats::lm, formula = formula, data = df, !!!lm_arg)
}
```

    #> % Generated by roxygen2: do not edit by hand
    #> % Please edit documentation in ./<text>
    #> \name{lm_copy}
    #> \alias{lm_copy}
    #> \title{Cited Version of \code{stats::lm} for
    #> Fitting Linear Models}
    #> \usage{
    #> lm_copy(formula, df, lm_arg)
    #> }
    #> \arguments{
    #> \item{formula}{Cited from \code{\link[stats]{lm}} with the same argument name:
    #> an object of class code{"link[stats]{formula}"} (or one that
    #>     can be coerced to that class): a symbolic description of the
    #>     model to be fitted.  The details of model specification are given
    #>     under sQuote{Details}.}
    #> 
    #> \item{df}{Cited from the argument `data` of \code{\link[stats]{lm}}:
    #> an optional data frame, list or environment (or object
    #>     coercible by code{link{as.data.frame}} to a data frame) containing
    #>     the variables in the model.  If not found in code{data}, the
    #>     variables are taken from code{environment(formula)},
    #>     typically the environment from which code{lm} is called.}
    #> 
    #> \item{lm_arg}{Named list of
    #> arguments passed on to code{link[stats:lm]{stats::lm}}
    #>   describe{
    #>     item{code{subset}}{an optional vector specifying a subset of observations
    #>     to be used in the fitting process.}
    #>     item{code{weights}}{an optional vector of weights to be used in the fitting
    #>     process.  Should be code{NULL} or a numeric vector.
    #>     If non-NULL, weighted least squares is used with weights
    #>     code{weights} (that is, minimizing code{sum(w*e^2)}); otherwise
    #>     ordinary least squares is used.  See also sQuote{Details},}
    #>     item{code{na.action}}{a function which indicates what should happen
    #>     when the data contain code{NA}s.  The default is set by
    #>     the code{na.action} setting of code{link{options}}, and is
    #>     code{link[stats]{na.fail}} if that is unset.  The sQuote{factory-fresh}
    #>     default is code{link[stats]{na.omit}}.  Another possible value is
    #>     code{NULL}, no action.  Value code{link[stats]{na.exclude}} can be useful.}
    #>     item{code{method}}{the method to be used; for fitting, currently only
    #>     code{method = "qr"} is supported; code{method = "model.frame"} returns
    #>     the model frame (the same as with code{model = TRUE}, see below).}
    #>     item{code{model}}{logicals.  If code{TRUE} the corresponding
    #>     components of the fit (the model frame, the model matrix, the
    #>     response, the QR decomposition) are returned.
    #>   }
    #>     item{code{x}}{logicals.  If code{TRUE} the corresponding
    #>     components of the fit (the model frame, the model matrix, the
    #>     response, the QR decomposition) are returned.
    #>   }
    #>     item{code{y}}{logicals.  If code{TRUE} the corresponding
    #>     components of the fit (the model frame, the model matrix, the
    #>     response, the QR decomposition) are returned.
    #>   }
    #>     item{code{qr}}{logicals.  If code{TRUE} the corresponding
    #>     components of the fit (the model frame, the model matrix, the
    #>     response, the QR decomposition) are returned.
    #>   }
    #>     item{code{singular.ok}}{logical. If code{FALSE} (the default in S but
    #>     not in R) a singular fit is an error.}
    #>     item{code{contrasts}}{an optional list. See the code{contrasts.arg}
    #>     of code{link[stats]{model.matrix.default}}.}
    #>     item{code{offset}}{this can be used to specify an emph{a priori} known
    #>     component to be included in the linear predictor during fitting.
    #>     This should be code{NULL} or a numeric vector or matrix of extents
    #>     matching those of the response.  One or more code{link[stats]{offset}} terms can be
    #>     included in the formula instead or as well, and if more than one are
    #>     specified their sum is used.  See code{link[stats]{model.offset}}.}
    #>   }}
    #> }
    #> \value{
    #> NANA
    #> }
    #> \description{
    #> Cited from \code{\link[stats]{lm}}:
    #> code{lm} is used to fit linear models.
    #>   It can be used to carry out regression,
    #>   single stratum analysis of variance and
    #>   analysis of covariance (although code{link[stats]{aov}} may provide a more
    #>   convenient interface for these).
    #> }
    #> \note{
    #> Copied by: Xiurui Zhu
    #> }
    #> \examples{
    #> require(graphics)
    #> 
    #> ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
    #> ## Page 9: Plant Weight Data.
    #> ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
    #> trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
    #> group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
    #> weight <- c(ctl, trt)
    #> lm.D9 <- lm(weight ~ group)
    #> lm.D90 <- lm(weight ~ group - 1) # omitting intercept
    #> donttest{
    #> anova(lm.D9)
    #> summary(lm.D90)
    #> }
    #> opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
    #> plot(lm.D9, las = 1)      # Residuals, Fitted, ...
    #> par(opar)
    #> dontshow{
    #> ## model frame :
    #> stopifnot(identical(lm(weight ~ group, method = "model.frame"),
    #>                     model.frame(lm.D9)))
    #> }
    #> ### less simple examples in "See Also" above
    #> }
