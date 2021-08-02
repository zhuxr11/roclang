
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Diffusing roxygen documentation content with roclang

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/roclang)](https://CRAN.R-project.org/package=roclang)
<!-- badges: end -->

**Package**: roclang<br /> **Authors**: Xiurui Zhu<br /> **Modified**:
2021-08-03 01:17:04<br /> **Compiled**: 2021-08-03 01:17:12

The goal of `roclang` is to diffuse documentation content to facilitate
more efficient programming. As a partner of
[`rlang`](https://github.com/r-lib/rlang/), which works on diffusing R
code, `roclang` works on diffusing
[roxygen](https://github.com/r-lib/roxygen2/) documentations. Sections,
parameters or dot parameters are extracted from function documentations
and turned into valid Rd character strings, which are ready to diffuse
into the roxygen documentation of another function by inserting inline
code.

## Installation

You can install the released version of roclang from
[github](https://github.com/zhuxr11/roclang) with:

``` r
remotes::install_github("zhuxr11/roclang")
```

## Examples of diffusing content from other functions’ documentions

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

## Use cases in roxygen comments

In roxygen comments, one can use inline code to diffuse the extracted
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

To view the compiled Rd file, use `roc_eval_text()` function as an
improved version of `roxygen::roc_proc_text()` function, in that the
former further evaluates inline code and code blocks before parsing the
text into Rd.

``` r
library(roxygen2)
library(magrittr)
fun_text <- '
#\' Cited Version of \\code{stats::lm} for
#\' `r roclang::extract_roc_text(stats::lm, type = "general", select = "title", capitalize = TRUE)`
#\'
#\' Cited from \\code{\\link[stats]{lm}}:
#\' `r roclang::extract_roc_text(stats::lm, type = "general", select = "description", capitalize = TRUE)`
#\'
#\' @param formula Cited from \\code{\\link[stats]{lm}} with the same argument name:
#\' `r roclang::extract_roc_text(stats::lm, type = "param", select = "formula", capitalize = NA)`
#\' @param df Cited from the argument `data` of \\code{\\link[stats]{lm}}:
#\' `r roclang::extract_roc_text(stats::lm, type = "param", select = "data", capitalize = NA)`
#\' @param lm_arg Named list of
#\' `r roclang::extract_roc_text(stats::lm, type = "dot_params", select = c("-formula", "-data"), capitalize = FALSE)`
#\'
#\' @return `r roclang::extract_roc_text(stats::lm, type = "general", select = "return", capitalize = TRUE)`
#\'
#\' @note Copied by: Xiurui Zhu
#\'
#\' @examples
#\' `r roclang::extract_roc_text(stats::lm, type = "general", select = "examples", capitalize = NA)`
lm_copy <- function(formula, df, lm_arg) {
  library(rlang)
  rlang::exec(stats::lm, formula = formula, data = df, !!!lm_arg)
}
'
roc_eval_text(roxygen2::rd_roclet(), fun_text)[[1L]]
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
#> an object of class \code{"\link[stats]{formula}"} (or one that
#>     can be coerced to that class): a symbolic description of the
#>     model to be fitted.  The details of model specification are given
#>     under \sQuote{Details}.}
#> 
#> \item{df}{Cited from the argument `data` of \code{\link[stats]{lm}}:
#> an optional data frame, list or environment (or object
#>     coercible by \code{\link{as.data.frame}} to a data frame) containing
#>     the variables in the model.  If not found in \code{data}, the
#>     variables are taken from \code{environment(formula)},
#>     typically the environment from which \code{lm} is called.}
#> 
#> \item{lm_arg}{Named list of
#> arguments passed on to \code{\link[stats:lm]{stats::lm}}
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
#>   }}
#> }
#> \value{
#> \code{lm} returns an object of \code{\link{class}} \code{"lm"} or for
#>   multiple responses of class \code{c("mlm", "lm")}.
#> 
#>   The functions \code{summary} and \code{\link[stats]{anova}} are used to
#>   obtain and print a summary and analysis of variance table of the
#>   results.  The generic accessor functions \code{coefficients},
#>   \code{effects}, \code{fitted.values} and \code{residuals} extract
#>   various useful features of the value returned by \code{lm}.
#> 
#>   An object of class \code{"lm"} is a list containing at least the
#>   following components:
#> 
#>   \item{coefficients}{a named vector of coefficients}
#>   \item{residuals}{the residuals, that is response minus fitted values.}
#>   \item{fitted.values}{the fitted mean values.}
#>   \item{rank}{the numeric rank of the fitted linear model.}
#>   \item{weights}{(only for weighted fits) the specified weights.}
#>   \item{df.residual}{the residual degrees of freedom.}
#>   \item{call}{the matched call.}
#>   \item{terms}{the \code{\link[stats]{terms}} object used.}
#>   \item{contrasts}{(only where relevant) the contrasts used.}
#>   \item{xlevels}{(only where relevant) a record of the levels of the
#>     factors used in fitting.}
#>   \item{offset}{the offset used (missing if none were used).}
#>   \item{y}{if requested, the response used.}
#>   \item{x}{if requested, the model matrix used.}
#>   \item{model}{if requested (the default), the model frame used.}
#>   \item{na.action}{(where relevant) information returned by
#>     \code{\link[stats]{model.frame}} on the special handling of \code{NA}s.}
#> 
#>   In addition, non-null fits will have components \code{assign},
#>   \code{effects} and (unless not requested) \code{qr} relating to the linear
#>   fit, for use by extractor functions such as \code{summary} and
#>   \code{\link[stats]{effects}}.
#> }
#> \description{
#> Cited from \code{\link[stats]{lm}}:
#> \code{lm} is used to fit linear models.
#>   It can be used to carry out regression,
#>   single stratum analysis of variance and
#>   analysis of covariance (although \code{\link[stats]{aov}} may provide a more
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
#> \donttest{
#> anova(lm.D9)
#> summary(lm.D90)
#> }
#> opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
#> plot(lm.D9, las = 1)      # Residuals, Fitted, ...
#> par(opar)
#> \dontshow{
#> ## model frame :
#> stopifnot(identical(lm(weight ~ group, method = "model.frame"),
#>                     model.frame(lm.D9)))
#> }
#> ### less simple examples in "See Also" above
#> }
```

## Further possibilities

Since `roclang` returns Rd text, which is by nature character, more *ad
hoc* manipulations can be performed in the inline code using functions
such as those from `stringr` package. This makes `roclang` even more
flexible in diffusing roxygen documentation content.

## Session info

This file is compiled with the following packages and versions:

``` r
sessionInfo()
#> R version 4.0.5 (2021-03-31)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 19042)
#> 
#> Matrix products: default
#> 
#> locale:
#> [1] LC_COLLATE=Chinese (Simplified)_China.936 
#> [2] LC_CTYPE=Chinese (Simplified)_China.936   
#> [3] LC_MONETARY=Chinese (Simplified)_China.936
#> [4] LC_NUMERIC=C                              
#> [5] LC_TIME=Chinese (Simplified)_China.936    
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] roxygen2_7.1.1 magrittr_2.0.1 roclang_0.1.0 
#> 
#> loaded via a namespace (and not attached):
#>  [1] Rcpp_1.0.5       rex_1.2.0        xml2_1.3.2       knitr_1.29      
#>  [5] tidyselect_1.1.0 R6_2.4.1         rlang_0.4.10     fansi_0.4.2     
#>  [9] blob_1.2.1       stringr_1.4.0    dplyr_1.0.7      tools_4.0.5     
#> [13] xfun_0.15        utf8_1.1.4       DBI_1.1.0        htmltools_0.5.0 
#> [17] ellipsis_0.3.2   assertthat_0.2.1 yaml_2.2.1       digest_0.6.25   
#> [21] tibble_3.1.3     lifecycle_1.0.0  crayon_1.3.4     tidyr_1.1.3     
#> [25] purrr_0.3.4      vctrs_0.3.8      glue_1.4.1       evaluate_0.14   
#> [29] rmarkdown_2.3    stringi_1.4.6    compiler_4.0.5   pillar_1.6.2    
#> [33] generics_0.0.2   pkgconfig_2.0.3
```
