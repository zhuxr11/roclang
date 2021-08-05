#' roclang: A package for diffusing function documentations into 'roxygen' comments
#'
#' The 'roclang' package facilitates efficient diffusing of content across function documentations.
#' Sections, parameters or dot parameters are extracted from function documentations and turned into valid Rd character strings,
#' which are ready to diffuse into the 'roxygen' comments of another function by inserting inline code.
#'
#' @section Functions:
#' * Text extraction and manipulation function: \code{\link{extract_roc_text}}.
#' * Rd evaluation and compilation function: \code{\link{roc_eval_text}}.
#'
#' @docType package
#' @name roclang
#' @aliases roclang-package
NULL
