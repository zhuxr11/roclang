# Functions for Rd evaluation and compilation

#' @import dplyr tidyr purrr tibble stringr
#' @importFrom magrittr %>% %T>%
NULL

#' Generate Rd from text with evaluated inline code and code blocks
#'
#' \code{roc_eval_text} is an upgraded version of \code{\link[roxygen2]{roc_proc_text}}
#' that evaluates inline and block code before generating Rd.
#'
#' @inheritParams roxygen2::roc_proc_text
#'
#' @return List with names as \code{fun_name.Rd}, where each element is the \code{\link[roxygen2]{RoxyTopic}} for
#' the corresponding function, same as the return of \code{\link[roxygen2]{roc_proc_text}}.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @example man-roxygen/ex-roc_eval_text.R
roc_eval_text <- function(roclet, input) {
  fun_text <- input
  fun_text <- fun_text %>%
    # Evaluate inline code and replace
    .replace_eval_block("`r (.*?)`") %>%
    # Evaluate code blocks and replace
    .replace_eval_block(
      stringr::regex("`{3,}\\{r.*\\}(.*?)`{3,}",
                     multiline = TRUE,
                     dotall = TRUE)
    )
  roxygen2::roc_proc_text(roxygen2::rd_roclet(), fun_text)
}

#' Add 'roxygen' comment header to a multi-line text
#'
#' @importFrom rex escape
#'
#' @inheritParams roc_eval_text
#'
#' @noRd
.add_roxy_header <- function(input) {
  # Get 'roxygen' comment header
  roxy_comment <- .get_roxy_header()
  input %>%
    stringr::str_replace_all(paste0("\n(?<!", rex::escape(roxy_comment), ")"),
                             paste0("\n", rex::escape(roxy_comment)))
}

#' Remove 'roxygen' comment header from a multi-line text
#'
#' @importFrom rex escape
#'
#' @inheritParams roc_eval_text
#'
#' @noRd
.remove_roxy_header <- function(input) {
  # Get 'roxygen' comment header
  roxy_comment <- .get_roxy_header()
  input %>%
    stringr::str_replace_all(paste0("\n", rex::escape(roxy_comment)), "\n") %>%
    stringr::str_replace(paste0("^", rex::escape(roxy_comment)), "") %>%
    stringr::str_replace(paste0(rex::escape(roxy_comment), "$"), "")
}

#' Get 'roxygen' comment header
#'
#' @noRd
.get_roxy_header <- function() {
  roxy_comment <- options("roxygen.comment") %>%
    purrr::pluck(1L)
  if (is.null(roxy_comment) == TRUE) {
    roxy_comment <- "#' "
  }
  roxy_comment
}

#' Evaluate code block and diffuse results into text
#'
#' @importFrom rlang parse_expr
#'
#' @inheritParams roc_eval_text
#' @param pattern Regular expression (of length 1L) that mark the pattern of code block.
#' The code section should be only 1 and marked as "(.*?)".
#' For example, inline \code{[code]} is of pattern: `r '\u0060r [code]\u0060'`,
#' and then we can use \code{pattern = "`r '\u0060r (.*?)\u0060'`"}.
#' And \code{[code]} block is of pattern: `r '\u0060\u0060\u0060{r}[code]\u0060\u0060\u0060'`,
#' and then we can use \code{pattern = "`r '\u0060{3,}\\\\{r.*\\\\}(.*?)\u0060{3,}'`"},
#' considering the variant number of backticks (at least 3) and variant content in code block options.
#'
#' @noRd
.replace_eval_block <- function(input, pattern) {
  input %>%
    stringr::str_replace_all(
      pattern,
      function(extract_text) {
        extract_text %>%
          stringr::str_replace(pattern, "\\1") %>%
          # Remove 'roxygen' comment headers from new lines
          .remove_roxy_header() %>%
          rlang::parse_expr() %>%
          eval() %>%
          # Add 'roxygen' comment headers to new lines
          .add_roxy_header()
      }
    )
}
