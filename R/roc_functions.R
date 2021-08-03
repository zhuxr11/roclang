#' @import dplyr tidyr purrr tibble stringr
#' @importFrom magrittr %>% %T>%
NULL

#' Extract a section, parameter or set of dot-parameters from a function documentation
#'
#' \code{extract_roc_text} cites sections or parameters from a function documentation
#' in the syntax of \code{@inherit}, \code{@inheritSection}, \code{@inheritParams} or \code{@inheritDotParams} tag
#' from \code{\link[roxygen2]{roxygen2}} package. See details about how to use this function.
#'
#' @importFrom methods formalArgs
#' @importFrom rlang parse_expr
#' @importFrom roxygen2 roc_proc_text rd_roclet
#'
#' @param fun Function or character indicating function name.
#' @param type Type of extraction. If content is extracted by \code{@inherit} tags,
#' set to \code{"general"}; if by \code{@inheritSection}, set to \code{section};
#' if by \code{@inheritParams}, set to \code{param}; if by \code{@inheritDotParams},
#' set to \code{"dot_params"}.
#' @param select Selection of extraction.
#' For \code{type = "general"}, character indicating the section to extract.
#' For \code{type = "section"}, character indicating the section title to extract.
#' For \code{type = "param"}, character indicating the name of parameter to extract.
#' For \code{type = "dot_params"}, character or character vector to add or remove (with "-") parameters as \code{@inheritDotParams};
#' if character vector provided, the elements are concatenated with spaces just as \code{@inheritDotParams} syntax.
#' to inherit, e.g. \code{"x,y"} to inherit 2 parameters or \code{"-z"} to remove a parameter.
#' @param capitalize Logical indicating whether the first letter of the return should be capitalized.
#' If \code{capitalize = NA}, the return is left as is.
#'
#' @details To diffuse the function output into \code{\link[roxygen2]{roxygen2}} comments,
#' one may write the function documentation like this:
#' \preformatted{
#' #' Function documentation for the diffusion of parameter descriptions
#' #'
#' #' @param lm_list Named list of
#' #' `r '\u0060r extract_roc_text(stats::lm, type = "dot_params", select = c("-formula", "-data"), capitalize = FALSE)\u0060'`
#' #'
#' my_fun <- function(lm_list) {}
#' }
#'
#' @return Character as valid Rd text to diffuse into \code{\link[roxygen2]{roxygen2}} comments.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @example man-roxygen/ex-extract_roc_text.R
extract_roc_text <- function(
  fun,
  type = c("general", "section", "param", "dot_params"),
  select = NULL,
  capitalize = TRUE
) {
  if (is.function(fun) == TRUE) {
    # Turn function into its name (character)
    fun <- deparse(substitute(fun))
  } else {
    stopifnot(is.character(fun) == TRUE)
  }
  type <- match.arg(type)
  # Parse select according to type
  select <- switch(
    type,
    # "general" and "param" only allow 1 selection (no spaces; space = multiple selection)
    general = ,
    param =  {
      if (length(select) != 1L) {
        stop("select should be length 1 for type = ", type)
      } else if (stringr::str_detect(select, " ") == TRUE) {
        stop("select should contain no spaces since only 1 is allowed for type = ", type)
      } else if (identical(stringr::str_trim(select), "") == TRUE) {
        stop("select should not be blank for type = ", type)
      } else {
        select
      }
    },
    # "section" only allow 1 selection (possibly with spaces)
    section = {
      if (length(select) != 1L) {
        stop("select should be length 1 for type = ", type)
      } else {
        select
      }
    },
    # "dot_params" allow multiple selections (concatenate with space)
    dot_params = paste(select, collapse = " "),
    stop("type not supported as: ", type)
  )
  # Check whether the selected parameter is one of the formalArgs of fun
  fun_function <- eval(rlang::parse_expr(fun))
  if (type %in% "param" == TRUE) {
    if (identical(select, "...") == TRUE) {
      stop("cannot select '...' for type = 'param'; ",
           "use type = 'dot_params' instead")
    } else if (select %in% methods::formalArgs(fun_function) == FALSE) {
      stop("select = ", select, " does not match any of methods::formalArgs(", fun, ")")
    }
  } else if (type %in% "dot_params" == TRUE) {
    if (identical(select, "...") == TRUE) {
      stop("cannot select '...' for type = 'dot_params'")
    } else {
      # Get positive selection(s)
      select_pos <- select %>%
        paste(collapse = " ") %>%
        stringr::str_split(" ") %>%
        purrr::pluck(1L) %>%
        stringr::str_subset("^-", negate = TRUE)
      select_pos_lgl <- select_pos %in% methods::formalArgs(fun_function)
      if (any(select_pos_lgl == FALSE)) {
        stop("select = c(\"", paste(select_pos[select_pos_lgl == FALSE], collapse = "\", \""),
             "\") does not match any of methods::formalArgs(", fun, ")")
      }
    }
  }

  # Format a Roxygen2 function blocks
  roxygen_fun_text <- .assemble_text_fun(fun = fun, type = type, select = select)

  # Compile "Rd" object
  roxygen_fun_rd <- roxygen2::roc_proc_text(roxygen2::rd_roclet(), roxygen_fun_text) %>%
    purrr::pluck("my_fun.Rd")
  # Extract the section/param text
  roxygen_extract_text <- roxygen_fun_rd$get_section(
    switch(
      type,
      general = {
        if (select %in% "return" == TRUE) {
          "value"
        } else {
          select
        }
      },
      section = type,
      param = "param",
      dot_params = "param",
      stop("type not supported as: ", type)
    )
  ) %>%
    as.character() %>%
    dplyr::last()
  roxygen_extract_text <- switch(
    type,
    general = ,
    param = ,
    dot_params = roxygen_extract_text %>%
      stringr::str_trim(),
    section = {
      roxygen_extract_text %>%
        str2lang() %>%
        eval() %>%
        purrr::pluck("content") %>%
        stringr::str_trim()
    },
    stop("type not supported as: ", type)
  )

  # Handle capitalization
  if (is.na(capitalize) == TRUE) {
    roxygen_extract_text_capital <- roxygen_extract_text
  } else {
    if (capitalize == TRUE) {
      roxygen_extract_text_capital <- paste0(
        roxygen_extract_text %>%
          stringr::str_sub(start = 1L, end = 1L) %>%
          toupper(),
        roxygen_extract_text %>%
          stringr::str_sub(start = 2L, end = -1L)
      )
    } else {
      roxygen_extract_text_capital <- paste0(
        roxygen_extract_text %>%
          stringr::str_sub(start = 1L, end = 1L) %>%
          tolower(),
        roxygen_extract_text %>%
          stringr::str_sub(start = 2L, end = -1L)
      )
    }
  }

  roxygen_extract_text_capital
}

#' Assemble text version of a function to process citation
#'
#' @inheritParams extract_roc_text
#' @param new_fun Character as the assembled function name
#'
#' @noRd
.assemble_text_fun <- function(fun, type, select, new_fun = "my_fun") {
  roxygen_text <- switch(
    type,
    general = {
      if (type %in% "general" == TRUE &&
          select %in% "title" == TRUE) {
        paste0("#' @inherit ", fun, " ", select)
      } else {
        paste(
          "#' Title",
          paste0("#' @inherit ", fun, " ", select),
          sep = "\n"
        )
      }
    },
    section = {
      paste(
        "#' Title",
        paste0("#' @inheritSection ", fun, " ", select),
        sep = "\n"
      )
    },
    param = {
      paste(
        "#' Title",
        paste0("#' @inheritParams ", fun),
        sep = "\n"
      )
    },
    dot_params = {
      paste(
        "#' Title",
        paste0("#' @inheritDotParams ", fun, " ", select),
        sep = "\n"
      )
    },
    stop("type not supported as: ", type)
  )
  fun_text <- switch(
    type,
    general = ,
    section = paste0(new_fun, " <- function() {}"),
    param = paste0(new_fun, " <- function(", select, ") {}"),
    dot_params = paste0(new_fun, " <- function(...) {}"),
    stop("type not supported as: ", type)
  )
  paste(roxygen_text, fun_text, sep = "\n")
}

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

#' Add roxygen2 header to a multi-line text
#'
#' @importFrom rex escape
#'
#' @inheritParams roc_eval_text
#'
#' @noRd
.add_roxy_header <- function(input) {
  # Get roxygen comment header
  roxy_comment <- .get_roxy_header()
  input %>%
    stringr::str_replace_all(paste0("\n(?<!", rex::escape(roxy_comment), ")"),
                             paste0("\n", rex::escape(roxy_comment)))
}

#' Add roxygen2 header to a multi-line text
#'
#' @importFrom rex escape
#'
#' @inheritParams roc_eval_text
#'
#' @noRd
.remove_roxy_header <- function(input) {
  # Get roxygen comment header
  roxy_comment <- .get_roxy_header()
  input %>%
    stringr::str_replace_all(paste0("\n", rex::escape(roxy_comment)), "\n") %>%
    stringr::str_replace(paste0("^", rex::escape(roxy_comment)), "") %>%
    stringr::str_replace(paste0(rex::escape(roxy_comment), "$"), "")
}

#' Get roxygen2 header
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
#' @param pattern Regular expression that mark the pattern of code block.
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
          # Remove roxygen comment headers from new lines
          .remove_roxy_header() %>%
          rlang::parse_expr() %>%
          eval() %>%
          # Add roxygen comment headers to new lines
          .add_roxy_header()
      }
    )
}
