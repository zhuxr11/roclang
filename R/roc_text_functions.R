# Functions for documentation text manipulation

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
#' @importFrom utils find
#' @importFrom roxygen2 roc_proc_text rd_roclet
#'
#' @param fun Function or character (of length 1L) indicating function name.
#' @param type Type of extraction. Please choose one from the following table
#' according to the \code{@tag} you would otherwise use if you would like to inherit
#' the section, parameter or set of dot-parameters as a whole:
#'
#' | \code{@tag} you would use | \code{type} you should choose |
#' | :-: | :-: |
#' | \code{@inherit} | \code{"general"} |
#' | \code{@inheritSection} | \code{"section"} |
#' | \code{@inheritParams} | \code{"param"} |
#' | \code{@inheritDotParams} | \code{"dot_params"} |
#'
#' @param select Selection of extraction based on \code{type}.
#' \describe{
#'   \item{\code{type = "general"}}{Character (of length 1L) indicating the section to extract}
#'   \item{\code{type = "section"}}{Character (of length 1L) indicating the section title to extract}
#'   \item{\code{type = "param"}}{Character (of length 1L) indicating the name of parameter to extract}
#'   \item{\code{type = "dot_params"}}{Character (of length 1L) or character vector to add or remove (with "-") parameters as \code{@inheritDotParams};
#'         if character vector provided, the elements are concatenated with spaces just as \code{@inheritDotParams} syntax,
#'         e.g. \code{"x y"} to inherit two parameters, \code{"-z"} to remove a parameter or \code{c("-x", "-y")} to remove two parameters}
#' }
#' @param capitalize Logical (of length 1L) indicating whether the first letter of the return should be capitalized.
#' Default to \code{capitalize = NA}, in which case the first letter of the return is left as is.
#'
#' @details To diffuse the function output into \code{\link[roxygen2]{roxygen2}} comments,
#' one may write the function documentation with inline code like this:
#' \preformatted{
#' #' Diffusion of function documentation with inline code
#' #'
#' #' @return Same as \code{\link[stats]{lm}}:
#' #' `r '\u0060r extract_roc_text(stats::lm, type = "general", select = "return")\u0060'`
#' my_fun <- function() {}
#' }
#'
#' or with code block like this:
#' \preformatted{
#' #' Diffusion of function documentation with code block
#' #'
#' #' @param lm_arg Named list of
#' #' `r paste('\u0060\u0060\u0060{r}', 'extract_roc_text(stats::lm,', '                 type = "dot_params",', '                 select = c("-formula", "-data"),', '                 capitalize = FALSE)', '\u0060\u0060\u0060', sep = "\n#\' ")`
#' my_fun <- function(lm_arg) {}
#' }
#'
#' @return Character (of length 1L) as a valid Rd character string to diffuse into \code{\link[roxygen2]{roxygen2}} comments.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#'   \item{0.1.1 Xiurui Zhu - Change the default of \code{capitalize} from \code{TRUE} to \code{NA}.}
#'   \item{0.1.1 Xiurui Zhu - Improve code security in evaluating the formal arguments of \code{fun}.}
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
  capitalize = NA
) {
  if (is.function(fun) == TRUE) {
    # Turn function into its name (character)
    fun <- deparse(substitute(fun))
  } else {
    if (is.character(fun) == FALSE) {
      stop("fun should be a function or character indicating a function, ",
           "e.g. stats::lm or 'stats::lm'")
    } else {
      if (length(fun) != 1L) {
        stop("fun should be length 1L")
      }
    }
  }
  type <- match.arg(type)
  # Parse select according to type
  select <- switch(
    type,
    # "general" and "param" only allow 1 selection (no spaces; space = multiple selection)
    general = ,
    param =  {
      if (length(select) != 1L) {
        stop("select should be length 1L for type = ", type)
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
        stop("select should be length 1L for type = ", type)
      } else if (identical(stringr::str_trim(select), "") == TRUE) {
        stop("select should not be blank for type = ", type)
      } else {
        select
      }
    },
    # "dot_params" allow multiple selections (concatenate with space)
    dot_params = paste(select, collapse = " "),
    stop("type not supported as: ", type)
  )
  # Check whether the selected parameter is one of the formalArgs of fun
  fun_function <- stringr::str_split(fun, pattern = ":{2,3}", n = 2L)[[1L]]
  if (length(fun_function) == 1L) {
    fun_function <- get(fun_function[1L], mode = "function")
  } else {
    fun_function <- get(fun_function[2L],
                        envir = asNamespace(fun_function[1L]),
                        mode = "function")
  }
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
      # It is OK to use "" to select all arguments as dot parameters
      if (identical(select, "") == FALSE) {
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
  }

  # Format a Roxygen2 function blocks
  roxygen_fun_text <- .assemble_text_fun(
    fun = if(stringr::str_detect(fun, ":{2,3}") == TRUE) {
      fun
    } else {
      # Attach package name
      fun_pgk_name <- utils::find(fun, mode = "function")
      if (stringr::str_detect(fun_pgk_name, "^package:") == TRUE) {
        fun_pgk_name <- stringr::str_replace(fun_pgk_name, "^package:", "")
        paste(fun_pgk_name, fun, sep = "::")
      } else {
        stop("Function ", fun, " should live in a package to get its documentation; ",
             "now its environment is: ", fun_pgk_name)
      }
    },
    type = type,
    select = select
  )

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

#' Assemble text version of a function to process extraction
#'
#' @inheritParams extract_roc_text
#' @param new_fun Character (of length 1L) as the assembled function name
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
