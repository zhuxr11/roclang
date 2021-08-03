library(testthat)
library(stringr)

fun_text <- '
#\' \\code{iris} is a `r nrow(iris)`-row matrix.
#\'
#\' \\code{iris} matrix has
#\' ```{r results="hold"}
#\' ncol(iris)
#\' ```
#\' columns.
print_iris <- function() iris
'
test_that("roc_eval_text parses diffused roxygen comments to Rd text", {
  expect_true(roc_eval_text(roxygen2::rd_roclet(), fun_text)[[1L]]$is_valid())
})
