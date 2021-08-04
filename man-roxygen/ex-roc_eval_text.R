# Formulate a text version of a function with documentation
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

# Parse the 'roxygen' comments to Rd documentation
roc_eval_text(roxygen2::rd_roclet(), fun_text)[[1L]]
