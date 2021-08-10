library(testthat)
library(stringr)

test_that("extract_roc_text extracts parts of a function documentation", {
  expect_match(extract_roc_text(stats::lm, "general", "title", NA),
               "^Fitting Linear Models$")
  expect_match(extract_roc_text(stats::lm, "general", "description", NA),
               "^\\\\code\\{lm\\} is used to fit linear models\\.")
  expect_match(extract_roc_text(stats::lm, "general", "return", NA),
               "^\\\\code\\{lm\\} returns an object of")
  expect_match(extract_roc_text(stats::lm, "general", "author", NA),
               "^The design was inspired by")
  expect_match(extract_roc_text(stats::lm, "general", "references", NA),
               "^Chambers, J\\. M\\. \\(1992\\)")
  expect_match(extract_roc_text(stats::lm, "general", "examples", NA),
               "^require\\(graphics\\)")
  expect_match(extract_roc_text(stats::lm, "section", "Using time series", NA),
               "^Considerable care is needed when using")
  expect_match(extract_roc_text(stats::lm, "param", "formula", NA),
               "^an object of class")
  expect_match(extract_roc_text(stats::lm, "dot_params", "formula data", NA),
               "^Arguments passed on to")
  expect_match(extract_roc_text(stats::lm, "dot_params", c("-formula", "-data"), NA),
               "^Arguments passed on to")
})

test_that("extract_roc_text sets capitalization", {
  expect_match(extract_roc_text(stats::lm, "general", "title", TRUE),
               "^Fitting Linear Models$")
  expect_match(extract_roc_text(stats::lm, "general", "title", FALSE),
               "^fitting Linear Models$")
  # Capitalization should not work on non-letter characters, e.g. \\
  expect_match(extract_roc_text(stats::lm, "general", "description", TRUE),
               "^\\\\code\\{lm\\} is used to fit linear models\\.")
  expect_match(extract_roc_text(stats::lm, "general", "description", TRUE),
               "^\\\\code\\{lm\\} is used to fit linear models\\.")
})

test_that("extract_roc_text uses character function name input", {
  # Character function name with package name
  expect_match(extract_roc_text("stats::lm", "general", "title", NA),
               "^Fitting Linear Models$")
  # Character function name without package name
  expect_match(extract_roc_text("paste", "param", "sep", NA),
               "^a character string to separate the terms\\.")
})

test_that("extract_roc_text errors with fun as non-function/character input", {
  # fun as erroneous basic type
  expect_error(extract_roc_text(1L, "general", "title", NA), NULL)
  # fun as erroneous S3 type
  expect_error(extract_roc_text(stats::lm(y ~ x, data.frame(x = 1:5, y = 2:6)),
                                "general", "title", NA),
               NULL)
  # fun as erroneous S4 type
  methods::setClass("myS4Class", slots = c(foo = "numeric", bar = "numeric"))
  my_S4_obj <- new("myS4Class")
  expect_error(extract_roc_text(my_S4_obj, "general", "title", NA), NULL)
  invisible(methods::removeClass("myS4Class"))
  rm(my_S4_obj)
})

test_that("extract_roc_text errors wtih fun as character vector", {
  expect_error(extract_roc_text(c("stats::lm", "paste"), "general", "title", NA), NULL)
})

test_that("extract_roc_text errors wtih multiple/blank selections unless type = 'dot_params'", {
  expect_error(extract_roc_text(stats::lm, "general", c("title", "return"), NA), NULL)
  expect_error(extract_roc_text(stats::lm, "section", rep("Using time series", 2L), NA), NULL)
  expect_error(extract_roc_text(stats::lm, "param", c("formula", "data"), NA), NULL)
  # Spaces are regarded as multiple selections unless type = "section"
  expect_error(extract_roc_text(stats::lm, "general", "title return", NA), NULL)
  expect_error(extract_roc_text(stats::lm, "section", "Using time series", NA), NA)
  expect_error(extract_roc_text(stats::lm, "param", "formula data", NA), NULL)
  # When type = "dot_params" there can be multiple selections
  expect_error(extract_roc_text(stats::lm, "dot_params", c("formula", "data"), NA), NA)
  expect_error(extract_roc_text(stats::lm, "dot_params", "formula data", NA), NA)
  # Blank selection not allowed unless type = 'dot_params'
  expect_error(extract_roc_text(stats::lm, "general", "", NA), NULL)
  expect_error(extract_roc_text(stats::lm, "section", "", NA), NULL)
  expect_error(extract_roc_text(stats::lm, "param", "", NA), NULL)
  expect_error(extract_roc_text(stats::lm, "dot_params", "", NA), NA)
})

test_that("extract_roc_text errors when selecting ... with type = 'param' or 'dot_params'", {
  expect_error(extract_roc_text(stats::lm, "param", "...", NA), NULL)
  expect_error(extract_roc_text(stats::lm, "dot_params", "...", NA), NULL)
})

test_that("extract_roc_text errors when selecting non-existing formalArgs", {
  expect_error(extract_roc_text(stats::lm, "param", "datum", NA), NULL)
  # For type = "dot_params", test a mixture of existing/non-existing formalArgs
  expect_error(extract_roc_text(stats::lm, "dot_params", "datum", NA), NULL)
  expect_error(extract_roc_text(stats::lm, "dot_params", "formulae datum", NA), NULL)
  expect_error(extract_roc_text(stats::lm, "dot_params", "formula datum", NA), NULL)
  expect_error(extract_roc_text(stats::lm, "dot_params", "formula -datum", NA), NULL)
})

test_that("extract_roc_text errors when selecting non-existing function or function without documentation", {
  expect_error(extract_roc_text("foobar", "general", "title", NA), NULL)
  # Function without documentation should also result in an error
  foobar <<- function() {}
  expect_error(extract_roc_text("foobar", "general", "title", NA), NULL)
  rm(foobar, envir = .GlobalEnv)
})
