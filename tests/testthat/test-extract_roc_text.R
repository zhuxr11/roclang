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
