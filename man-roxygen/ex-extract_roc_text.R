# Inherit a standard section, and leave the first letter as is
cat(
  extract_roc_text(stats::lm,
                   type = "general",
                   select = "description",
                   capitalize = NA)
)

# Inherit a self-defined section, and capitalize the first letter
cat(
  extract_roc_text(stats::lm,
                   type = "section",
                   select = "Using time series",
                   capitalize = TRUE)
)

# Inherit a parameter, and diffuse it into text
cat(
  paste0(
    "Here is the `formula` argument of `stats::lm`, defined as: ",
    extract_roc_text(stats::lm,
                     type = "param",
                     select = "formula",
                     capitalize = FALSE)
  )
)

# Inherit a set of dot params, and diffuse it into text
cat(
  paste0(
    "`lm_arg` is a named list of ",
    extract_roc_text(stats::lm,
                     type = "dot_params",
                     select = c("-formula", "-data"),
                     capitalize = FALSE)
  )
)
