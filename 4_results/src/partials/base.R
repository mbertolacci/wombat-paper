library(logger)
library(dplyr, warn.conflicts = FALSE)

options(dplyr.summarise.inform = FALSE)

printf <- function(...) cat(sprintf(...))
paste_columns <- function(x) paste0(x, collapse = ' & ')
collapse0 <- function(x) paste0(x, collapse = '')
