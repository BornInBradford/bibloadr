source("R/bib_data_request_functions.R")

varfile <- "examples/variables.txt"

dat <- get_bibloadr_data(varfile = varfile, level = "child", label = T, allow_hidden = T)
meta <- get_bibloadr_meta(varfile = varfile)
codebook <- get_bibloadr_meta(varfile = varfile, type = "code")
