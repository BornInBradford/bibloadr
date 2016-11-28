source("R/bib_data_request_functions.R")

varfile <- "examples/variables.txt"

get_bibloadr_data(varfile = varfile, level = "child")
get_bibloadr_meta(varfile = varfile)
