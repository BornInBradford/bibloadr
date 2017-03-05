#source("R/bibloadr_package/bib_data_request_functions.R")

require(bibloadr)

varfile_multi <- "examples/variables.txt"
varfile_single <- "examples/variables_single_extract.txt"

dat <- get_bibloadr_data(varfile = varfile_single, level = "child", allow_hidden = T)
meta <- get_bibloadr_meta(varfile = varfile_single)
codebook <- get_bibloadr_meta(varfile = varfile_single, type = "code")

p <- make_data_package(varfile = varfile_single, level = "child", allow_hidden = T,
                       package_file_stem = "test_data_package", 
                       package_name = "Test data package for bibloadr development",
                       dict_template = "examples/BiB_data_dictionary.rmd")


