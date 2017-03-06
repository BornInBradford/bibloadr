#source("R/bibloadr_package/bib_data_request_functions.R")

require(bibloadr)

varfile_multi <- "examples/variables.txt"

p <- make_data_package_multi(varfile = varfile_multi, level = "child", allow_hidden = FALSE,
                       package_file_stem = "test_data_package", 
                       package_name = "Test data package for bibloadr development",
                       dict_template = "examples/BiB_data_dictionary.rmd",
                       full_dict = TRUE, multi_dict = FALSE, combine_wide = TRUE)


