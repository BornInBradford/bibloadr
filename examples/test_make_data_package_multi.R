#source("R/bibloadr_package/bib_data_request_functions.R")

require(bibloadr)

# varfile_multi <- "examples/b18_variables.txt"
# 
# p <- make_data_package_multi(varfile = varfile_multi, level = "child", allow_hidden = FALSE,
#                        package_file_stem = "test_multi_data_package", 
#                        package_name = "Test data package for bibloadr development",
#                        dict_template = "examples/BiB_data_dictionary.rmd",
#                        full_dict = TRUE, multi_dict = TRUE, combine_wide = TRUE)



p <- make_data_package_multi(srclist = c("bioca1", "bioca2", "biochb", "biomob", "biopyb"), 
                             allow_hidden = FALSE,
                             package_file_stem = "test_multi_data_package", 
                             package_name = "Test data package for bibloadr development",
                             dict_template = "examples/BiB_data_dictionary.rmd",
                             full_dict = TRUE, multi_dict = TRUE, combine_wide = FALSE,
                             preserve_levels = TRUE)

