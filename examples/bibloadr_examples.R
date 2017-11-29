#source("R/bibloadr_package/bib_data_request_functions.R")

require(bibloadr)

varfile_multi <- "C:/repos/bibloadr/examples/variables.txt"
varfile_single <- "C:/repos/bibloadr/examples/variables_single_extract.txt"

#dat <- get_bibloadr_data(varfile = varfile_single, level = "child", allow_hidden = T)
#meta <- get_bibloadr_meta(varfile = varfile_single)
#codebook <- get_bibloadr_meta(varfile = varfile_single, type = "code")

p <- make_data_package(varfile = varfile_single, level = "Pregnancy", allow_hidden = T,
                       format = c("stata", "csv", "rdata"),
                       package_file_stem = "MaternalBaselineQuestionnaire", 
                       package_name = "Maternal Baseline Questionnaire",
                       dict_template = "examples/BiB_data_dictionary.rmd")


srclist <- c("adminc", "mbagtt")


dat <- get_bibloadr_data(srclist = srclist, level = "child", allow_hidden = F)

save_bibloadr_dta(dat, "test_Stata_save.dta")

meta <- get_bibloadr_meta(srclist = srclist)
codebook <- get_bibloadr_meta(srclist = srclist, type = "code")
stats <- get_bibloadr_stats(srclist = srclist)


dat <- get_bibloadr_data(srclist = srclist, subcohort = c("has_mbqall", "has_eclprg"), level = "child", allow_hidden = F)

meta <- get_bibloadr_meta(srclist = srclist, subcohort = c("has_mbqall", "has_eclprg"))

stats <- get_bibloadr_stats(srclist = srclist, subcohort = c("has_mbqall", "has_eclprg"))

require(bibloadr)
coll <- get_collections()

