
# Generate XML for data request, to be submitted to database function
# returns character containing XML string
bibloadr_request_xml <- function(namelist = character(0), nametype = "variable",
                             level = character(0), cbtype = character(0), subclist = character(0),
                             allow_null_ids = FALSE, allow_hidden = FALSE, user = "BiBUser", log = FALSE, testmode = FALSE,
                             cohort = "BiB") {
    
    # end of line for xml building
    eol <- "\n"
    
    # required xml element tags
    el_DR <- c("<DataRequest>","</DataRequest>")
    if(toupper(nametype) == "SOURCE") {
        el_Nlist <- c("<Sources>","</Sources>")
        el_Nitem <- c("<Source>","</Source>")
        el_Nattr <- c("<SourceName>","</SourceName>")
    } else if(toupper(nametype) == "VARIABLE") {
        el_Nlist <- c("<Variables>","</Variables>")
        el_Nitem <- c("<Variable>","</Variable>")
        el_Nattr <- c("<VariableName>","</VariableName>")
    }
    
    el_UN <- paste0("<User>",user,"</User>",eol)
    el_CO <- paste0("<Cohort>",cohort,"</Cohort>",eol)
    
    # optional xml elements
    el_CB <- ifelse(length(cbtype)>0, paste0("<CodeBook>",cbtype,"</CodeBook>",eol), "")
    el_ML <- ifelse(length(level)>0, paste0("<MeasurementLevel>",level,"</MeasurementLevel>",eol), "")
    el_AN <- ifelse(allow_null_ids, paste0("<AllowNullIds>On</AllowNullIds>",eol), "")
    el_AH <- ifelse(allow_hidden, paste0("<AllowHidden>On</AllowHidden>",eol), "")
    if (log) {
        el_LG <- paste0("<LogRequest>On</LogRequest>",eol)
    } else {
        el_LG <- ""
    }
    el_TM <- ifelse(testmode, paste0("<TestMode>On</TestMode>",eol), "")
    el_SC <- ""
    if(length(subclist)>0) {
        subclist <- paste0("<Subcohort>", eol, "<SubcohortName>", subclist, "</SubcohortName>", 
                           eol, "</Subcohort>", eol)
        el_SC <- paste0("<Subcohorts>", eol, paste0(subclist, sep = "", collapse = ""), "</Subcohorts>", eol)
    }
    
    # build string vector of xml code and formatting elements
    # first, header up to list of names
    xmlv <- paste0(el_DR[1], eol, el_CO, el_CB, el_ML, el_AN, el_AH, el_LG, el_UN, el_TM, el_SC, el_Nlist[1], eol)
    # now output name list
    namelist <- paste0(el_Nitem[1], eol, el_Nattr[1], namelist, el_Nattr[2], 
                       eol, el_Nitem[2], eol)
    # finally, pull together and add footer
    xmlv <- paste0(xmlv, paste0(namelist, sep = "", collapse = ""), el_Nlist[2], eol, el_DR[2], eol)
    
    return(xmlv)

}

# Read a name file and return as character vector
# strips zero length strings (blank lines)
read_namefile <- function(namefile) {
  
  namelist <- readLines(namefile)

  namelist <- namelist[nchar(namelist) > 0]
  
  return(namelist)
  
}

# return a connection to the bibloadr database for submitting the data request XML
# stores it in global var BIBLOADR_db
open_bibloadr_db <- function(devmode = FALSE) {
  
  require(RODBC)
  
  if(devmode) {
    connection_string <- "Driver={SQL Server Native Client 10.0};Server=BHTS-RESEARCHDV;Database=ResearchMeta;Trusted_Connection=yes"
  } else {
    connection_string <- "Driver={SQL Server Native Client 10.0};Server=BHTS-RESEARCH1\\BIB;Database=ResearchMeta;Trusted_Connection=yes"
  }
  
  BIBLOADR_db <<- odbcDriverConnect(connection_string)
  
  return(BIBLOADR_db)
  
}

# close connection to bibloadr database, assuming connection was opened using open_bibloadr_db
# i.e. connection is stored in BIBLOADR_db
close_bibloadr_db <- function() {
  
  odbcClose(BIBLOADR_db)

}

# submit sql query to bibloadr database
# returns dataframe or error string
bibloadr_query <- function(query_string, devmode = FALSE, as.is = FALSE) {
  
  # do database bit to get data
  db <- open_bibloadr_db(devmode)
  
  dat <- sqlQuery(db, query_string, stringsAsFactors = FALSE, as.is = as.is)
  
  close_bibloadr_db()
  
  # return data frame
  return(dat)
  
}

# constructs a namelist vector from a file and a character vector
make_namelist <- function(namefile = character(0), namelist = character(0)) {
  
  # concatenate namelist names to namefile names
  if(length(namefile) > 0 && file.exists(namefile)) namelist <- c(read_namefile(namefile),namelist)
  
  return(namelist)

}

# label a dataframe's columns using meta-data provided in var_labels
label_columns <- function (dat, var_labels) {
  
  # drop unwanted study IDs
  var_labels <- var_labels[var_labels$VariableName %in% names(dat), ]
  
  if (nrow(var_labels) != length(dat)) stop("Number of data columns does not match number of metadata records.")
  
  # reorder var_labels to match columns in dat
  var_labels <- var_labels[match(names(dat), var_labels$VariableName),]
  
  # add variable attributes
  attr(dat, "VariableLabel") <- c(var_labels$VariableLabel)
  attr(dat, "Description") <- c(var_labels$Description)
  attr(dat, "ValueType") <- c(var_labels$ValueType)
  attr(dat, "CodeSetName") <- c(var_labels$CodeSetName)
  attr(dat, "MeasureType") <- c(var_labels$MeasureType)
  attr(dat, "SourceName") <- c(var_labels$SourceName)
  
  return(dat)
  
}

# converts column x of dat to factor based on labels from val_labels
# variables should be in the same order in dat and val_labels
format_factor_column <- function(dat, val_labels, col_name) {
  
  data_column <- which(names(dat) == col_name)
  label_row <- which(val_labels$VariableName == col_name)
  
  dat[data_column] <- factor(dat[[data_column]], 
                             levels=val_labels$Value[label_row], 
                             labels=val_labels$ValueLabel[label_row])
  
  return(dat)
  
}

# takes data frame annotated with attributes describing intended value types
# and does some reformatting
format_column_types <- function(dat, val_labels) {
  
  for(x in 1:length(dat)) {
    
    value_type <- attr(dat, "ValueType")[x]
    
    if(!is.na(value_type)) {
      if(value_type == "Integer") dat[x] <- as.integer(dat[[x]])
      if(value_type == "Continuous") dat[x] <- as.numeric(dat[[x]])
      if(value_type == "Date") dat[x] <- as.Date(dat[[x]])
      if(value_type == "Text") dat[x] <- as.character(dat[[x]])
      if(value_type == "Categorical") dat <- format_factor_column(dat, val_labels, names(dat)[x])
    }
    
  }
  
  return(dat)
  
}

# takes data request parameters and submits to bibloadr db, returning data frame
# concatenates variables in varlist character vector to variables in varfile
get_bibloadr_data <- function(varfile = character(0), varlist = character(0), srclist = character(0), 
                              level = character(0), subcohort = character(0),
                              allow_null_ids = FALSE, allow_hidden = FALSE, log = FALSE, testmode = FALSE, 
                              cohort = "BiB", devmode = FALSE) {
  
  # concatenate varlist vars to varfile vars
  namelist <- make_namelist(varfile, varlist)
  nametype <- "Variable"
  
  # have we got variables? if not, check srclist
  # if this is empty, exit
  if(length(namelist) == 0) {
    if(length(srclist) == 0) {
      stop("No variables found in request.")
    } else {
      namelist <- srclist
      nametype <- "Source"
    }
  }
  
  # SQL string building
  sql_start <- paste0("EXEC [ResearchMeta].[Explorer].[Get", nametype, "Data]\n@DataRequest = N'")
  
  # needs to find logged in username
  sql_xml <- bibloadr_request_xml(namelist = namelist, nametype = nametype, level = level, subclist = subcohort, 
                                  allow_null_ids = allow_null_ids, allow_hidden = allow_hidden, user = Sys.info()[["login"]], 
                                  log = log, testmode = testmode, cohort = cohort)
  
  sql_end <- "';\n"
  
  query_string <- paste0(sql_start, sql_xml, sql_end)
  
  dat <- bibloadr_query(query_string, devmode)
  
  # label and format variables if bibloadr_query did not return error string
  # drop hidden variables from meta data if allow_hidden == F
  if (typeof(dat) != "character") {
    var_labels <- get_bibloadr_meta (varfile = varfile, varlist = varlist, srclist = srclist, subcohort = subcohort, type = "varsection",
                                     testmode = testmode, devmode = devmode)
    val_labels <- get_bibloadr_meta (varfile = varfile, varlist = varlist, srclist = srclist, subcohort = subcohort, type = "code",
                                     testmode = testmode, devmode = devmode)
    
    if(!allow_hidden) var_labels <- var_labels[var_labels$Hidden == 0, ]
    
    dat <- label_columns (dat, var_labels)
    
    dat <- format_column_types(dat, val_labels)
    
  }
  
  if(!is.data.frame(dat)) stop(dat)
  
  # return data frame
  return(dat)
  
}


# takes metadata request parameters and submits to bibloadr db, returning data frame
# concatenates variables in varlist character vector to variables in varfile
# default type is varlong
get_bibloadr_meta <- function(varfile = character(0), varlist = character(0), subcohort = character(0),
                              srclist = character(0),
                              type = "varlong", testmode = FALSE, devmode = FALSE) {
  
  # concatenate varlist vars to varfile vars
  namelist <- make_namelist(varfile, varlist)
  nametype <- "Variable"
  
  # have we got variables? if not, check srclist
  # if this is empty, exit
  if(length(namelist) == 0) {
    if(length(srclist) == 0) {
      stop("No variables found in request.")
    } else {
      namelist <- srclist
      nametype <- "Source"
    }
  }
  
  # SQL string building
  sql_start <- paste0("EXEC [ResearchMeta].[Explorer].[Get", nametype, "Meta]\n@DataRequest = N'")
  
  sql_xml <- bibloadr_request_xml(namelist = namelist, subclist = subcohort, nametype = nametype, cbtype = type, testmode = testmode)
  
  sql_end <- "';\n"
  
  query_string <- paste0(sql_start, sql_xml, sql_end)
  
  dat <- bibloadr_query(query_string, devmode)
  
  if(!is.data.frame(dat)) stop(dat)
  
  # return data frame
  return(dat)
  
}


# takes variable list, gets sources and requests stats for each
# then merges them into one dataframe
get_bibloadr_stats <- function(varfile = character(0), varlist = character(0), srclist = character(0),
                               subcohort = character(0), cohort = "BiB", 
                               testmode = FALSE, devmode = FALSE) {
  
  # get sources
  src <- get_bibloadr_meta(varfile = varfile, varlist = varlist, srclist = srclist, subcohort = subcohort, type = "source",
                            testmode = testmode, devmode = devmode)
  
  # get variables
  var <- get_bibloadr_meta(varfile = varfile, varlist = varlist, srclist = srclist, subcohort = subcohort, type = "var",
                           testmode = testmode, devmode = devmode)
  
  var <- var[1]
  
  dat <- list()
  
  for(x in 1:nrow(src)) {
    
    dat[[x]] <- get_source_stats(source_name = src$SourceName[x],
                                 cohort = cohort, 
                                 testmode = testmode,
                                 devmode = devmode)
    
    dat[[x]] <- merge(var, dat[[x]], by = "VariableName")
    
  }
  
  dat <- do.call(rbind, dat)
  
  if(!is.data.frame(dat)) stop(dat)
  
  # return data frame
  return(dat)
  
  
  
  
}



# takes single sourcename parameter and retrieves stats
# returns dataframe
get_source_stats <- function(source_name = character(0), cohort = "BiB",
                             testmode = FALSE, devmode = FALSE) {
  
  testmode <- ifelse(testmode, 1, 0)

  query_string <- paste0("EXEC [ResearchMeta].[Explorer].[GetSourceStats] @SourceName = '", 
                        source_name, "', @TestMode = ", testmode, ", @CohortName = '", cohort, "';")

  dat <- bibloadr_query(query_string, devmode, as.is = T)
  
  if(!is.data.frame(dat)) stop(dat)
  
  # return data frame
  return(dat)
  
}


# returns view of source names in collections
# with collections descriptions
get_collections <- function(testmode = FALSE, devmode = FALSE) {
  
  testmode <- ifelse(testmode, "Test", "ResearchData")
  
  query_string <- paste0("SELECT * FROM [ResearchMeta].[", testmode, "].[CollectionCatalogue];")
  
  dat <- bibloadr_query(query_string, devmode, as.is = T)
  
  if(!is.data.frame(dat)) stop(dat)
  
  # return data frame
  return(dat)
  
}


# save data frame to dta file
# assumes version 13 of stata
save_bibloadr_dta <- function(dat, file = character(0), about = NULL, version = 13) {
  
  require(readstata13)

  # find text fields in dat and convert NAs to ""
  convert_text_NAs <- which(attr(dat, "ValueType") == "Text")
  if(length(convert_text_NAs > 0)) dat[,convert_text_NAs][is.na(dat[,convert_text_NAs])] <- ""
  
  # add var.labels attribute replacing NAs with ""
  var.labels <- attr(dat, "VariableLabel")
  var.labels[is.na(var.labels)] <- ""
  attr(dat, "var.labels") <- var.labels
  
  # save file
  save.dta13(data = dat, file = file, data.label = about, version = version)
  
}


# save data frame to csv file
save_bibloadr_csv <- function(dat, file = character(0)) {
  
  # find text fields in dat and convert NAs to ""
  convert_text_NAs <- which(attr(dat, "ValueType") == "Text")
  if(length(convert_text_NAs > 0)) dat[,convert_text_NAs][is.na(dat[,convert_text_NAs])] <- ""
  
  # save file
  write.csv(dat, file = file, na = "", row.names = F)
  
}

# save data frame to RData file
save_bibloadr_rdata <- function(dat, file = character(0), about = NULL) {
  
  # find text fields in dat and convert NAs to ""
  convert_text_NAs <- which(attr(dat, "ValueType") == "Text")
  if(length(convert_text_NAs > 0)) dat[,convert_text_NAs][is.na(dat[,convert_text_NAs])] <- ""
  
  save_list <- "dat"
  
  if(!is.null(about)) save_list <- c(save_list, "about")
  
  # save file
  save(file = file, list = save_list)
  
}


# save data dictionary
# only supports varfile input, pdf output, no test or dev options yet
# this is due to how the current rmd template is set up
save_bibloadr_dict <- function(varfile = character(0), varlist = character(0), srclist = character(0), subcohort = character(0), 
                               output_file = NULL, data_package_name = NULL, dict_template = NULL) {
  
  # NB devmode not implemented for data dictionary - would need to be implemented in Rmd template
  database_version <- get_bibloadr_db_version()

  rmarkdown::render(input = dict_template,  
                    output_format = "pdf_document",
                    output_file = output_file)
  
}


# get database version
get_bibloadr_db_version <- function(devmode = FALSE) {
  
  v <- bibloadr_query(query_string = "select [Explorer].[GetVersion]();", devmode = devmode, as.is = FALSE)
  
  v <- v[,1]

  return(v)
  
}

# make package for single data file
# the data request needs to succeed
# so only one multiobs source allowed
make_data_package <- function(varfile = character(0), varlist = character(0), srclist = character(0), level = character(0), 
                              subcohort = character(0), allow_null_ids = FALSE, allow_hidden = FALSE, log = FALSE, testmode = FALSE, 
                              cohort = "BiB", devmode = FALSE, format = "stata", stata_version = 13,
                              package_directory = getwd(),
                              package_file_stem = character(0), package_name = character(0),
                              dict_template = "BiB_data_dictionary.rmd",
                              output_dict = TRUE) {
  
  if(!file.exists(dict_template)) {
    search_dict_template <- paste0(package_directory, "/", dict_template)
    if(!file.exists(search_dict_template)) {
      stop(paste0("Dictionary template not found: ", dict_template))
    } else {
      dict_template <- search_dict_template
    }
  }
  
  dat <- get_bibloadr_data(varfile = varfile, varlist = varlist, srclist = srclist, level = level, subcohort = subcohort,
                           allow_null_ids = allow_null_ids, allow_hidden = allow_hidden,
                           log = log, testmode = testmode, cohort = cohort, devmode = devmode)
  
  about <- paste0(package_name, " | ", get_bibloadr_db_version())
  
  if("stata" %in% format) save_bibloadr_dta(dat = dat, file = paste0(package_directory, "/", package_file_stem, "_Data.dta"), 
                                            about = about, version = stata_version)
  
  if("csv" %in% format) save_bibloadr_csv(dat = dat, file = paste0(package_directory, "/", package_file_stem, "_Data.csv"))
  
  if("rdata" %in% format) save_bibloadr_rdata(dat = dat, file = paste0(package_directory, "/", package_file_stem, "_Data.Rdata"),
                                            about = about)
  
  if(output_dict) save_bibloadr_dict(varfile = varfile, 
                                     varlist = varlist, subcohort = subcohort, 
                                     srclist = srclist, output_file = paste0(package_directory, "/", package_file_stem, "_Dict.pdf"),
                     data_package_name = package_name,
                     dict_template = dict_template)
  
}

# make multi-datafile data package
# multi-obs sources are split up
# subcohort can be passed as vector, in which case all must be
# valid for the level of each source, e.g. a child level subcohort
# will cause problems with a mother level source
# alternatively subcohort can be taken from the Collect table
# if collect_subcohort = TRUE
make_data_package_multi <- function(varfile = character(0), varlist = character(0), srclist = srclist, level = character(0), 
                                    subcohort = character(0),
                              allow_null_ids = FALSE, allow_hidden = FALSE, log = FALSE, testmode = FALSE, 
                              cohort = "BiB", devmode = FALSE, format = "stata", stata_version = 13,
                              package_directory = getwd(),
                              package_file_stem = character(0), package_name = character(0),
                              dict_template = "BiB_data_dictionary.rmd",
                              full_dict = TRUE, multi_dict = FALSE, combine_wide = TRUE,
                              preserve_levels = FALSE, collect_subcohort = FALSE) {
  
  if(!file.exists(dict_template)) {
    search_dict_template <- paste0(package_directory, "/", dict_template)
    if(!file.exists(search_dict_template)) {
      stop(paste0("Dictionary template not found: ", dict_template))
    } else {
      dict_template <- search_dict_template
    }
  }
  
  
  # validate parameters
  if(preserve_levels == FALSE && level == character(0)) stop("level is required when preserve_levels is FALSE")
  if(preserve_levels == TRUE && combine_wide == TRUE) {
    if(level == character(0)) {
      stop("preserve_levels is ignored for combined wide and level is required")
    } else {
      warning("preserve_levels is on so level is only used for combined wide sources")
    }
  }
  if(length(subcohort) > 0 && collect_subcohort == TRUE) stop("collect_subcohort cannot be used when subcohort is passed explicitly")
  
  # output full dictionary if requested
  if(full_dict) save_bibloadr_dict(varfile = varfile, varlist = varlist, subcohort = subcohort, srclist = srclist, 
                                    output_file = paste0(package_directory, "/", package_file_stem, "_Full_Dict.pdf"),
                                    data_package_name = package_name,
                                    dict_template = dict_template)
  
  # for working out splits
  source_properties <- get_bibloadr_meta(varfile = varfile, varlist = varlist, srclist = srclist, subcohort = subcohort, type = "sourceproperties")
  var_source <- get_bibloadr_meta(varfile = varfile, varlist = varlist, srclist = srclist, subcohort = subcohort, type = "varlong")
  
  # vector of sources to split
  # if wide to be combined, only select long sources
  # need to remove dummy study ID sourcename as it has no level
  split_sources <- source_properties$SourceName[!source_properties$SourceName == "studid"]
  if (combine_wide) split_sources <- source_properties$SourceName[source_properties$MultipleObservations == 1]
  wide_sources <- source_properties$SourceName[source_properties$MultipleObservations == 0 & !source_properties$SourceName == "studid"]
  
  # get subcohorts from collections if requested
  split_subcohorts <- subcohort
  wide_subcohorts <- subcohort
  if(collect_subcohort) {
    coll <- get_collections()
    split_subcohorts <- coll$Subcohort[match(split_sources, coll$SourceName)]
    wide_subcohorts <- coll$Subcohort[match(wide_sources, coll$SourceName)]
  }
  
  # output combined wide if requested
  if (combine_wide && length(wide_sources) > 0) {
    
    wide_vars <- var_source$VariableName[var_source$SourceName %in% wide_sources]
    
    if(is.na(wide_subcohorts)) wide_subcohorts <- character(0)
    
    make_data_package(varlist = wide_vars, level = level, subcohort = wide_subcohorts,
                      allow_null_ids = allow_null_ids, allow_hidden = allow_hidden, 
                      log = log, testmode = testmode, cohort = cohort, devmode = devmode, 
                      format = format, stata_version = stata_version,
                      package_directory = package_directory,
                      package_file_stem = paste0(package_file_stem, "_Wide"), 
                      package_name = paste0(package_name, " (Wide)"),
                      dict_template = dict_template,
                      output_dict = multi_dict)
    
  }
  
  # output any sources that need to be done separately
  if (length(split_sources) > 0) {
    
    for(x in 1:length(split_sources)) {
      
      split_vars <- var_source$VariableName[var_source$SourceName == split_sources[x]]
      
      if(preserve_levels) level <- source_properties$MeasurementLevel[source_properties$SourceName == split_sources[x]]
      
      subcohort <- split_subcohorts[x]
      
      if(is.na(subcohort)) subcohort = character(0)
      
      make_data_package(varlist = split_vars, level = level, subcohort = subcohort,
                        allow_null_ids = allow_null_ids, allow_hidden = allow_hidden, 
                        log = log, testmode = testmode, cohort = cohort, devmode = devmode, 
                        format = format, stata_version = stata_version,
                        package_directory = package_directory,
                        package_file_stem = paste0(package_file_stem, "_", split_sources[x]), 
                        package_name = paste0(package_name, " (", split_sources[x], ")"),
                        dict_template = dict_template,
                        output_dict = multi_dict)
      
      
    }
    
  }

  
}

