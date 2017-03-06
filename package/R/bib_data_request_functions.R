
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
    if(nametype == "source") {
        el_Nlist <- c("<Sources>","</Sources>")
        el_Nitem <- c("<Source>","</Source>")
        el_Nattr <- c("<SourceName>","</SourceName>")
    } else if(nametype == "variable") {
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
  if(length(namefile) > 0) namelist <- c(read_namefile(namefile),namelist)
  
  return(namelist)

}

# label a dataframe's columns using meta-data provided in var_labels and val_labels
label_columns <- function (dat, var_labels, val_labels, 
                           ignore = c("ChildID", "PregnancyID", "MotherID")) {
  
  # how many columns to ignore - these must be at the left hand side
  n_ignore <- sum(names(dat) %in% ignore)
  # create ignore vector
  skip <- rep(NA, n_ignore)
  # add variable attributes
  attr(dat, "VariableLabel") <- c(skip, var_labels$VariableLabel)
  attr(dat, "Description") <- c(skip, var_labels$Description)
  attr(dat, "ValueType") <- c(skip, var_labels$ValueType)
  attr(dat, "CodeSetName") <- c(skip, var_labels$CodeSetName)
  attr(dat, "MeasureType") <- c(skip, var_labels$MeasureType)
  attr(dat, "SourceName") <- c(skip, var_labels$SourceName)
  
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
get_bibloadr_data <- function(varfile = character(0), varlist = character(0), level = character(0),
                              allow_null_ids = FALSE, allow_hidden = FALSE, log = FALSE, testmode = FALSE, 
                              cohort = "BiB", devmode = FALSE) {
  
  # concatenate varlist vars to varfile vars
  varlist <- make_namelist(varfile, varlist)
  
  # if we still don't have any, exit with error
  if(length(varlist) == 0) stop("No variables found in request.")
  
  # SQL string building
  sql_start <- "EXEC [ResearchMeta].[Explorer].[GetVariableData]\n@DataRequest = N'"
  
  # needs to find logged in username
  sql_xml <- bibloadr_request_xml(namelist = varlist, level = level, allow_null_ids = allow_null_ids, 
                                  allow_hidden = allow_hidden, user = "MasonD", log = log, testmode = testmode,
                                  cohort = cohort)
  
  sql_end <- "';\n"
  
  query_string <- paste0(sql_start, sql_xml, sql_end)
  
  dat <- bibloadr_query(query_string, devmode)
  
  # label and format variables bibloadr_query did not return error string
  if (typeof(dat) != "character") {
    var_labels <- get_bibloadr_meta (varfile = varfile, varlist = varlist, type = "varlong",
                                     testmode = testmode, devmode = devmode)
    val_labels <- get_bibloadr_meta (varfile = varfile, varlist = varlist, type = "code",
                                     testmode = testmode, devmode = devmode)
    dat <- label_columns (dat, var_labels, val_labels)
    
    dat <- format_column_types(dat, val_labels)
    
  }
  
  # return data frame
  return(dat)
  
}


# takes metadata request parameters and submits to bibloadr db, returning data frame
# concatenates variables in varlist character vector to variables in varfile
# default type is varlong
get_bibloadr_meta <- function(varfile = character(0), varlist = character(0),
                              type = "varlong", testmode = FALSE, devmode = FALSE) {
  
  # concatenate varlist vars to varfile vars
  varlist <- make_namelist(varfile, varlist)
  
  # if we still don't have any, exit with error
  if(length(varlist) == 0) stop("No variables found in request.")
  
  # SQL string building
  sql_start <- "EXEC [ResearchMeta].[Explorer].[GetVariableMeta]\n@DataRequest = N'"
  
  sql_xml <- bibloadr_request_xml(namelist = varlist, cbtype = type, testmode = testmode)
  
  sql_end <- "';\n"
  
  query_string <- paste0(sql_start, sql_xml, sql_end)
  
  dat <- bibloadr_query(query_string, devmode)
  
  # return data frame
  return(dat)
  
}


# takes variable list, gets sources and requests stats for each
# then merges them into one dataframe
get_bibloadr_stats <- function(varfile = character(0), varlist = character(0),
                               testmode = FALSE, devmode = FALSE) {
  
  # concatenate varlist vars to varfile vars
  varlist <- make_namelist(varfile, varlist)
  
  # if we still don't have any, exit with error
  if(length(varlist) == 0) stop("No variables found in request.")
  
  # get sources
  src <- get_bibloadr_meta(varfile = varfile, varlist = varlist, type = "source",
                            testmode = testmode, devmode = devmode)
  
  # get variables
  var <- get_bibloadr_meta(varfile = varfile, varlist = varlist, type = "var",
                           testmode = testmode, devmode = devmode)
  
  var <- var[1]
  
  dat <- list()
  
  for(x in 1:nrow(src)) {
    
    dat[[x]] <- get_source_stats(source_name = src$SourceName[x],
                               testmode = testmode,
                               devmode = devmode)
    
    dat[[x]] <- merge(var, dat[[x]], by = "VariableName")
    
  }
  
  dat <- do.call(rbind, dat)
    
  # return data frame
  return(dat)
  
  
  
  
}



# takes single sourcename parameter and retrieves stats
# returns dataframe
get_source_stats <- function(source_name = character(0), 
                             testmode = FALSE, devmode = FALSE) {
  
  testmode = ifelse(testmode, 1, 0)

  query_string <- paste0("EXEC [ResearchMeta].[Explorer].[GetSourceStats] @SourceName = '", 
                        source_name, "', @TestMode = ", testmode, ";")

  dat <- bibloadr_query(query_string, devmode, as.is = T)
  
  # return data frame
  return(dat)
  
}


# save data frame to dta file
# assumes version 13 of stata
save_bibloadr_dta <- function(dat, file = character(0), about = NULL, version = 13) {
  
  require(readstata13)

  # find text fields in dat and convert NAs to ""
  convert_text_NAs <- which(attr(dat, "ValueType") == "Text")
  dat[,convert_text_NAs][is.na(dat[,convert_text_NAs])] <- ""
  
  save.dta13(data = dat, file = file, data.label = about, version = version)
  
}


# save data dictionary
# only supports varfile input, pdf output, no test or dev options yet
# this is due to how the current rmd template is set up
save_bibloadr_dict <- function(varfile = character(0), varlist = character(0), output_file = NULL,
                               database_version = NULL, data_package_name = NULL,
                               dict_template = NULL) {
  
  rmarkdown::render(input = dict_template,  
                    output_format = "pdf_document",
                    output_file = output_file)
  
}


# get database version
get_bibloadr_db_version <- function(devmode = FALSE) {
  
  # something like:
  #query_string <- "SELECT TOP 1 Version FROM Reference.Version ORDER BY Date DESC;"
  
  #v <- bibloadr_query(query_string, devmode = FALSE, as.is = FALSE)
  
  #v <- v$Version[1]
  
  #temporary workaround as this table doesn't exist
  v <- "BUILD-JAN2017"
  
  return(v)
  
}

# make package for single data file
# the data request needs to succeed
# so only one multiobs source allowed
make_data_package <- function(varfile = character(0), varlist = character(0), level = character(0),
                              allow_null_ids = FALSE, allow_hidden = FALSE, log = FALSE, testmode = FALSE, 
                              cohort = "BiB", devmode = FALSE, format = "stata", stata_version = 13,
                              package_directory = getwd(),
                              package_file_stem = character(0), package_name = character(0),
                              dict_template = "BiB_data_dictionary.rmd",
                              output_dict = TRUE) {
  
  
  dat <- get_bibloadr_data(varfile = varfile, varlist = varlist, level = level,
                           allow_null_ids = allow_null_ids, allow_hidden = allow_hidden,
                           log = log, testmode = testmode, cohort = cohort, devmode = devmode)
  
  database_version <- get_bibloadr_db_version(devmode = devmode)
  
  about <- paste0(package_name, " | ", database_version)
  
  if(format == "stata") save_bibloadr_dta(dat = dat, file = paste0(package_directory, "/", package_file_stem, "_Data.dta"), 
                                          about = about, version = stata_version)
  
  if(output_dict) save_bibloadr_dict(varfile = paste0(package_directory, "/", varfile), 
                                     varlist = varlist, output_file = paste0(package_directory, "/", package_file_stem, "_Dict.pdf"),
                     database_version = database_version, data_package_name = package_name,
                     dict_template = paste0(package_directory, "/", dict_template))
  
}

# make multi-datafile data package
# multi-obs sources are split up
make_data_package_multi <- function(varfile = character(0), varlist = character(0), level = character(0),
                              allow_null_ids = FALSE, allow_hidden = FALSE, log = FALSE, testmode = FALSE, 
                              cohort = "BiB", devmode = FALSE, format = "stata", stata_version = 13,
                              package_directory = getwd(),
                              package_file_stem = character(0), package_name = character(0),
                              dict_template = "BiB_data_dictionary.rmd",
                              full_dict = TRUE, multi_dict = FALSE, combine_wide = TRUE) {

  # output full dictionary if requested
  if(full_dict) save_bibloadr_dict(varfile = paste0(package_directory, "/", varfile), varlist = varlist, 
                                    output_file = paste0(package_directory, "/", package_file_stem, "_Full_Dict.pdf"),
                                    database_version = get_bibloadr_db_version(devmode = devmode), 
                                    data_package_name = package_name,
                                    dict_template = paste0(package_directory, "/", dict_template))
  
  # for working out splits
  source_properties <- get_bibloadr_meta(varfile = varfile, varlist = varlist, type = "sourceproperties")
  var_source <- get_bibloadr_meta(varfile = varfile, varlist = varlist, type = "varlong")
  
  # vector of sources to split
  # if wide to be combined, only select long sources
  split_sources <- ifelse(combine_wide, source_properties$SourceName[source_properties$MultipleObservations == 1],
                                        source_properties$SourceName)
  wide_sources <- source_properies$SourceName[source_properties$MultipleObservations == 0]
  
  # output combined wide if requested
  if (combine_wide && nrow(wide_sources) > 0) {
    
    wide_vars <- var_source$VariableName[var_source$SourceName %in% wide_sources]
    
    make_data_package(varlist = wide_vars, level = level,
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
  if (nrow(split_sources) > 0) {
    
    
  }
  
  # make data package
  dat <- get_bibloadr_data(varfile = varfile, varlist = varlist, level = level,
                           allow_null_ids = allow_null_ids, allow_hidden = allow_hidden,
                           log = log, testmode = testmode, cohort = cohort, devmode = devmode)
  
  about <- paste0(package_name, " | ", database_version)
  
  if(format == "stata") save_bibloadr_dta(dat = dat, file = paste0(package_directory, "/", package_file_stem, "_Data.dta"), 
                                          about = about, version = stata_version)
  
  
}
