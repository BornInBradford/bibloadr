
# Generate XML for data request, to be submitted to database function
# returns character containing XML string
bibloadr_request_xml <- function(namelist = character(0), nametype = "variable",
                             level = character(0), cbtype = character(0), subclist = character(0),
                             allow_null_ids = F, log = F, testmode = F) {
    
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
    
    # optional xml elements
    el_CB <- ifelse(length(cbtype)>0, paste0("<CodeBook>",cbtype,"</CodeBook>",eol), "")
    el_ML <- ifelse(length(level)>0, paste0("<MeasurementLevel>",level,"</MeasurementLevel>",eol), "")
    el_AN <- ifelse(allow_null_ids, paste0("<AllowNullIds>On</AllowNullIds>",eol), "")
    if (log) {
        el_LG <- paste0("<LogRequest>On</LogRequest>",eol)
        el_UN <- paste0("<User>","BiBUser","</User>",eol)
    } else {
        el_LG <- ""
        el_UN <- ""
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
    xmlv <- paste0(el_DR[1], eol, el_CB, el_ML, el_AN, el_LG, el_UN, el_TM, el_SC, el_Nlist[1], eol)
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
open_bibloadr_db <- function(devmode = F) {
  
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
bibloadr_query <- function(query_string, devmode = F) {
  
  # do database bit to get data
  db <- open_bibloadr_db(devmode)
  
  dat <- sqlQuery(db, query_string, stringsAsFactors = F)
  
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

# label a dataframe using meta-data provided in var_labels and val_labels
label_data <- function (dat, var_labels, val_labels) {
  
  return(dat)
  
}

# takes data request parameters and submits to bibloadr db, returning data frame
# concatenates variables in varlist character vector to variables in varfile
get_bibloadr_data <- function(varfile = character(0), varlist = character(0), level = character(0),
                              allow_null_ids = F, label = T, log = F, testmode = F, devmode = F) {
  
  # concatenate varlist vars to varfile vars
  varlist <- make_namelist(varfile, varlist)
  
  # if we still don't have any, exit with error
  if(length(varlist) == 0) stop("No variables found in request.")
  
  # SQL string building
  sql_start <- "EXEC [ResearchMeta].[Explorer].[GetVariableData]\n@DataRequest = N'"
  
  sql_xml <- bibloadr_request_xml(namelist = varlist, level = level, allow_null_ids = allow_null_ids, 
                                  log = log, testmode = testmode)
  
  sql_end <- "';\n"
  
  query_string <- paste0(sql_start, sql_xml, sql_end)
  
  dat <- bibloadr_query(query_string, devmode)
  
  # if we need to label variables, do that now
  if (label) {
    var_labels <- get_bibloadr_meta (varfile = varfile, varlist = varlist, type = "varlong",
                                     testmode = testmode, devmode = devmode)
    val_labels <- get_bibloadr_meta (varfile = varfile, varlist = varlist, type = "code",
                                     testmode = testmode, devmode = devmode)
    dat <- label_data (dat, var_labels, val_labels)
  }
  
  # return data frame
  return(dat)
  
}


# takes metadata request parameters and submits to bibloadr db, returning data frame
# concatenates variables in varlist character vector to variables in varfile
# default type is varlong
get_bibloadr_meta <- function(varfile = character(0), varlist = character(0),
                              type = "varlong", testmode = F, devmode = F) {
  
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
