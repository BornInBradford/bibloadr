
dataRequestXML <- function(testmode = F, namelist = character(0), nametype = "variable",
                           level = character(0), cbtype = character(0), subclist = character(0),
                           AllowNullIds = F, log = F) {
    
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
    el_AN <- ifelse(AllowNullIds, paste0("<AllowNullIds>On</AllowNullIds>",eol), "")
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