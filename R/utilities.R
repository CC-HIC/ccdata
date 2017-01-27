#' This is a simplified version of as.data.frame() with better performance. 
#' @export .simple.data.frame
.simple.data.frame <- function(x) {
    nm <- names(x)
    attr(x, "row.names") <- .set_row_names(length(x[[1]]))
    attr(x, "col.names") <- nm
    class(x) <- "data.frame"
    x
}



#' get indexing tables for time label, time-wise value, meta data label, and
#' meta data.  
#' @return list of vectors contains time.index, datat.index, meta.index,
#'          datam.index
extractIndexTable <- function() {
    info <- extractInfo()

    checklist <- list()
    for(i in seq(info$nontime))
        checklist[[info$nontime[i]]] <- "item1d"
    for(i in seq(info$time$idt))
        checklist[[info$time$idt[i]]] <-"time"
    for(i in seq(info$time$id))
        checklist[[info$time$id[i]]] <- "item2d"
    for(i in seq(info$meta$meta))
        checklist[[info$meta$meta[i]]] <- "meta"
    
    return(checklist)
}

#'
#' @export .which.type
.which.type <- function(id) {
    return(ccdata:::checklist[[id]])
}

#'
#' @export .which.datatype
.which.datatype <- function(id) {
  # List with the conversion operations to do for each data type
  operations <- list('numeric' = as.numeric,
                     'text' = as.character,
                     'date' = as.character, # They are hashed for now
                     'time' = as.character, # They are hashed for now
                     'logical' = as.logical,
                     'list' = as.character,
                     'date/time' = as.character, # They are hashed for now
                     'list / logical' = as.character) # what are they?

  datatype = ccdata:::ITEM_REF[[id]]$Datatype
  if (!is.null(datatype)){
    if (exists(datatype, operations)){
      return(operations[[datatype]])
    }
  }
  # accounts for not listed or null (eg. when working with dt labels)
  return(as.character)
}

#' give id number from NHIC code like "NIHR_HIC_ICU_xxxx"
#' @param nhic NHIC code
whichIsCode <- function(nhic) {
    return(grepl(nhic, pattern="[0-9][0-9][0-9][0-9]"))
}

#' extract information from data.checklist
#' @return list of time [data.frame(id, idt)], meta [data.frame(id, idmeta)], 
#'         nontime [numeric], MAX_NUM_NHIC
#' @export extractInfo
extractInfo <- function() {
    if(!exists("data.checklist"))
        data("data.checklist")
    index.time <- whichIsCode(data.checklist$NHICdtCode) 
    index.meta <- whichIsCode(data.checklist$NHICmetaCode)

    item.labels <- StdId(data.checklist$NHICcode[index.time])
    time.labels <- StdId(data.checklist$NHICdtCode[index.time])

    metaitem.labels <- StdId(data.checklist$NHICcode[index.meta])
    meta.labels <- StdId(data.checklist$NHICmetaCode[index.meta])
    
    time.list <-
        data.frame(id=item.labels@ids, idt=time.labels@ids,
                   stringsAsFactors=FALSE)
    meta.list <- data.frame(id=metaitem.labels@ids, meta=meta.labels@ids,
                            stringsAsFactors=FALSE)
    
    
    nontime<- StdId(data.checklist$NHICcode[!index.time])
    # get all ids which should be the assemble of NHICcode and NHICmetaCode
    all.nhic.code <- StdId(data.checklist$NHICcode)
    all.ids <- c(meta.list$idmeta,
                 all.nhic.code@ids)
    if (any(duplicated(all.ids)))
        stop("data.checklist.RData error! meta data code and NHICcode are overlaped")
    return(list(time=time.list, meta=meta.list, nontime=nontime@ids,
                MAX_NUM_NHIC=max(as.numeric(as.number(all.nhic.code)), 
                                 as.numeric(as.number(StdId(time.list$idt))))))
}

#' retrieve information of the query code/item names from data.checklist
#' @param item.code it can be either item name or NHIC_code, dt_code, or
#'        meta_code
#' @return a vector contains NHIC_code, dt_code, meta_code and row_in_checklist
#' @export getItemInfo
getItemInfo <- function(item.code) {
    if (!exists("data.checklist"))
        data("data.checklist")

    if(grepl("NIHR_HIC_ICU_", item.code)){# input is code
        item <- data.checklist$NHICcode == item.code
        dt <- data.checklist$NHICdtCode == item.code
        meta <- data.checklist$NHICmetaCode == item.code
        row.in.list <- which(item | dt | meta)
    }
    else{ # input is item name
        row.in.list <- which(data.checklist$dataItem==item.code)
    }

    if (length(row.in.list) != 1){
        stop("item/NHIC code cannot be found in the list.\n")
    }

    item.info <- c(as.character(data.checklist$dataItem[row.in.list]),
                   as.character(data.checklist$NHICcode[row.in.list]),
                   as.character(data.checklist$NHICdtCode[row.in.list]),
                   as.character(data.checklist$NHICmetaCode[row.in.list]),
                   as.character(data.checklist$Units[row.in.list]),
                   as.character(row.in.list))

    names(item.info) <- c("item", "NHIC_code", "dt_code", 
                          "meta_code", "unit", "row_in_checklist")
    return(item.info)
}

#' get information of a group of code of items and return an array.
getItemsInfo <- function(items.code, var) {
    info_ <- array(NA, length(items.code))
    for (i in seq(items.code))
        info_[i] <- getItemInfo(items.code[i])[var]
    return(info_)
}

#' Convert time from xml to POSIX format.
#'
#' Convert the XML time The XML time format to POSIXct. 
#' @param xml.time character. Time in XML format such as 2014-02-01T03:00:00
#' @param allow logical. Wrong format will be accepted when \code{allow} is set
#' to be TRUE and NA will be the return value, otherwise return error. 
#' It is useful while dealing with pseudonymous data where the time format is
#' not presented correctly. 
#' 
#' @export
xmlTime2POSIX <- function(xml.time, allow=FALSE){
    if (is.null(xml.time))
        return(NA)
    tp <- as.POSIXct(xml.time, "GMT", format="%Y-%m-%dT%H:%M:%S")
    tp[is.na(tp)] <- as.POSIXct(xml.time[is.na(tp)], "GMT", format="%Y-%m-%d")
    if (!allow) {
        if(any(is.na(tp)))
            stop(xml.time[is.na(tp)])
    }
    return(tp)
}


#' @export site.info
site.info <- function(){
    si <- list(
               "D20N"=c("Addenbrooke's Hospital", "Neuro", "Cambridge", ""),  
               "D20"=c("Addenbrooke's Hospital", "ICU/HDU", "Cambridge", "John Farnham"), 
               "K32"=c("Guy's Hospital", "ICU", "GSTT", ""),   
               "V47"=c("St Thomas' Hospital", "ICU/HDU", "GSTT", ""),   
               "H09"=c("St Thomas' Hospital", "OIR", "GSTT", "overnight intensive recovery"), 
               "Z89"=c("St Thomas' Hosptial", "HDU", "GSTT", "page and victoria"), 
               "C90"=c("Hammersmith Hospital", "ICU/HDU", "Imperial", ""),  
               "Q13"=c("St Mary's Hospital London", "ICU", "Imperial", "Milne Ward"),
               "S28"=c("John Radcliffe", "ICU", "Oxford", ""),
               "F42"=c("Oxford Neuro", "", "Oxford", "no data"),
               "F54"=c("Oxford Horton", "", "Oxford", "no data because not on ICIP"),
               "Q70"=c("University College Hospital", "ICU/HDU", "UCLH", ""),
               "Q70W"=c("Westmoreland Street", "ICU/HDU", "UCLH", ""),   
               "R42"=c("Unknown", "Unknown", "Unknown", "Unknown"),
               "X90"=c("Addenbrooke's Hospital", "General/Liver/Transplant",  "Cambridge", "John Farnham")
               )
    si <- data.frame(t(.simple.data.frame(si)), stringsAsFactors=F)
    names(si) <- c("Hospital", "Unit", "Trust", "Comments")
    return(si)
}
