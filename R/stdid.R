#' S4 class to hold standard IDs such as "NIHR_HIC_ICU_0001"
#' @slot ids single or multiple characters
#' @import methods
StdId <- setClass ("StdId",
                   slots = c(ids="vector"), 
                   validity=function(object)
                   {
                       reg <- regexpr("NIHR_HIC_ICU_[0-9]+", object@ids)
                       ids <- regmatches(object@ids, reg)
                       if (length(object@ids) != length(ids)) 
                           return("initialisation failure, as the standard ID pattern cannot be found.")
                       else
                           object@ids = ids
                       return(TRUE)
                   })


#' constructor of StdId class
#' @export StdId
StdId <- function(text) {
    reg <- regexpr("NIHR_HIC_ICU_[0-9]+", text)
    ids <- regmatches(text, reg)
    if (length(text) != length(ids) | length(text) == 0) 
        stop("StdId: initialisation failure, as the standard ID pattern cannot be found.")
    else
        return(methods::new("StdId", ids=ids))
}


#' convert standard ids to numbers (character) which can be used for indexing.
#' @export as.number
as.number <- function(obj) {
    d <- as.character(obj@ids)
    if(!all(grepl("NIHR_HIC_ICU_", obj@ids)))# input is code alredy
        return(obj@ids)
    no.prefix <- 
        unlist(strsplit(obj@ids, "NIHR_HIC_ICU_"))[seq(2, length(obj@ids) * 2, 2)]
    return(as.character(no.prefix))
}

#'
#' @export .as.number
.as.number <- function(code) {
    return(as.numeric(strsplit(code, "NIHR_HIC_ICU_")[[1]][2]))
}


all.nhic.code <- function(cls) {
        data.checklist[data.checklist$Classification1 == cls,"NHICcode"]
}
#' convert NHIC codes to the short names
#' @export code2stname
code2stname <- function(code) {
    code <- as.character(code)
    stn <- ccdata:::code2stname.dict[code]
    stn[is.na(stn)] <- code[is.na(stn)]
    return(stn)
}

#' convert short names to NHIC code
#' @export stname2code
stname2code <- function(stname) {
    stname <- as.character(stname)
    code <- ccdata:::stname2code.dict[stname]
    code[is.na(code)] <- stname[is.na(code)]
    return(code)
}

#' @export 
stname2longname <- function(stname) {
    stname <- as.character(stname)
    code <- ccdata:::stname2longname.dict[stname]
    code[is.na(code)] <- stname[is.na(code)]
    return(code)
}
#' convert short names to long names
#' @export short2longname
short2longname <- function(stname) {
    stname <- as.character(stname)
    longname <- array("NULL", length(stname))
    for (i in seq_along(stname))
        longname[i] <- ccdata:::ITEM_REF[[stname2code(stname[i])]]$dataItem
    return(longname)
}


#' Identify the classification - classification1 of a given item code or short
#' name. Classification1 has 5 labels: 
#' [1] "Demographic", [2] "Physiology" 
#' [3] "Drugs" [4] "Nursing_other" [5] "Laboratory"
#' @param item_name NHIC code or the short name
#' @return character the item classification
#' @export which.classification
which.classification <- function(item_name) {
    cls <- ccdata:::class.dict_code[item_name]
    if (is.na(cls))
        cls <- ccdata:::class.dict_stname[item_name]
    if (is.na(cls)) stop(paste("item name", item_name, "cannot be found."))
    return(cls)
}


#' Check if the item NHIC code or short name belongs to the demographic
#' category.
#' @param item_name character the NHIC code or the short name
#' @return logical
#' @export is.demographic
is.demographic <- function(item_name) {
    return(which.classification(item_name) == "Demographic")
}


#' Check if the item NHIC code or short name belongs to the physiology
#' category.
#' @param item_name character the NHIC code or the short name
#' @return logical
#' @export is.physiology
is.physiology <- function(item_name) {
    return(which.classification(item_name) == "Physiology")
}


#' Check if the item NHIC code or short name belongs to the drugs 
#' category.
#' @param item_name character the NHIC code or the short name
#' @return logical
#' @export is.drugs
is.drugs <- function(item_name) {
    return(which.classification(item_name) == "Drugs")
}


#' Check if the item NHIC code or short name belongs to the Laboratory 
#' category.
#' @param item_name character the NHIC code or the short name
#' @return logical
#' @export is.laboratory
is.laboratory <- function(item_name) {
    return(which.classification(item_name) == "Laboratory")
}
