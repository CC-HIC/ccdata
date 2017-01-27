#' @import XML
#' @import data.table
#' @import yaml
#' @import Rcpp
NULL


.onLoad <- function(libname = find.package("ccdata"), pkgname = "ccdata") {
    path <- find.package("ccdata")
    env <- parent.env(environment())
    
    # Assign ITEM_REF tables
    utils::data("ITEM_REFTABLE", package="ccdata", envir=env)
    ITEM_REF <- yaml.load_file(paste(path, "data", "ITEM_REF.yaml", sep=.Platform$file.sep))
    assign("ITEM_REF", ITEM_REF, envir=env)
   
    # Build up short name / NIHR code / Classification conversion dictionary
    reverse.name.value <- function(vec) {
        new <- names(vec)
        names(new) <- vec
        return(new)
    }

    stname <- sapply(ITEM_REF, function(x) x$shortName)
    meta <- paste0(stname, ".meta")
    names(meta) <- paste0(names(stname), ".meta")
    code2stname.dict <- c(stname, meta)
    stname2code.dict <- reverse.name.value(code2stname.dict)


    longnames <- sapply(ITEM_REF, function(x) x$dataItem)
    stname2longname.dict <- longnames
    names(stname2longname.dict) <- stname

    #' classification dictionary: demographic, nurse, physiology, laboratory, drugs
    class.dict_code <-  sapply(ITEM_REF, function(x) x$Classification1)
    class.dict_stname <- class.dict_code
    names(class.dict_stname) <- as.character(code2stname.dict[class.dict_stname])

    #' Time variable dictionary
    tval.dict_code <- data.checklist$NHICdtCode != "NULL" 
    tval.dict_stname <- tval.dict_code
    names(tval.dict_stname) <- as.character(data.checklist$NHICcode)
    names(tval.dict_code) <- as.character(data.checklist$NHICcode)

    assign("ITEM_REF"         , ITEM_REF         , envir=env) 
    assign("code2stname.dict" , code2stname.dict , envir=env) 
    assign("stname2code.dict" , stname2code.dict , envir=env) 
    assign("stname2longname.dict" , stname2longname.dict , envir=env) 
    assign("class.dict_code"  , class.dict_code  , envir=env)
    assign("class.dict_stname", class.dict_stname, envir=env) 
    assign("tval.dict_code"   , tval.dict_code   , envir=env) 
    assign("tval.dict_stname" , tval.dict_stname , envir=env) 

    assign('checklist', extractIndexTable(), envir=env)
}
