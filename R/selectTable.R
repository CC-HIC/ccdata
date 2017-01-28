#' Create wide table from ccRecord
#' 
#' @param record ccRecord
#' @param items_opt character vectors. Items (HIC code) selected in item_opt are optional items, which will be automatically 
#' filled when item is missing. 
#' @param items_obg obligatory items that is obligatory; Any episode that doesn't contain
#' item in this vector will be removed.
#' @param freq numeric cadence in hour. 
#' @param return_list logical if TRUE return as a list.  
#' @return data.table
#' @export selectTable
selectTable <- function(record, items_opt=NULL, items_obg=NULL, freq,
                        return_list=FALSE) {
    all_items <- c(items_opt, items_obg)
    if (is.null(all_items))
        stop('both items_opt and items_obg are NULL')

    env <- environment()
    lt <- list()
    for_each_episode(record,
                     function(ep) {
                         if (all(items_obg %in% names(ep@data))) {
                             result <- list()
                             period_length <- getEpisodePeriod(ep)
                             # getEpisodePeriod will return NULL when no 2D
                             # data been found. 
                             if (!is.null(period_length)) {
                                 if (period_length > 0 ) {
                                     result <- append(result,
                                                      itemsToDataFrame(ep, all_items,
                                                                       period_length,
                                                                       freq))
                                     nlength <- length(result[["time"]])
                                     result[["site"]] <- rep(ep@site_id, nlength)
                                     result[["episode_id"]] <- rep(ep@episode_id, nlength)
                                     env$lt[[length(lt) + 1]]<- .simple.data.frame(result)
                                 }
                             }
                         }
                     })
    if (return_list)
        return(lt)

    # fill is true because meta data column can be missing. 
    dt <- rbindlist(lt, fill=TRUE) 

   
    # Adding missing meta columns to keep the 2d wide consistent. 
    code.has.meta <- names(unlist(sapply(ITEM_REF, function(x) x$NHICmetaCode)))
    for (i in all_items) {
        meta.code <- paste(i, "meta", sep=".")
        if (i %in% code.has.meta & !(meta.code %in% names(dt))) {
            dt[[meta.code]] <- rep("NA", nrow(dt))
        }
    }

    # convert data type 
    for (i in all_items)
        dt[[i]] <- suppressWarnings(.which.datatype(i)(as.character(dt[[i]])))


    return(dt)
}

itemsToDataFrame <- function(ep, items, period_length, freq) {
    listmatrix <- list()
    time <- seq(0, period_length, freq)

    listmatrix[["time"]] <- time

    for (i in items) {
        if (length(ep@data[[i]]) == 1) {
            listmatrix[[i]] <- rep(ep@data[[i]], length(time))
        }
        else {
            if ("time" %in% names(ep@data[[i]])) {
                new <- reallocateTime(ep@data[[i]], period_length, freq)
                listmatrix[[i]] <- new$val
                if ("meta" %in% names(ep@data[[i]]))
                    listmatrix[[paste(i, "meta", sep=".")]] <- new$meta
            }
            else
                listmatrix[[i]] <- rep("NA", length(time))
        }
    }
    return(listmatrix)
}
