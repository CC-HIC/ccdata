#' @include ccTable.R

interpolateVec <- function(v, lead, lag, FUN="mean", ...) {
    v <- suppressWarnings(as.numeric(as.character(v)))
    na.ind <- which(is.na(v))
    if (length(na.ind) > 0) { # do interpolation if NA is found. 
        v2 <- c(rep(NA, lead), v, rep(NA, lag))
        n_x <- sapply(na.ind, 
                      function(i) {
                          do.call(FUN,
                                  c(list(x=v2[i + lead + seq(-lead, lag)]),
                                    as.list(substitute(list(...)))[-1L]))
                      })
        v[na.ind] <- n_x
    }
    v
}


ccTable$methods(
    imputation = function() {
    "Filling missing data to a time series data by performing a given imputation
    method on a selected window period nearby the missing data."
        imputation_columns <- function(sd) {
            for (i in names(.self$conf)) {
                imwin <- .self$conf[[i]][['missingness']][['impute_2d']]
                if (!is.null(imwin)) {
                    fun <- imwin[['fun']]
                    lead <- imwin[['lead']]
                    lag <- imwin[['lag']]
                    sd[[i]] <- interpolateVec(v=sd[[i]], lead=lead, lag=lag, FUN=fun, na.rm=T)
                }
            }
            return(sd)
        }
        .self$tclean <- .self$tclean[, imputation_columns(.SD), by=c("episode_id", "site")]
})
