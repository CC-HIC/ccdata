#' Check if the values of a vector v is in the given ranges.
#' @param v vector numeric
#' @param range A string contains the numeric ranges in a form such as (low,
#' up) for open range and [low, up] for close range. Multiple
#' ranges should be separated by semi-columns which is equivalent to logical
#' OR. e.g. (low1, up1); (low2, up2)
inrange <- function(v, range) {
    funtxt <- function(r) {
        spr <- unlist(strsplit(r, ";"))
        number <- regmatches(spr, gregexpr("[-+]?[0-9]*\\.?[0-9]+", spr))
        operation <- regmatches(spr, gregexpr("\\[|\\]|\\(|\\)", spr))
        
        if (any(is.na(as.numeric(unlist(number)))))
            stop("Cannot find the proper numeric range expression.")

        stopifnot(length(number) == length(operation))
        total.op <- vector()

        for (i in seq(operation)) {
            op <- operation[[i]]
            op[op == "("] <- "x >"
            op[op == "["] <- "x >="
            op[op == ")"] <- "x <"
            op[op == "]"] <- "x <="
            if (as.numeric(number[[i]][1]) > as.numeric(number[[i]][2]))
                stop("the lower bound value (", number[[i]][1], ') is bigger than the upper bound value (', number[[i]][2], ")")
            total.op[i] <- paste("(", paste(op, number[[i]], collapse=" & "), ")")
        }
        total.op <- paste(total.op, collapse=" | ")
        return(paste("function(x) ", total.op))
    }
    cmpfunc <- eval(parse(text=funtxt(range)))
    return(cmpfunc(v))
}

#' @include ccTable.R
ccTable$methods(
    get.ranges = function() {
        # Initialise with temp column to make sure that dquality has the same
        # number of rows than torigin 
        .self$dquality$range <- .self$torigin[,c('site', 'episode_id'), with=F]
        rgnum <- list('red'=1, 'amber'=2, 'green'=3)
        for(item_name in names(.self$conf)) {
            item <- .self$conf[[item_name]]

            if (!is.null(item$range$labels)){
                rgclass <- rep(NA, nrow(.self$torigin))
                rgclass[.self$torigin[[item_name]] != "NA"] <- 0
                                
                for(rg_label in names(item$range$labels)) {
                    irg <- item$range$labels[[rg_label]]
                    rgclass[which(inrange(.self$torigin[[item_name]], irg))] <- 
                        rgnum[[rg_label]]
                }
                .self$dquality$range[[item_name]] <- rgclass
            }
        }
    }
)

ccTable$methods(
    filter.ranges = function(select='red') {
        rgnum <- list('red'=1, 'amber'=2, 'green'=3)
        inselectrange <- function(x, ...) {
            x >= rgnum[[select]]
        }

        if(is.null(.self$dquality$range) || 
           nrow(.self$dquality$range) != nrow(.self$tclean))
            .self$get.ranges()

        .self$dfilter$range <- getfilter(.self$dquality$range, inselectrange)
    }
)
