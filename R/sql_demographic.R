#' Create demographic SQL tables. The data type of each column is in its
#' corresponding data type.
#' @export sql.demographic.table
sql.demographic.table <- function(record, dtype=TRUE) {
    env <- environment()
    demogls <- list()
    all.demcode <- all.nhic.code("Demographic")
    for_each_episode(record, 
        function(x){
            demog.data <- rep("NULL", length(all.demcode))
            names(demog.data) <- all.demcode
            demog.data <- as.list(demog.data)
            for(item in names(x@data)) {
                if (length(x@data[[item]]) == 1) {
                    demog.data[[item]] <- x@data[[item]]
                }
            }
            env$demogls[[length(env$demogls) + 1]] <- .simple.data.frame(demog.data)
        })
    demogt <- rbindlist(demogls, fill=T)
    setnames(demogt, code2stname(names(demogt)))

    if (dtype) {
        for (i in seq(ncol(demogt))){
            demogt[[i]] <- 
                .which.datatype(stname2code(names(demogt)[i]))(demogt[[i]])
        }
    }
    demogt[, index:=seq(nrow(demogt))]
    return(demogt)
}
