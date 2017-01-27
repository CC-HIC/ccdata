#' @import parallel
NULL

find.new.xml.file <- function(xml.path) {
    rdata.path <- paste(xml.path, ".database", sep="/")
    dir.create(rdata.path, showWarnings=F)

    xml.file.name <- dir(xml.path)

    if (! all(grepl("(.xml|.XML|.partxml)$", xml.file.name))) 
        stop("file names in xml.path ", xml.path, " must end with suffix .xml or .partxml. ") 

    parsed.pattern <- unique(sapply(strsplit(dir(rdata.path), ".xml"), function(x) x[1]))
    if (length(parsed.pattern) == 0) {
        return(xml.file.name)
    } else {
        all.xml.files <- 
            xml.file.name[!grepl(paste(parsed.pattern, collapse="|"), xml.file.name)]
        return(all.xml.files)
    }
}


#' Update the RData database
#'
#' Inject episode data from the newly added XML files to the RData database. 
#' @param xml.path
#' @param mc.cores 
#' @return ccRecord object
update.new.xml <- function(xml.path, mc.cores=4, quiet=FALSE) {
    files.to.parse <- find.new.xml.file(xml.path)

    db.collection <- mclapply(files.to.parse, 
                              function(x) {
                                  fxml <- paste(xml.path, x, sep="/")
                                  frdata <- paste(xml.path, "/.database/", x, ".RData", sep="")
                                  db <- xml2Data(fxml, quiet=quiet)
                                  save(db, file=frdata)
                                  return(db)
                              }, mc.cores=mc.cores)

    #' combine new data to a ccRecord 
    new.db <- ccRecord()
    for (i in seq(db.collection)) {
        new.db <- new.db + db.collection[[i]]
    }
    return(new.db)
}


#' Update the critical care database (RData) 
#' 
#' Parse critical care data from XML files and inject them into the RData
#' database. 
#' 
#' @export update.database 
update.database <- function(xml.path, restart=FALSE, splitxml=FALSE, 
                            mc.cores=4, quiet=FALSE) {
    if (restart)
        unlink('.database')
    if (splitxml) {
        break.down.xml(xml.path)
        xml.path2 <- paste(xml.path, ".partxml", sep="/")
    }
    else 
        xml.path2 <- xml.path

    alldata.loc <- paste(xml.path, "alldata.RData", paste="/")
    update.new.xml(xml.path2, mc.cores, quiet)


    alldata <- ccRecord()
    files <- dir(paste(xml.path, ".database", sep="/"), 
                 pattern="[^alldata.RData]", 
                 full.name=TRUE)
    
    for (i in files) {
        load(i)
        alldata <- alldata + db
    }

    save(alldata, file=paste(xml.path, ".database", "alldata.RData", sep="/"))
    
    invisible(alldata)
}


break.down.xml <- function(xml.path) {
    unlink(paste(xml.path, ".partxml", sep="/"), recursive=T)
    partxml.dir <- paste(xml.path, ".partxml", sep="/")
    dir.create(partxml.dir)
    cmd <- paste(find.package('ccdata'), "pipeline/break_into.sh", sep="/")
    # in the case of using testings, the original package layout is slightly
    # different from the compiled one. 
    if (! file.exists(cmd))         
        cmd <- paste(find.package('ccdata'), "inst/pipeline/break_into.sh", sep="/")
    
    newfile <- find.new.xml.file(xml.path)
    if (length(newfile) > 0)
        newfile <- paste(xml.path, newfile, sep="/")
    else{
        return(1)
    } 

    for (f in newfile) {
        system2(cmd, c(f, 100))
        partxml.file <- list.files(xml.path, pattern=".partxml", full.name=T)
        file.copy(partxml.file, partxml.dir) 
        file.remove(partxml.file)
    }
    return(1)
}
