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
#' @param xml.path the path of the folder of which contains the XML files. 
#' @param mc.cores number of processors to be applied for parallelisation.
#' @param quiet logical switch on/off of the progress bar. 
#' @return ccRecord object
parse.new.xml <- function(xml.path, mc.cores=4, quiet=FALSE) {
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
#' @param xml.path character the path of the folder of which contains the XML files. 
#' @param mc.cores integer number of processors to be applied for parallelisation. 
#' @param restart logical purge the previous database and restart parsing for all the XML files presented. 
#' @param splitxml logical break down the XML files into chuncks. (Do it when the file is too big)
#' @param quiet logical show the progress bar if true
#' @export update_database 
update_database <- function(xml.path, restart=FALSE, splitxml=FALSE, 
                            mc.cores=4, quiet=FALSE, dt=TRUE) {
    if (restart)
        unlink('.database')
    if (splitxml) {
        break.down.xml(xml.path)
        xml.path2 <- paste(xml.path, ".partxml", sep="/")
    }
    else 
        xml.path2 <- xml.path

    alldata.loc <- paste(xml.path, "alldata.RData", paste="/")
    parse.new.xml(xml.path2, mc.cores, quiet)


    alldata <- ccRecord()
    files <- dir(paste(xml.path, ".database", sep="/"), 
                 pattern="[^alldata.RData]", 
                 full.names=TRUE)
    
    for (i in files) {
        load(i)
        alldata <- alldata + db
    }

    if (dt) {
        ccdt <- deltaTime(alldata)
        save(list=c("alldata", "ccdt"), file=paste(xml.path, ".database", "alldata.RData", sep="/"))
    }
    else 
        save(alldata, file=paste(xml.path, ".database", "alldata.RData", sep="/"))
    
    invisible(alldata)
}

parse.big.xml <- function(xml.path, mc.cores=4, quiet=TRUE, tmpd="/tmp", maxsize=3) {

    fparse<- paste(xml.path, find.new.xml.file(xml.path), sep="/")
    fbig <- fparse[file.info(fparse)$size/1e9 > maxsize]
    fbig_nopath <- sapply(strsplit(fbig, "/"), function(x) x[length(x)])

    if (length(fbig > 0)) {
        cat("Detected big files ... \n")

        tmpd <- paste(tmpd, "ccd_big_files", sep="/")

        if (dir.exists(tmpd)) unlink(tmpd, recursive=T)
        dir.create(tmpd)
#        file.copy(fbig, tmpd)
#        stopifnot(all(fbig_nopath == dir(tmpd)))
#        update_database(tmpd, mc.cores=mc.cores, quiet=quiet)

        cmd <- system.file("pipeline/break_into.sh", package="ccdata")
        print(cmd)
        for (i in fbig) system2(cmd, c(i, 3))
    
        for(i in fbig_nopath) cat(" [+] ", i, "\n", sep="")
    }

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
        partxml.file <- list.files(xml.path, pattern=".partxml", full.names=T)
        file.copy(partxml.file, partxml.dir) 
        file.remove(partxml.file)
    }
    return(1)
}
