#' Create the data quality report 
#' Create a detailed data quality report, including file summary, site 
#' summary, data completeness, and density plot. The result can be found 
#' in {work_dir}/report/data_quality_report.{pdf}/{md}. Using this function, 
#' one can also create a site/trust specified report, see the argument "site". 
#' You need to make sure that you have the right to write into the {work_dir}. 
#' 
#' @param ccd ccRecord 
#' @param site a vector of the site ids for the site specified report. 
#' @param pdf logical create the pdf version of the DQ report, 
#' otherwise stay in markdown format
#' @export data.quality.report
#' @import knitr
#' @import pander
#' @import ggplot2
data.quality.report <- function(ccd, site=NULL, pdf=T) {
    if (is.null(site)) {
        dbfull <<- "YES"
    }
    else {
        dbfull <<- "NO"
        ccd <- ccd[site]
    }
 
    if (dir.exists("report")) {
        unlink("report", recursive=T)
    }

    wd <- getwd()
    rptpath <- paste(path.package('ccdata'), "report", sep="/")
    file.copy(rptpath, ".", recursive=T)

    write.report <- function() {
        setwd('report')
        dqpath <- "data_quality_report.Rmd"
        headerpath <- "listings-setup.tex"
        tpltpath <- "report.latex"

        knit(dqpath, "data_quality_report.md")
        if (pdf) {
            pandoc.cmd <- 
                paste("pandoc -s -N --toc --listings -H ", headerpath,
                      " --template=", tpltpath, 
                      " -V --number-section  -V papersize:a4paper -V geometry:margin=1.3in ", 
                      "data_quality_report.md -o data_quality_report.pdf", sep="")
            tryCatch(system(pandoc.cmd), 
                     error = function(e) {
                         cat(e)
                         setwd(wd)
                     }, 
                     finally = {
                         setwd(wd)
                     })
            setwd(wd)
        }
    }

    tryCatch(write.report(), finally={setwd(wd)})

}


#' @export file.summary
file.summary <- function(ccd) {
    infotb <- ccd@infotb
    file.summary <- infotb[, list("Number of Episode"=.N, 
                                  "Upload time"=max(parse_time), 
                                  "Sites"=paste(unique(site_id), collapse=", ")), by=parse_file]
    file.summary[, "File":=parse_file]
    file.summary[, parse_file:=NULL]
    return(file.summary)
}

#' @export xml.site.duration.plot
xml.site.duration.plot <- function(ccd) {
    tb <- copy(ccd@infotb)
    tb <- tb[, list(minadm=min(t_admission, na.rm=T), 
              maxadm=max(t_admission, na.rm=T),
              mindis=min(t_discharge, na.rm=T),
              maxdis=max(t_discharge, na.rm=T)), by=site_id]
    site_name <- apply((site.info()[tb$site_id, ][,1:2]), 1, 
          function(x) paste(x, collapse="-"))
    tb[, site_name:=site_name]
    
    ggplot(tb, aes(x=minadm, y=site_name)) +
        geom_segment(aes(xend=maxdis, yend=site_name), color="gray", size=10) +
        annotate("text", x=tb$minadm+(tb$maxdis-tb$minadm)/2, 
                 y=tb$site_name, label=tb$site_name, size=7) + 
        scale_x_datetime(date_breaks="3 month")+
        theme(axis.text.y=element_blank()) + 
        ggtitle("Site") +
        xlab("") + ylab("")
}


#' @export xml.file.duration.plot
xml.file.duration.plot <- function(ccd) {
    tb <- copy(ccd@infotb)
    tb <- tb[, list(minadm=min(t_admission, na.rm=T), 
              maxadm=max(t_admission, na.rm=T),
              mindis=min(t_discharge, na.rm=T),
              maxdis=max(t_discharge, na.rm=T)), by=parse_file]
    ggplot(tb, aes(x=minadm, y=parse_file)) +
        geom_segment(aes(xend=maxdis, yend=parse_file), color="gray", size=10) +
        annotate("text", x=tb$minadm+(tb$maxdis-tb$minadm)/2, 
                 y=tb$parse_file, label=tb$parse_file, size=7) + 
        scale_x_datetime(date_breaks="3 month")+
        theme(axis.text.y=element_blank()) + 
        ggtitle("The Duration of XML Files") +
        xlab("") + ylab("")
}



txt.color <- function(x, color) {
    x <- sprintf("%3.2f", x)
    paste("\\colorbox{", color, "}{", x, "}", sep="")
}


#' @export demographic.data.completeness
demographic.data.completeness <- function(demg, names=NULL, return.data=FALSE) {
    site.reject <- function(demg, name, ref) {
        if (ref == 0 | name == "ICNNO")
            return("")
       stb <- 
            demg[, 
                 round(length(which(!is.na(get(name)) & 
                                    get(name)!="NULL"))/.N * 100, 
                       digits=2), by="ICNNO"]
        rej <- stb[stb[[2]] < ref]
        if (nrow(rej) == 0)
            return("")
        else
            return(paste(apply(rej, 1, function(x) paste(x, collapse=":")),
                  collapse="; "))
    }

    path <- find.package("ccdata")
    acpt <- unlist(yaml.load_file(paste(path, "data", "accept_completeness.yaml", 
                                             sep=.Platform$file.sep)))

 
    demg <- copy(demg)
    demg[, index:=NULL]
    if (!is.null(names))
        demg <- demg[, names, with=F]

    cmplt <- apply(demg, 2, function(x) length(which(!(x=="NULL" | is.na(x)))))
    cmplt <- data.frame(cmplt)
    cmplt[, 1] <- round(cmplt[, 1]/nrow(demg)*100, digits=2)

    ref <- acpt[rownames(cmplt)]
    stopifnot(all(!is.na(ref)))
    vals <- cmplt[, 1]
    stname <- rownames(cmplt)
    
    reject <- array("", length(stname))
    for (i in seq(nrow(cmplt))) { 
        reject[i] <- site.reject(demg, stname[i], ref[i])
    }

    # color the text according the reference
    ind <- vals >= ref & ref != 0
    cmplt[, 1][ind] <- txt.color(vals[ind], "ccdgreen")
    ind <- vals < ref & ref != 0
    cmplt[, 1][ind] <- txt.color(vals[ind], "ccdred")

    

   
    rownames(cmplt) <- stname2longname(rownames(cmplt))
    cmplt$ref <- as.character(ref)
    cmplt$ref[cmplt$ref=="0"] <- ""
    cmplt$reject <- reject

    names(cmplt) <- c("Completeness %", "Accept Completeness %", "Rejected Sites (Site: %)")
    if (return.data)
        return(cmplt)
    pander(cmplt, style="rmarkdown", justify = c('left', 'center', "center",
                                                 "center"))
}

#' @export samplerate2d
samplerate2d <- function(cctb) {
    sample.rate.table <- data.frame(fix.empty.names=T)
    # items are the columns before site.  
    items <- names(cctb)[-c(grep("meta", names(cctb)), 
                            which(names(cctb) %in% 
                                  c("site", "time", "episode_id")))]
    for (i in items) {
        sr <- nrow(cctb)/length(which(is.na(cctb[[i]])))
        sample.rate.table <- 
            rbind(sample.rate.table, 
                  data.frame("item"=stname2longname(code2stname(i)), 
                             "sr"=sr))
    }
    rownames(sample.rate.table) <- NULL
    names(sample.rate.table) <- c("Item", "Sample Period (hour)")

    pander(sample.rate.table, style="rmarkdown")
}



#' 
#' @export total.data.point
total.data.point <- function(ccd) {
    dp.physio <- 
        sum(unlist(for_each_episode(ccd, 
                                    function(x) 
                                        Reduce(sum, sapply(x@data, nrow)))))
    dp.demg <-
        sum(unlist(for_each_episode(ccd, 
                                    function(x) 
                                        Reduce(sum, sapply(x@data, nrow)))))
    return(sum(dp.physio, dp.demg))
}

#' @export table1
table1 <- function(demg, names, return.data=FALSE) {
    panderOptions('knitr.auto.asis', FALSE)

    if (!return.data)
        cat(paste("\n## Table ONE\n"))
    table1.item <- function(demg, name) {
        ref <- ccdata:::ITEM_REF[[stname2code(name)]]
        if (is.null(ref))
            stop("The short name cannot be found in ITEM_REF.")
        if (!return.data)
            cat(paste("\n###", ref$dataItem, "\n"))
        if (ref$Datatype %in% c("text", "list", "Logical", "list / Logical")) {
            stopifnot(!is.null(ref$category))
            nmref <- sapply(ref$category$levels, function(x) x)
            r <- demg[, .N, by=name]
            level.name <- nmref[r[[name]]]
            r[, nm:=level.name]
            r[, percent:=N/nrow(demg)*100]

            tb <- data.table(
                              "Category"=r$nm,
                              "Episode Count"=r$N,  
                              "Percentage"=paste(round(r$percent, digits=1), "%"))
            setkey(tb, "Episode Count")

        }
        else stop(name, "is not a categorical variable.")
        if (return.data)
            return(tb)
        else 
            pander(tb, style="rmarkdown")
    }

    for (i in names)
        table1.item(demg, i)

    panderOptions('knitr.auto.asis', TRUE)
}


#' demg.distribution
#' Create a plot of the distribution of numerical demographic data.
#' 
#' @param demg ccRecord or demographic table created by sql.demographic.table()
#' @param names a vector of short names of numerical demographic data. 
#' @export demg.distribution
demg.distribution <- function(demg, names) {
    if (class(demg) == "ccRecord")
        demg <- sql.demographic.table(demg)
    for (nm in names) {
        ref <- ccdata:::ITEM_REF[[stname2code(nm)]]
        cat(paste("\n\n###", ref$dataItem, "\n"))
        gg <- ggplot(demg, aes_string(nm)) + geom_density(fill="lightsteelblue3") + 
            facet_wrap(~ICNNO, scales="free")
        print(gg)
        cat('\\newpage')
    }
}


#' @export physio.distribution
physio.distribution <- function(cctb, names) {
    for (nm in names) {
        ref <- ccdata:::ITEM_REF[[stname2code(nm)]]
        cat(paste("\n\n###", ref$dataItem, "\n"))
        gg <- ggplot(cctb, aes_string(ref$NHICcode)) + geom_density(fill="lightsteelblue3") + 
            facet_wrap(~site)
        print(gg)
        cat('\\newpage')
    }
}
