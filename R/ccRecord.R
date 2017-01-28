#' @import XML
#' @import data.table
#' @import yaml
#' @import Rcpp
NULL


#' The S3 class which holds all the CCHIC patient record - served as a database.
#'
#' @description  ccRecord is a class to hold the raw episode data parsed directly from XML or
#' CSV files.
#' @field nepisodes is an integer number indicates the total number of episode
#'       the record is holding.
#' @field dmgtb a data.table containing all the demographic information of each
#'       episode, including site_id, NHS number, PAS number, admission date/time,
#'       and discharge date/time. Call 
#' @field infotb a data.table holding the parsing information of each episode such as the
#'       parsing time and from which file it parsed from.
#' @field episdoes a list of ccEpisode objects. 
#' @exportClass ccRecord
#' @export ccRecord
#' @examples
#' heart_rate <- data.frame(seq(10), rep(70, 10)) # NIHR_HIC_ICU_0108
#' site_id <- "Q70" #  NIHR_HIC_ICU_0002
#' episode_id <- "0000001" # NIHR_HIC_ICU_0005
#'
#' # Create a new episode 
#' ep <- new.episode(list(NIHR_HIC_ICU_0108=heart_rate, 
#'                          NIHR_HIC_ICU_0002=site_id, 
#'                          NIHR_HIC_ICU_0005=episode_id)) 
#' 
#' # modifying records 
#' rec <- ccRecord() # a new record 
#' rec <- rec + ep # adding a new episode to the record
#' rec <- rec + NULL # adding nothing to the record
#' rec <- rec + rec # adding a record to a record
#' # Adding a list of episodes 
#' rec <- ccRecord()
#' ep1 <- new.episode()
#' ep2 <- new.episode()
#' eps.list <- list(ep1, ep2)
#' new.rec <- rec + eps.list
ccRecord <- setClass("ccRecord", 
                      slots=c(nepisodes="integer", dmgtb="data.table", 
                              infotb="data.table", episodes="list"),
                      prototype=prototype(nepisodes=as.integer(0), 
                                          infotb=data.table(), 
                                          dmgtb=data.table()))

#' The S3 class which holds data of a single episode. 
#' 
#' @field site_id character string. Site ID, if presented, otherwise "NA".
#' @field episode_id character string. Episode ID, if presented, otherwise "NA".
#' @field nhs_number character string. NHS number, if presented, otherwise "NA".
#' @field pas_number character string. PAS number, if presented, otherwise "NA".
#' @field parse_file character string. The source XML file. If the source is not a file then "NA".
#' @field t_admission POSIXct. Time of Admission to the ICU, if presented, otherwise NA.
#' @field t_discharge POSIXct. Time of discharge of the ICU, if presented, otherwise NA.
#' @field parse_time POSIXct. Parse time. 
#' @field data A list which holds all the data of this episode which is indexed by NIHIC code. 
#' @exportClass ccEpisode 
ccEpisode <- setClass("ccEpisode", 
                       slots=c(site_id="character", 
                               episode_id="character",
                               nhs_number="character",
                               pas_number="character",
                               t_admission="POSIXct", 
                               t_discharge="POSIXct",
                               parse_file="character",
                               parse_time="POSIXct", 
                               data="list"), 

                       prototype=prototype(site_id="NA", 
                                           episode_id="NA", 
                                           nhs_number="NA",
                                           pas_number="NA", 
                                           t_admission=as.POSIXct(NA), 
                                           t_discharge=as.POSIXct(NA), 
                                           parse_file="NA",
                                           parse_time=as.POSIXct(NA),
                                           data=list()))


#' Adding one ccEpisode object to ccRecord object.
#'
#' @param rec ccRecord-class
#' @param episode ccEpisode-class 
#' @return ccRecord object
add.episode.to.record <- function(rec, episode) {
    rec@episodes[[length(rec@episodes) + 1]] <- episode
    index.record(rec)
}

#' Adding a list of ccEpisode to ccRecord
#' 
#' @description Adding a list of one or multiple ccEpisode objects to a
#' ccRecord object, the information table (infotb) will be updated automatically.
#' It is the more efficient way to add multiple ccEpisode objects. See
#' add.episode.to.record() for just adding one ccEpisode. 
#' @param rec ccRecord
#' @param lst a list of ccEpisode objects
#' @return ccRecord
add.episode.list.to.record <- function(rec, lst) {
    for(i in seq(length(lst)))
        rec@episodes[[length(rec@episodes) + 1]] <- lst[[i]]
    index.record(rec)
}

#' Combine two ccRecord objects
#'
#' Combine two ccRecord objects and re-calculate the infortb
#' 
#' @param rec1 ccRecord object
#' @param rec2 ccRecord object
#' @return ccRecord object
add.record.to.record <- function(rec1, rec2) {
    rec1@episodes <- append(rec1@episodes, rec2@episodes)
    index.record(rec1)
}


#' Adding a list of ccEpisode objects to a ccRecord 
#' 
#' @param e1 ccRecord-class
#' @param e2 A list of ccEpisode objects 
#' @return ccRecord-class 
setMethod('+', c("ccRecord", "list"), 
          function(e1, e2) {add.episode.list.to.record(e1, e2)}
          )

#' Adding one ccEpisode object to a ccRecord 
#' 
#' @param e1 ccRecord-class
#' @param e2 ccEpisode-class
#' @return ccRecord-class 
setMethod('+', c("ccRecord", "ccEpisode"), 
          function(e1, e2) {add.episode.to.record(e1, e2)})

#' Combine two ccRecord objects 
#' 
#' @param e1 ccRecord-class
#' @param e2 ccRecord-class
#' @return ccRecord-class
setMethod('+', c("ccRecord", "ccRecord"), 
          function(e1, e2) {add.record.to.record(e1, e2)}
          )

#' Adding nothing to a ccRecord object.
#' 
#' @param e1 ccRecord-class 
#' @param e2 NULL 
setMethod('+', c("ccRecord", "NULL"), 
          function(e1, e2) return(e1))


index.record <- function(rec) {
    retrieve_all <- function(x) {
        .simple.data.frame(list(site_id    = x@site_id, 
                                episode_id = x@episode_id,
                                nhs_number = x@nhs_number, 
                                pas_number = x@pas_number, 
                                t_admission= x@t_admission, 
                                t_discharge= x@t_discharge, 
                                parse_file = x@parse_file, 
                                parse_time = x@parse_time))
    
    }
    rec@nepisodes <- length(rec@episodes)
    rec@infotb <- rbindlist(for_each_episode(rec, retrieve_all))

    # id will be filled in the following sequence, NHS number, PAS number,
    # site-episode combination and unknown tags. 
    if (nrow(rec@infotb) > 1) {
        id <- rec@infotb$nhs_number
        id[id=="NA"] <- rec@infotb$pas_number[id=="NA"]
        id[id=="NA"] <- paste(rec@infotb$site_id[id=="NA"], 
                                rec@infotb$episode_id[id=="NA"],
                                sep="-")
        id[id=="NA-NA"] <- paste("unknown", seq(length(which(id=="NA-NA"))))
        id <- data.table(id=id)
        id[, pid:=.GRP, by="id"]
        rec@infotb[, pid:=id$pid]
        rec@infotb[, index:=seq(nrow(rec@infotb))]
    }
    rec
}


#' Create a new episode
#' 
#' create a new ccEpisode object by given the episode data as a
#' list. The list should be organised in data items and indexed with NIHC code,
#' e.g. NIHR_HIC_ICU_0108. 
#'
#' @param lt is a list
#' @param parse_file the file location from which the episode comes from.
#' @param parse_time the parse date and time of the episode.
#' @return ccEpisode object
#' @examples 
#' eps <- list()
#' eps[["NIHR_HIC_ICU_0018"]] <- data.frame(time=seq(10), rep(70, 10))
#' new.episode(eps)
#' 
#' @export 
new.episode <- function(lt=list(), parse_file="NA", parse_time=as.POSIXct(NA)) { 
    eps <- ccEpisode()
    eps@data <- lt
    
    short.name <- c("NHSNO", "pasno", "ADNO", "ICNNO")
    slot.name  <- c("nhs_number", "pas_number", "episode_id", "site_id")
    
    # character values 
    for (i in seq(slot.name)) {
        val <- lt[[stname2code(short.name[i])]]
        if (is.null(val)) slot(eps, slot.name[i]) <- "NA"
        else slot(eps, slot.name[i]) <- val
    } # Time data 
    short.name <- c("DAICU", "DDICU")
    slot.name <- c("t_admission", "t_discharge")
    for (i in seq(slot.name)) 
        slot(eps, slot.name[i]) <- as.POSIXct(
                    xmlTime2POSIX(lt[[stname2code(short.name[i])]], allow=T))

    eps@data[["AGE"]] <- eps@t_admission -
        xmlTime2POSIX(eps@data[[stname2code("DOB")]], allow=T)
    eps@data[["LENSTAY"]] <- eps@t_discharge - eps@t_admission


    eps@parse_file <- parse_file
    eps@parse_time <- parse_time 
    eps
}

#' loop over all episodes of a ccRecord object 
#' 
#' @param record ccRecord 
#' @param fun function 
#' @export 
for_each_episode <- function(record, fun) {
    lapply(record@episodes, fun)
}


#' Subseting a ccRecord object and return a list of ccEpisode objects.
#' 
#' @param x ccRecord-class
#' @param i integer vector
#' @exportMethod [[
setMethod("[[", "ccRecord",
          function(x, i) {
              eplst <- list()
              for (ep in i) {
                  eplst[[length(eplst) + 1]] <- x@episodes[[ep]]
              }
              eplst
          }
)

#' Create a subset of ccRecord object from the original one via specifying the row number of episodes.
#'
#' @param x ccRecord-class
#' @param i integer vector
#' @exportMethod [
setMethod("[", "ccRecord",
          function(x, i){ 
              eplst <- list()
              for (ep in i) {
                  eplst[[length(eplst) + 1]] <- x@episodes[[ep]]
              }
              ccRecord() + eplst
          })

#' Create a ccRecord subset via selected sites.
#'
#' @param x ccRecord-class
#' @param i character vector which contains site_ids, e.g. c("Q70", "Q70W")
#' @exportMethod [
setMethod("[", signature(x="ccRecord", i="character"), 
          definition=function(x, i) {
              stopifnot(all(i%in%rownames(site.info())))
              ind <- x@infotb[site_id%in%i]$index
              if (length(ind) == 0) {
                  return(ccRecord())
              }
              eplst <- list()
              for (ep in ind) {
                  eplst[[length(eplst) + 1]] <- x@episodes[[ep]]
              }
              ccRecord() + eplst
          })


#' Subset episodes from the specified XML files. 
#' 
#' @param ccd ccRecord object
#' @param files character a vector of XML file names - see ccRecord: parse_file 
#' @return ccRecord object 
#' @export ccRecord_subset_files
ccRecord_subset_files <- function(ccd, files) {
    ind <- ccd@infotb[parse_file%in%files]$index
    if (length(ind) == 0) {
        return(ccRecord())
    }
    eplst <- list()
    for (ep in ind) {
        eplst[[length(eplst) + 1]] <- ccd@episodes[[ep]]
    }
    ccRecord() + eplst
}
