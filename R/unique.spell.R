#' find the unique spell ID.
#'
#' @param record ccRecord
#' @return data.table contains spell id.
#' @export unique.spell
unique.spell <- function(rec, duration=2) {
    tb <- rec@infotb
    short.time.group <- function(sd) {
        zeroday <- 0
        if (length(sd[[1]]) == 1)
            return(zeroday)
        dic <- sd$t_discharge[1:length(sd$t_discharge)-1]
        adm <- sd$t_admission[2:length(sd$t_admission)]
        
        diffday <- (c(zeroday, difftime(adm, dic, units="days")))
        diffday[is.na(diffday)] <- 0
        diffday
    }
    setkey(tb, "pid", "t_admission", "t_discharge")
    tb[, diffday:=short.time.group(.SD), by="pid"]

    spell <- Reduce(sum, tb$diffday == 0 | tb$diffday > duration, accumulate=T)
    tb$spell <- spell
    return(tb)
}

#' @export demographic.patient.spell
demographic.patient.spell <- function(rec, duration=2) {
    dmg <- sql.demographic.table(rec)
    us <- unique.spell(rec, duration)
    us <- data.table(index=us$index, pid=us$pid, spell=us$spell)
    dmg <- merge(dmg, us, by=c("index"))
    return(dmg)
} 
