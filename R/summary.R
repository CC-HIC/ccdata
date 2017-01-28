
#' @export
episode.graph <- function(ccd, eid=601, items=NULL) {
    library(gridExtra)
    ep <- ccd[[eid]][[1]]
    t_ad <- ep@t_admission
    t_dc <- ep@t_discharge


    if (is.null(items))
        items <- c("h_rate", "spo2", "bilirubin", "platelets", "pao2_fio2", "gcs_total")

    all.drugs <- names(which(class.dict_code[names(ITEM_REF)] == "Drugs"))
    used.drugs <- code2stname(all.drugs[all.drugs %in% names(ep@data)])

    classification.dictionary <- sapply(ITEM_REF, function(x) x$Classification1)


    create.long.table <- function(ep, items) {
        items <- data.table(items=items, 
                            code=stname2code(items),
                            longname=stname2longname(items),
                            class=classification.dictionary[stname2code(items)])
        units <- unit.dict[items$code]
        units[is.na(units)] <- ""
        items$longname <- paste0(items$longname, "\n", units)

        ltb <- list()
        for (i in seq(nrow(items))) {
            if (is.null(ep@data[[items[i]$code]]))
                ltb[[i]] <- data.frame()
            else
                ltb[[i]] <- data.frame(ep@data[[items[i]$code]], 
                                       item=items[i]$longname)
        }
        ltb <- rbindlist(ltb, use.names=T, fill=T)
        if (is.numeric(ltb$time))
            ltb$time <- t_ad + ltb$time * 60 * 60
        ltb$item2d <- as.numeric(ltb$item2d)
        return(ltb)
    }

    physio.tb <- create.long.table(ep, items)
    physio.tb <- data.frame(physio.tb, 
                            catg1=physio.tb$item, 
                            catg2="Physiology Data")
    drug.tb <- create.long.table(ep, used.drugs)

    drug.tb <- data.frame(drug.tb, catg1="Drugs", 
                          catg2=drug.tb$item)


    tb <- rbindlist(list(physio.tb, drug.tb), fill=T, use.names=T)


    ggp <- ggplot(tb, aes(x=time, y=item2d, group=item, colour=catg2)) + geom_line(colour="#1E506C") + 
        geom_point(size=1) + 
        facet_grid(catg1 ~., scales="free_y") + 
        geom_vline(xintercept = as.numeric(t_ad), colour="#D1746F") + 
        geom_vline(xintercept = as.numeric(t_dc), colour="#D1746F") + 
        scale_colour_manual(values=c("#1E506C", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#FFFFFF"), 
                            name=paste0(ep@episode_id, "_", ep@site_id, "\n", 
                                       icnarc2diagnosis(ep@data[[stname2code('RAICU1')]]), "\n\n")) +  
        theme(legend.title = element_text(size=8), 
              legend.text  = element_text(size=8)) +
                            labs(x="", y="")



    plot(ggp)
    #"#1E506C""#D1746F"

    return(tb)
}
