#!/usr/bin/Rscript

library(cleanEHR)

args = commandArgs(trailingOnly=TRUE)

institute <- c("CUH", "Oxford", "GSTT", "imperial", "UCLH")

data<-dir(institute, full.name=TRUE)[grep(".RData", dir(institute,
                                                        full.name=TRUE))]

new <- ccRecord()
for (d in data) {
    print(d)
    load(d)
    new <- new + ccd 
}

ccd <- new

save(ccd, file=args[1])
