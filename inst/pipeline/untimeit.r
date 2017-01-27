#!/usr/bin/Rscript

library(ccdata)

args = commandArgs(trailingOnly=TRUE)

load(args[1])
ccd <- reindexRecord(ccd)
ccd <- deltaTime(ccd, pseudotime=T)
ccd <- uniquePatients(ccd)
ccd_delta_num <- ccd
save(ccd_delta_num, file=args[2])
