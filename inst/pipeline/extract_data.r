#!/usr/bin/Rscript

library(cleanEHR)

args = commandArgs(trailingOnly=TRUE)

cat("extract patients from", c(args[1], "\n"))
ccd <- xml2Data(args[1])
save(ccd, file=paste(args[1], ".RData", sep="_"))
