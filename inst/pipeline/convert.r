ym <- yaml.load_file('data/ITEM_REF.yaml')
csv <- read.csv("shortname2.csv")
csv <- csv[2:3]

for (i in seq(nrow(csv))) {
    code <-
        as.character(data.checklist$NHICcode[data.checklist$dataItem==as.character(csv[i,2])])
    ym[[code]]$shortName <- as.character(csv[i, 1])
}

f <- file("new.yml")
writeLines(con=f, as.yaml(ym))
close(f)
