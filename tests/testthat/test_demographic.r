context("Testing demographic table")

test_that("create demographic table from ccdata and expect the equivalent
          results with or without data type awareness.", {
    demg <- sql.demographic.table(ccd, dtype=F)
    expect_equal(nrow(demg), ccd@nepisodes)
    expect_match(class(demg)[1], "data.table")

    convert.back.to.char <- function(v) {
        v <- as.character(v)
        v[v=="NA"] <- "NULL"
        v[is.na(v)] <- "NULL"
        v
    }

    demg_t <- suppressWarnings(sql.demographic.table(ccd))
    for (i in seq(ncol(demg) - 1))
        expect_equivalent(demg[[i]], convert.back.to.char(demg_t[[i]]))
}) 
