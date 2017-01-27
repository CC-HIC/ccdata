context("Testing table selection")

test_that("",{
    itemsToDataFrame(ccdt@episodes[[1]], "NIHR_HIC_ICU_0108", 2, 1)
})



test_that("",{
    selectTable(ccdt, items_opt="NIHR_HIC_ICU_0108", freq=1)
})

