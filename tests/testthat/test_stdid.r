context("Testing standard IDs")

test_that("single id initialisation",{
    selected_tags <- StdId(c("NIHR_HIC_ICU_0001"))
    expect_match(selected_tags@ids, "NIHR_HIC_ICU_0001")

    selected_tags <- StdId(c("something.NIHR_HIC_ICU_0001"))
    expect_match(selected_tags@ids, "NIHR_HIC_ICU_0001")
    
    selected_tags <- StdId(c("x.NIHR_HIC_ICU_0001aac"))
    expect_match(selected_tags@ids, "NIHR_HIC_ICU_0001")

    expect_error(StdId(c("NIHR_HICx_ICU_0001")))
    expect_error(StdId())
    expect_error(StdId(character()))
})


test_that("multiple id initialisation", {
    tags <- c("something. NIHR_HIC_ICU_0001", "xxxNIHR_HIC_ICU_0002xxx")
    selected_tags <- StdId(tags)
    expect_match(selected_tags@ids[1], "NIHR_HIC_ICU_0001")
    expect_match(selected_tags@ids[2], "NIHR_HIC_ICU_0002")
})


test_that("test as.number", {
    selected_tags <- StdId("NIHR_HIC_ICU_0001")
    expect_match("0001", as.number(selected_tags))
    
    selected_tags <- StdId("NIHR_HIC_ICU_9999")
    expect_match("9999", as.number(selected_tags))
})

test_that("test .as.number", {
    expect_equal(.as.number("NIHR_HIC_ICU_0001"), 1)
})
