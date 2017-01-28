context("data parsing pipeline")

test.setup <- function() {
    for (i in c(".temp", ".temp/XML")) {
        unlink(i, recursive=T)
        dir.create(i)
    }
}

test.teardown <- function() {
    unlink(".temp", recursive=T)
}


test_that("Discover parsed XML files and new files", {
    test.setup()
    f <- find.new.xml.file(".temp/XML")
    expect_true(dir.exists(".temp/XML/.database"))
    expect_length(f, 0)

    file.create(".temp/XML/test1.xml")
    file.create(".temp/XML/test2.xml")
    
    f <- find.new.xml.file(".temp/XML")
    expect_equivalent(f, c("test1.xml", "test2.xml"))

    file.create(".temp/XML/.database/test1.xml_someothersuffix.RData")
    f <- find.new.xml.file(".temp/XML")
    expect_equivalent(f, c("test2.xml"))

    # adding xml files with the same name pattern should not initiating the 
    # parsing process. 
    file.create(".temp/XML/test1.xml_1.partxml")
    file.create(".temp/XML/test1.xml_2.partxml")
    f <- find.new.xml.file(".temp/XML")
    expect_equivalent(f, "test2.xml")
   
    # test if .partxml suffix can pass
    file.create(".temp/XML/test2.xml_2.partxml")
    file.create(".temp/XML/test2.xml_1.partxml")
    file.create(".temp/XML/test2.xml_3.partxml")
    f <- find.new.xml.file(".temp/XML")

    # should pass with XML as suffix
    file.create(".temp/XML/file.XML")
    find.new.xml.file(".temp/XML")

    file.create(".temp/XML/not_end_with_xmlsuffix")
    expect_error(find.new.xml.file(".temp/XML"))
    file.remove(".temp/XML/not_end_with_xmlsuffix")

    test.teardown()
})


#test_that("update the new XML files", {
#    test.setup()
#    system("cp ../data/test_data_anonym.xml .temp/XML/test1.xml")
#    system("cp ../data/test_data_anonym.xml .temp/XML/test2.xml")
#    new.db <- update.new.xml(".temp/XML", quiet=T)
#    expect_is(new.db, "ccRecord")
#
#    xml1 <- xml2Data(".temp/XML/test1.xml")
#    xml2 <- xml2Data(".temp/XML/test2.xml")
#    
#    expect_equal(new.db@nepisodes, xml1@nepisodes + xml2@nepisodes)
#    expect_true("test1.xml.RData" %in% dir(".temp/XML/.database"))
#    expect_true("test2.xml.RData" %in% dir(".temp/XML/.database"))
#})

#test_that("update the database", {
#    alld <- update_database(xml.path=".temp/XML", restart=T)
#    expect_true("alldata.RData" %in% dir(".temp/XML/.database"))
#    expect_equal(alld@nepisodes, 4)
#
#    alld <- update_database(xml.path=".temp/XML", restart=F)
#    expect_equal(alld@nepisodes, 4)
#
#    load(".temp/XML/.database/alldata.RData")
#    expect_equal(alldata@nepisodes, 4)
#
#})
#
#test_that("break down the large XML files", {
#    if (Sys.info()[['sysname']] != "Darwin") {
#        partxml.dir <-".temp/XML/.partxml" 
#
#        unlink("partxml.dir", recursive=T)
#        expect_true(!file.exists("partxml.dir"))
#
#        unlink(".temp/XML/.database/", recursive=T)
#        break.down.xml(".temp/XML")
#        expect_true(file.exists(partxml.dir))
#        expect_equal(length(dir(partxml.dir)), 2)
#
#        file.create(paste(partxml.dir, "files_to_be_delete", sep="/"))
#        break.down.xml(".temp/XML")
#        expect_true(file.exists(partxml.dir))
#        expect_equal(length(dir(partxml.dir)), 2)
#    }
#})

test_that("Tear down the test", {
    test.teardown()
})
