context("Tests of the xml parser")

test_that("check original filename",
{
  # with no changes
  file_input_orig <- "and_its_file.xml"
  file_input <- paste("/this/is/a/path/", file_input_orig, sep="")
  expect_equal(extract_file_origin(file_input),file_input_orig)

  # with a suffix
  file_input <- "/this/is/a/path/and_its_file.xml_00.part"
  expect_equal(extract_file_origin(file_input),file_input_orig)

  # with multiple xmls
  file_input <- "/this/is/a/path/and_its_file.xml_00.xml.part"
  expect_equal(extract_file_origin(file_input),file_input_orig)

  # extracting different extension
  file_input <- "/this/is/a/path/and_its_file.xml.txt.part"
  expect_equal(extract_file_origin(file_input, removestr='.txt'),
               paste(file_input_orig, ".txt", sep=""))
})


test_that("load xml file", 
{
})


test_that("check first patient heart rate (2d)",
{
})


test_that("check patients nhs number and pas number (1d)", 
{
})


test_that("test searching",
{

})
