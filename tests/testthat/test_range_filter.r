context("Testing the range filter")

test_that("from string expression to range functions",
{
    expect_true(inrange(1, "(0,10)"))
    expect_true(!inrange(2, "(-10, 0); (2, 3)"))
    expect_true(inrange(2, "(-10, 0); [2, 3)"))
    expect_true(inrange(-1.2222, "[-1.2222, 0]"))
    expect_true(!inrange(-1.2223, "[-1.2222, 0]"))
   
    expect_error(inrange(1, "()"))
    expect_error(inrange(1, "(xx, yy)"))
    expect_error(inrange(1, "100, 10"))
    expect_error(inrange(1, "(10, 1)"))

})
