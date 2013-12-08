library(testthat)
library(binaryLogic)

context("Test rev")

b <- binary(Byte())
sb <- binary(Byte(), signed=TRUE)
lb <- binary(Byte(), littleEndian=TRUE)

test_that("Lost Attributes", {
    expect_that(attr(rev(b), "class"), equals("binary"))
    expect_that(attr(rev(b), "signed"), equals(FALSE))
    expect_that(attr(rev(b), "littleEndian"), equals(FALSE))
    expect_that(attr(rev(sb), "signed"), equals(TRUE))
    expect_that(attr(rev(lb), "littleEndian"), equals(TRUE))
})