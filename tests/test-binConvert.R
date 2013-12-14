library(testthat)
library(binaryLogic)

context("Test dec2bin")

test_that("Lost Attributes", {
    expect_that(attr(dec2bin(1), "class"), equals(c("binary","logical")))
    expect_that(attr(dec2bin(1), "signed"), equals(FALSE))
    expect_that(attr(dec2bin(1), "littleEndian"), equals(FALSE))
    expect_that(attr(dec2bin(1, signed=TRUE), "signed"), equals(TRUE))
    expect_that(attr(dec2bin(1, littleEndian=TRUE), "littleEndian"), equals(TRUE))
})

test_that("Return value", {
    expect_that(dec2bin(0), is_a("binary"))
    expect_that(dec2bin(0), is_equivalent_to(as.binary(c(0))))
    expect_that(dec2bin(0, signed=TRUE, size=1), is_equivalent_to(as.binary(c(0,0,0,0,0,0,0,0))))
    expect_that(dec2bin(0, littleEndian=TRUE, signed=TRUE, size=1), is_equivalent_to(as.binary(c(0,0,0,0,0,0,0,0))))
    expect_that(dec2bin(1), is_equivalent_to(as.binary(c(1), signed=FALSE, littleEndian=FALSE)))
    expect_that(dec2bin(1, signed=TRUE, size=1), is_equivalent_to(as.binary(c(0,0,0,0,0,0,0,1))))
    expect_that(dec2bin(1, littleEndian=TRUE, signed=TRUE, size=1), is_equivalent_to(as.binary(c(1,0,0,0,0,0,0,0), signed=TRUE, littleEndian=TRUE)))
    expect_that(dec2bin(-1,signed=TRUE), is_equivalent_to(as.binary(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))))
    expect_that(dec2bin(-1, littleEndian=TRUE, signed=TRUE, size=2), is_equivalent_to(as.binary(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))))
    expect_that(dec2bin(8, signed=TRUE, size=1) , is_equivalent_to(as.binary(c(0,0,0,0,1,0,0,0), signed=TRUE)))
    expect_that(dec2bin(8, littleEndian=TRUE, signed=TRUE, size=1) , is_equivalent_to(as.binary(c(0,0,0,1,0,0,0,0), signed=TRUE, littleEndian=TRUE)))
})

test_that("Warnings", {
    expect_that(dec2bin(), throws_error())
    expect_that(dec2bin(-2), throws_error("Negative number is not possible with unsigned method."))
    #expect_that(dec2bin(2^15, signed=TRUE), throws_error("Out of Range. Please increase the size[Byte]"))
})


# need some testing for attributes

context("Test bin2dec")

test_that("Return value", {
    expect_that(bin2dec(as.binary(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), signed=TRUE)), is_equivalent_to(-1))
    expect_that(bin2dec(as.binary(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), signed=TRUE, littleEndian=TRUE)), is_equivalent_to(-1))
    expect_that(bin2dec(as.binary(c(0))), is_equivalent_to(0))
    expect_that(bin2dec(as.binary(c(0), littleEndian=TRUE)), is_equivalent_to(0))
    expect_that(bin2dec(as.binary(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), signed=TRUE, littleEndian=TRUE)), is_equivalent_to(0))
    expect_that(bin2dec(as.binary(c(1))), is_equivalent_to(1))
    expect_that(bin2dec(as.binary(c(1), littleEndian=TRUE)), is_equivalent_to(1))
    expect_that(bin2dec(as.binary(c(1,0,0,0,0,0,0,0), signed=TRUE, littleEndian=TRUE)), is_equivalent_to(1))
    expect_that(bin2dec(as.binary(c(0,0,0,0,0,0,0,1), signed=TRUE)), is_equivalent_to(1))
    expect_that(bin2dec(as.binary(c(1,1,1,1)), hex=TRUE), is_equivalent_to(as.hexmode("f")))
})

test_that("Warnings", {
    expect_that(bin2dec(), throws_error())
})


context("Test both (dec2bin, bin2dec)")

# in work !
inputtest <- function(s)
{
    for(i in s) {
        x <- binaryLogic::bin2dec(binaryLogic::dec2bin(i, signed=TRUE))
        if  (x != i) return(1)
    }
    return(0)
}

test_that("Range", {
    expect_that(inputtest(-64:64), equals(0))
})

