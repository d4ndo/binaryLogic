library(testthat)
library(binaryLogic)

context("dec2bin")

test_that("Return value", {
    expect_that(dec2bin(0), is_a("binary"))
    expect_that(dec2bin(0), is_equivalent_to(as.binary(c(0))))
    expect_that(dec2bin(0, signed=TRUE, size=1), is_equivalent_to(as.binary(c(0,0,0,0,0,0,0,0))))
    expect_that(dec2bin(0, littleEndian=TRUE, signed=TRUE, size=1), is_equivalent_to(as.binary(c(0,0,0,0,0,0,0,0))))
    expect_that(dec2bin(1), is_equivalent_to(as.binary(c(1))))
    expect_that(dec2bin(1, signed=TRUE, size=1), is_equivalent_to(as.binary(c(0,0,0,0,0,0,0,1))))
    expect_that(dec2bin(1, littleEndian=TRUE, signed=TRUE, size=1), is_equivalent_to(as.binary(c(1,0,0,0,0,0,0,0))))    
    expect_that(dec2bin(-1,signed=TRUE), is_equivalent_to(as.binary(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))))
    expect_that(dec2bin(-1, littleEndian=TRUE, signed=TRUE, size=2), is_equivalent_to(as.binary(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))))
    expect_that(dec2bin(8, signed=TRUE, size=1) , is_equivalent_to(as.binary(c(0,0,0,0,1,0,0,0))))
    expect_that(dec2bin(8, littleEndian=TRUE, signed=TRUE, size=1) , is_equivalent_to(as.binary(c(0,0,0,1,0,0,0,0))))
})

test_that("Warnings", {
    expect_that(dec2bin(), throws_error("num is missing."))
    expect_that(dec2bin(-2), throws_error("Negative number is not possible with unsigned method."))
    #expect_that(dec2bin(2^15, signed=TRUE), throws_error("Out of Range. Please increase the size[Byte]"))
})

context("bin2dec")

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
    expect_that(bin2dec(), throws_error("bin is missing."))
})