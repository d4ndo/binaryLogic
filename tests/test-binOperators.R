library(testthat)
library(binaryLogic)

context("Test negate")

test_that("Lost Attributes", {
    expect_that(attr(negate(binary(Byte(), signed=TRUE)), "class"), equals(c("binary","logical")))
    expect_that(attr(negate(binary(Byte(), signed=TRUE)), "littleEndian"), equals(FALSE))
    expect_that(attr(negate(binary(Byte(), signed=TRUE)), "signed"), equals(TRUE))
    expect_that(attr(negate(binary(Byte(), signed=TRUE, littleEndian=TRUE)), "littleEndian"), equals(TRUE))
})


context("Test shiftLeft")

test_that("Lost Attributes", {
    expect_that(class(shiftLeft(binary(Byte()),1)), equals(c("binary","logical")))
    expect_that(attr(shiftLeft(binary(Byte()),1), "signed"), equals(FALSE))
    expect_that(attr(shiftLeft(binary(Byte()),1), "littleEndian"), equals(FALSE))
    expect_that(attr(shiftLeft(binary(Byte(), signed=TRUE),1), "signed"), equals(TRUE))
    expect_that(attr(shiftLeft(binary(Byte(), littleEndian=TRUE),1), "littleEndian"), equals(TRUE))
    expect_that(class(shiftLeft(logical(Byte()),1)), equals("logical"))
})


context("Test shiftRight")

test_that("Lost Attributes", {
    expect_that(class(shiftRight(binary(Byte()),1)), equals("binary"))
    expect_that(attr(shiftRight(binary(Byte()),1), "signed"), equals(FALSE))
    expect_that(attr(shiftRight(binary(Byte()),1), "littleEndian"), equals(FALSE))
    expect_that(attr(shiftRight(binary(Byte(), signed=TRUE),1), "signed"), equals(TRUE))
    expect_that(attr(shiftRight(binary(Byte(), littleEndian=TRUE),1), "littleEndian"), equals(TRUE))
    expect_that(class(shiftRight(logical(Byte()),1)), equals("logical"))
})

context("Test rotate")

test_that("Lost Attributes", {
    expect_that(class(rotate(binary(Byte()),1)), equals(c("binary","logical")))
    expect_that(attr(rotate(binary(Byte()),1), "signed"), equals(FALSE))
    expect_that(attr(rotate(binary(Byte()),1), "littleEndian"), equals(FALSE))
    expect_that(attr(rotate(binary(Byte(), signed=TRUE),1), "signed"), equals(TRUE))
    expect_that(attr(rotate(binary(Byte(), littleEndian=TRUE),1), "littleEndian"), equals(TRUE))
    expect_that(class(rotate(logical(Byte()),1)), equals("logical"))
})

context("Test fillBits")

test_that("Lost Attributes", {
    expect_that(class(fillBits(binary(Byte()), 2)), equals(c("binary","logical")))
    expect_that(attr(fillBits(binary(Byte()), 2), "signed"), equals(FALSE))
    expect_that(attr(fillBits(binary(Byte()), 2), "littleEndian"), equals(FALSE))
    expect_that(attr(fillBits(binary(Byte(), signed=TRUE), 2), "signed"), equals(TRUE))
    expect_that(attr(fillBits(binary(Byte(), littleEndian=TRUE), 2), "littleEndian"), equals(TRUE))
})

context("Test switchEndianess")

test_that("Lost Attributes", {
    expect_that(class(switchEndianess(binary(Byte()))), equals("binary"))
    expect_that(attr(switchEndianess(binary(Byte())), "signed"), equals(FALSE))
    expect_that(attr(switchEndianess(binary(Byte())), "littleEndian"), equals(TRUE))
    expect_that(attr(switchEndianess(binary(Byte(), signed=TRUE)), "signed"), equals(TRUE))
    expect_that(attr(switchEndianess(binary(Byte(), littleEndian=TRUE)), "littleEndian"), equals(FALSE))
})

