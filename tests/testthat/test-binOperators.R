context("Test negate")

test_that("Lost Attributes", {
    expect_that(attr(negate(binary(Byte(), signed=TRUE)), "class"), equals(c("binary","logical")))
    expect_that(attr(negate(binary(Byte(), signed=TRUE)), "littleEndian"), equals(FALSE))
    expect_that(attr(negate(binary(Byte(), signed=TRUE)), "signed"), equals(TRUE))
    expect_that(attr(negate(binary(Byte(), signed=TRUE, littleEndian=TRUE)), "littleEndian"), equals(TRUE))
})

test_that("Return negate", {
    expect_that(bin2dec(negate(binary(Byte(), signed=TRUE))), equals(0))
    expect_that(bin2dec(negate(dec2bin(0, signed=TRUE))), equals(0))
    expect_that(bin2dec(negate(dec2bin(0, signed=FALSE))), equals(0))
    expect_that(bin2dec(negate(dec2bin(0, signed=TRUE, littleEndian=TRUE))), equals(0))
    expect_that(bin2dec(negate(dec2bin(0, signed=FALSE, littleEndian=TRUE))), equals(0))
    expect_that(bin2dec(negate(dec2bin(-1, signed=TRUE))), equals(1))
    expect_that(bin2dec(negate(dec2bin(-1, signed=TRUE, littleEndian=TRUE))), equals(1))
    expect_that(bin2dec(negate(dec2bin(1, signed=TRUE))), equals(-1))
    expect_that(bin2dec(negate(dec2bin(1, signed=FALSE))), equals(-1))
    expect_that(bin2dec(negate(dec2bin(1, signed=TRUE, littleEndian=TRUE))), equals(-1))
    expect_that(bin2dec(negate(dec2bin(1, signed=FALSE, littleEndian=TRUE))), equals(-1))    
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

one <- dec2bin(1, signed=TRUE, size=1)
l <- rep(FALSE, 4)
l2 <- c(TRUE,TRUE,FALSE,TRUE)
l3 <- c(TRUE,FALSE,TRUE,FALSE)
l4 <- c(FALSE,TRUE,FALSE,FALSE)
l5 <- c(TRUE,FALSE,FALSE,FALSE)

test_that("Return shiftLeft", {
    expect_that(length(shiftLeft(binary(Byte()), 1)), equals(Byte()))
    expect_that(length(shiftLeft(logical(Byte()), 1)), equals(Byte()))
    expect_that(bin2dec(shiftLeft(one, 1)), equals(2))
    expect_that(bin2dec(shiftLeft(one, 2)), equals(4))
    expect_that(bin2dec(shiftLeft(one, 3)), equals(8))
    expect_that(shiftLeft(l, 1), equals(l))
    expect_that(shiftLeft(l2, 1), equals(l3))
    expect_that(shiftLeft(l2, 2), equals(l4))
    expect_that(shiftLeft(l2, 3), equals(l5))
})


context("Test shiftRight")

test_that("Lost Attributes", {
    expect_that(class(shiftRight(binary(Byte()),1)), equals(c("binary","logical")))
    expect_that(attr(shiftRight(binary(Byte()),1), "signed"), equals(FALSE))
    expect_that(attr(shiftRight(binary(Byte()),1), "littleEndian"), equals(FALSE))
    expect_that(attr(shiftRight(binary(Byte(), signed=TRUE),1), "signed"), equals(TRUE))
    expect_that(attr(shiftRight(binary(Byte(), littleEndian=TRUE),1), "littleEndian"), equals(TRUE))
    expect_that(class(shiftRight(logical(Byte()),1)), equals("logical"))
})

eight <- dec2bin(8, signed=TRUE, size=1)
l <- rep(FALSE, 4)
l2 <- c(TRUE,TRUE,FALSE,TRUE)
l3 <- c(FALSE,TRUE,TRUE,FALSE)
l4 <- c(FALSE,FALSE,TRUE,TRUE)
l5 <- c(FALSE,FALSE,FALSE,TRUE)

test_that("Return shiftRight", {
    expect_that(length(shiftRight(binary(Byte()), 1)), equals(Byte()))
    expect_that(bin2dec(shiftRight(eight, 1)), equals(4))
    expect_that(bin2dec(shiftRight(eight, 2)), equals(2))
    expect_that(bin2dec(shiftRight(eight, 3)), equals(1))
    expect_that(bin2dec(shiftRight(eight, 4)), equals(0))
    expect_that(shiftRight(l, 1), equals(l))
    expect_that(shiftRight(l2, 1), equals(l3))
    expect_that(shiftRight(l2, 2), equals(l4))
    expect_that(shiftRight(l2, 3), equals(l5))  
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

one <- dec2bin(1, signed=TRUE, size=1)
l <- rep(FALSE, 4)
l2 <- c(TRUE,TRUE,FALSE,TRUE)
l3 <- c(TRUE,FALSE,TRUE,TRUE)
l4 <- c(FALSE,TRUE,TRUE,TRUE)
l5 <- c(TRUE,TRUE,TRUE,FALSE)

test_that("Return rotate", {
    expect_that(length(rotate(binary(Byte()), 1)), equals(Byte()))
    expect_that(bin2dec(rotate(one, 1)), equals(2))
    expect_that(bin2dec(rotate(one, 2)), equals(4))
    expect_that(bin2dec(rotate(one, 3)), equals(8))
    expect_that(rotate(l, 1), equals(l))
    expect_that(rotate(l2, 1), equals(l3))
    expect_that(rotate(l2, 2), equals(l4))
    expect_that(rotate(l2, 3), equals(l5))      
})

context("Test fillBits")

test_that("Lost Attributes", {
    expect_that(class(fillBits(binary(Byte()), 2)), equals(c("binary","logical")))
    expect_that(attr(fillBits(binary(Byte()), 2), "signed"), equals(FALSE))
    expect_that(attr(fillBits(binary(Byte()), 2), "littleEndian"), equals(FALSE))
    expect_that(attr(fillBits(binary(Byte(), signed=TRUE), 2), "signed"), equals(TRUE))
    expect_that(attr(fillBits(binary(Byte(), littleEndian=TRUE), 2), "littleEndian"), equals(TRUE))
})

input1 <- as.binary(c(TRUE,TRUE,FALSE,TRUE))
l2 <- as.binary(c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,TRUE))
l3 <- as.binary(c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE))
input2 <- as.binary(c(TRUE,TRUE,FALSE,TRUE), littleEndian=TRUE)
l4 <- as.binary(c(TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE), littleEndian=TRUE)
l5 <- as.binary(c(TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE), littleEndian=TRUE)

test_that("Return fillBits", {
    expect_that(fillBits(input1, value=FALSE, size=1), equals(l2))
    expect_that(fillBits(input1, value=TRUE, size=1), equals(l3))
    expect_that(fillBits(input2, value=FALSE, size=1), equals(l4))
    expect_that(fillBits(input2, value=TRUE, size=1), equals(l5))    
})

context("Test switchEndianess")

test_that("Lost Attributes", {
    expect_that(class(switchEndianess(binary(Byte()))), equals(c("binary","logical")))
    expect_that(attr(switchEndianess(binary(Byte())), "signed"), equals(FALSE))
    expect_that(attr(switchEndianess(binary(Byte())), "littleEndian"), equals(TRUE))
    expect_that(attr(switchEndianess(binary(Byte(), signed=TRUE)), "signed"), equals(TRUE))
    expect_that(attr(switchEndianess(binary(Byte(), littleEndian=TRUE)), "littleEndian"), equals(FALSE))
})

s1 <- as.binary(c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE))
s2 <- as.binary(c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE), littleEndian=TRUE)
s3 <- as.binary(c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE), signed=TRUE)
s4 <- as.binary(c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE), signed=TRUE, littleEndian=TRUE)

test_that("Return fillBits", {
    expect_that(switchEndianess(s1), equals(s2))
    expect_that(switchEndianess(s2), equals(s1))
    expect_that(switchEndianess(s3), equals(s4))
    expect_that(switchEndianess(s4), equals(s3))
})