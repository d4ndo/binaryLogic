context("Test binary")

b <- binary(Byte())
bs <- binary(Byte(), signed=TRUE)
bl <- binary(Byte(), littleEndian=TRUE)

test_that("Lost Attributes", {
    expect_that(class(b), equals(c("binary","logical")))
    expect_that(attr(bs, "signed"), equals(TRUE))
    expect_that(attr(bl, "littleEndian"), equals(TRUE))
})

n <- c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
class(n) <- c("binary","logical")
attr(n, "signed") <- FALSE
attr(n, "littleEndian") <- FALSE
    
test_that("Return binary", {
    expect_that(length(binary(Byte())), equals(Byte()))
    expect_that(length(binary(Byte(), signed=TRUE)), equals(Byte()))
    expect_that(length(binary(1, signed=TRUE)), equals(Byte()))
    expect_that(binary(Byte()), equals(n))
})

context("Test as.binary")

b <- as.binary(rep(FALSE,8))
bs <- as.binary(rep(FALSE,8), signed=TRUE)
bs2 <- as.binary(rep(FALSE,1), signed=TRUE)
bl <- as.binary(rep(FALSE,8), littleEndian=TRUE)

test_that("Lost Attributes", {
    expect_that(class(b), equals(c("binary","logical")))
    expect_that(attr(b, "signed"), equals(FALSE))
    expect_that(attr(b, "littleEndian"), equals(FALSE))
    expect_that(attr(bs, "signed"), equals(TRUE))
    expect_that(attr(bl, "littleEndian"), equals(TRUE))
})

test_that("Return as.binary", {
    expect_that(length(b), equals(Byte()))
    expect_that(length(bs2), equals(Byte()))    
})

context("Test is.binary")

b <- binary(1)
l <- logical(1)
i <- integer(1)
d <- double(1)
n <- numeric(1)
f <- factor(1)

test_that("Return as.binary", {
    expect_that(is.binary(b), equals(TRUE))
    expect_that(is.binary(l), equals(FALSE))
    expect_that(is.binary(i), equals(FALSE))
    expect_that(is.binary(d), equals(FALSE))
    expect_that(is.binary(n), equals(FALSE))
    expect_that(is.binary(f), equals(FALSE))
})


context("Test print.binary")
cat("no test available")

context("Test summary.binary")
cat("no test available")

context("Test as.raw.binary")
cat("no test available")

context("Test as.integer.binary")
cat("no test available")

context("Test as.double.binary")
cat("no test available")

context("Test Ops.binary")
cat("no test available")

context("Test '+.binary'")
cat("no test available")

context("Test '-.binary'")
cat("no test available")

context("Test '==.binary'")
cat("no test available")

context("Test '!=.binary'")
cat("no test available")

context("Test '[.binary'")
cat("no test available")

context("Test rev")

b <- binary(Byte())
sb <- binary(Byte(), signed=TRUE)
lb <- binary(Byte(), littleEndian=TRUE)

test_that("Lost Attributes", {
    expect_that(attr(rev(b), "class"), equals(c("binary","logical")))
    expect_that(attr(rev(b), "signed"), equals(FALSE))
    expect_that(attr(rev(b), "littleEndian"), equals(FALSE))
    expect_that(attr(rev(sb), "signed"), equals(TRUE))
    expect_that(attr(rev(lb), "littleEndian"), equals(TRUE))
})

context("Test saveAttributes")
cat("no test available")

context("Test loadAttributes")
cat("no test available")
