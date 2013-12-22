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
bl2b <- binary(Byte(), littleEndian=TRUE)
bl2b <- as.binary(bl2b, littleEndian=FALSE)
bs2u <- binary(Byte(), signed=TRUE)
bs2u <- as.binary(bs2u, signed=FALSE)

test_that("Lost Attributes", {
    expect_that(class(b), equals(c("binary","logical")))
    expect_that(attr(b, "signed"), equals(FALSE))
    expect_that(attr(b, "littleEndian"), equals(FALSE))
    expect_that(attr(bs, "signed"), equals(TRUE))
    expect_that(attr(bl, "littleEndian"), equals(TRUE))
    expect_that(attr(bl2b, "littleEndian"), equals(FALSE))
    expect_that(attr(bs2u, "signed"), equals(FALSE))    
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

l <- rep(TRUE, 32)
l2 <- l
l2[25] <- FALSE
l3 <- l
l3[5] <- FALSE
r <- packBits(l)
r2 <- packBits(l2)
r3 <- packBits(l3)
b <- dec2bin(0xffffffff)
b2 <- dec2bin(-2, signed=TRUE, size=4)
b3 <- dec2bin(-2, signed=TRUE, littleEndian=TRUE, size=4)

test_that("Return as.raw.binary", {
    expect_that(length(as.raw(b)), equals(4))
    expect_that(as.raw(b), equals(r))
    expect_that(as.raw(b2), equals(r2))
    expect_that(as.raw(b3), equals(r3))
})


context("Test as.integer.binary")

zero <- dec2bin(0)
zeroL <- dec2bin(0, littleEndian=TRUE)
zeroS <- dec2bin(0, signed=TRUE)
zeroSL <- dec2bin(0, signed=TRUE, littleEndian=TRUE)
minusone <- dec2bin(-1, signed=TRUE)
minusoneL <- dec2bin(-1, signed=TRUE, littleEndian=TRUE)
one <- dec2bin(1)
oneL <- dec2bin(1, littleEndian=TRUE)
oneS <- dec2bin(1, signed=TRUE)
oneSL <- dec2bin(1, signed=TRUE, littleEndian=TRUE)

test_that("Type of as.integer.binary", {
    expect_that(typeof(as.integer(zero)), equals("integer"))
    expect_that(typeof(as.integer(zeroL)), equals("integer"))
    expect_that(typeof(as.integer(zeroS)), equals("integer"))
    expect_that(typeof(as.integer(zeroSL)), equals("integer"))
    expect_that(typeof(as.integer(minusone)), equals("integer"))
    expect_that(typeof(as.integer(minusoneL)), equals("integer"))
    expect_that(typeof(as.integer(one)), equals("integer"))
    expect_that(typeof(as.integer(oneL)), equals("integer"))
    expect_that(typeof(as.integer(oneS)), equals("integer"))
    expect_that(typeof(as.integer(oneSL)), equals("integer"))
})

test_that("Return as.integer.binary", {
    expect_that(as.integer(zero), equals(0))
    expect_that(as.integer(zeroL), equals(0))
    expect_that(as.integer(zeroS), equals(0))
    expect_that(as.integer(zeroSL), equals(0))
    expect_that(as.integer(minusone), equals(-1))
    expect_that(as.integer(minusoneL), equals(-1))
    expect_that(as.integer(one), equals(1))
    expect_that(as.integer(oneL), equals(1))
    expect_that(as.integer(oneS), equals(1))
    expect_that(as.integer(oneSL), equals(1))
})

context("Test as.double.binary")

zero <- dec2bin(0)
zeroL <- dec2bin(0, littleEndian=TRUE)
zeroS <- dec2bin(0, signed=TRUE)
zeroSL <- dec2bin(0, signed=TRUE, littleEndian=TRUE)
minusone <- dec2bin(-1, signed=TRUE)
minusoneL <- dec2bin(-1, signed=TRUE, littleEndian=TRUE)
one <- dec2bin(1)
oneL <- dec2bin(1, littleEndian=TRUE)
oneS <- dec2bin(1, signed=TRUE)
oneSL <- dec2bin(1, signed=TRUE, littleEndian=TRUE)

test_that("Type of as.double.binary", {
    expect_that(typeof(as.double(zero)), equals("double"))
    expect_that(typeof(as.double(zeroL)), equals("double"))
    expect_that(typeof(as.double(zeroS)), equals("double"))
    expect_that(typeof(as.double(zeroSL)), equals("double"))
    expect_that(typeof(as.double(minusone)), equals("double"))
    expect_that(typeof(as.double(minusoneL)), equals("double"))
    expect_that(typeof(as.double(one)), equals("double"))
    expect_that(typeof(as.double(oneL)), equals("double"))
    expect_that(typeof(as.double(oneS)), equals("double"))
    expect_that(typeof(as.double(oneSL)), equals("double"))
})

test_that("Return as.double.binary", {
    expect_that(as.double(zero), equals(0))
    expect_that(as.double(zeroL), equals(0))
    expect_that(as.double(zeroS), equals(0))
    expect_that(as.double(zeroSL), equals(0))
    expect_that(as.double(minusone), equals(-1))
    expect_that(as.double(minusoneL), equals(-1))
    expect_that(as.double(one), equals(1))
    expect_that(as.double(oneL), equals(1))
    expect_that(as.double(oneS), equals(1))
    expect_that(as.double(oneSL), equals(1))
})

context("Test Ops.binary")
cat("no test available")

context("Test '+.binary'")

mtwo <- dec2bin(-2, signed=TRUE)
mone <- dec2bin(-1, signed=TRUE)
zero <- dec2bin(0, signed=TRUE)
one <- dec2bin(1, signed=TRUE)
two <- dec2bin(2, signed=TRUE)

# signed and bid endian only.
test_that("Return +", {
    expect_that(zero + zero, equals(zero))
    expect_that(zero + one, equals(one))
    expect_that(one + zero, equals(one))
    expect_that(bin2dec(one + one), equals(2))
    expect_that(bin2dec(one + mone), equals(0))
    expect_that(bin2dec(mone + one), equals(0))    
    expect_that(bin2dec(zero + mone), equals(-1))
    expect_that(bin2dec(mone + zero), equals(-1))
    expect_that(bin2dec(zero + mtwo), equals(-2))
    expect_that(bin2dec(mtwo + zero), equals(-2))
    expect_that(bin2dec(one + two), equals(3))
    expect_that(bin2dec(two + one), equals(3))
    expect_that(bin2dec(two + two), equals(4))
    expect_that(bin2dec(mtwo + mtwo), equals(-4))    
})

context("Test '-.binary'")

mtwo <- dec2bin(-2, signed=TRUE)
mone <- dec2bin(-1, signed=TRUE)
zero <- dec2bin(0, signed=TRUE)
one <- dec2bin(1, signed=TRUE)
two <- dec2bin(2, signed=TRUE)

# signed and bid endian only.
test_that("Return -", {
    expect_that(zero - zero, equals(zero))
    expect_that(zero - one, equals(mone))
    expect_that(one - zero, equals(one))
    expect_that(bin2dec(one - one), equals(0))
    expect_that(bin2dec(one - mone), equals(2))
    expect_that(bin2dec(mone - one), equals(-2))    
    expect_that(bin2dec(zero - mone), equals(1))
    expect_that(bin2dec(mone - zero), equals(-1))
    expect_that(bin2dec(zero - mtwo), equals(2))
    expect_that(bin2dec(mtwo - zero), equals(-2))
    expect_that(bin2dec(one - two), equals(-1))
    expect_that(bin2dec(two - one), equals(1))
    expect_that(bin2dec(two - two), equals(0))
    expect_that(bin2dec(mtwo - mtwo), equals(0))    
})

context("Test '==.binary'")

s_mtwo <- dec2bin(-2, signed=TRUE)
s_mone <- dec2bin(-1, signed=TRUE)
s_zero <- dec2bin(0, signed=TRUE)
s_one <- dec2bin(1, signed=TRUE)
s_two <- dec2bin(2, signed=TRUE)
zero <- dec2bin(0)
one <- dec2bin(1)
two <- dec2bin(2)

test_that("Return -", {
    expect_that(one == two, equals(FALSE))
    expect_that(s_mtwo == s_mtwo, equals(TRUE))
    expect_that(s_mone == s_mone, equals(TRUE))
    expect_that(s_zero == s_zero, equals(TRUE))
    expect_that(s_one == s_one, equals(TRUE))
    expect_that(s_two == s_two, equals(TRUE))    
    expect_that(zero == zero, equals(TRUE))
    expect_that(one == one, equals(TRUE))
    expect_that(two == two, equals(TRUE))
    
    expect_that(switchEndianess(s_mtwo) == s_mtwo, equals(TRUE))
    expect_that(s_mone == switchEndianess(s_mone), equals(TRUE))
    expect_that(switchEndianess(s_zero )== s_zero, equals(TRUE))
    expect_that(s_one == switchEndianess(s_one), equals(TRUE))
    expect_that(switchEndianess(s_two) == s_two, equals(TRUE))
    expect_that(zero == switchEndianess(zero), equals(TRUE))
    expect_that(switchEndianess(one) == one, equals(TRUE))
    expect_that(two == switchEndianess(two), equals(TRUE))
})

context("Test '!=.binary'")


s_mtwo <- dec2bin(-2, signed=TRUE)
s_mone <- dec2bin(-1, signed=TRUE)
s_zero <- dec2bin(0, signed=TRUE)
s_one <- dec2bin(1, signed=TRUE)
s_two <- dec2bin(2, signed=TRUE)
zero <- dec2bin(0)
one <- dec2bin(1)
two <- dec2bin(2)

test_that("Return -", {
    expect_that(one != two, equals(TRUE))
    expect_that(s_mtwo != s_mtwo, equals(FALSE))
    expect_that(s_mone != s_mone, equals(FALSE))
    expect_that(s_zero != s_zero, equals(FALSE))
    expect_that(s_one != s_one, equals(FALSE))
    expect_that(s_two != s_two, equals(FALSE))
    expect_that(zero != zero, equals(FALSE))
    expect_that(one != one, equals(FALSE))
    expect_that(two != two, equals(FALSE))
    
    expect_that(switchEndianess(s_mtwo) != s_mtwo, equals(FALSE))
    expect_that(s_mone != switchEndianess(s_mone), equals(FALSE))
    expect_that(switchEndianess(s_zero )!= s_zero, equals(FALSE))
    expect_that(s_one != switchEndianess(s_one), equals(FALSE))
    expect_that(switchEndianess(s_two) != s_two, equals(FALSE))
    expect_that(zero != switchEndianess(zero), equals(FALSE))
    expect_that(switchEndianess(one) != one, equals(FALSE))
    expect_that(two != switchEndianess(two), equals(FALSE))
})

context("Test '[.binary'")

b <- binary(Byte())
sb <- binary(Byte(), signed=TRUE)
lb <- binary(Byte(), littleEndian=TRUE)

test_that("Lost Attributes", {
    expect_that(attr(b[length(b):1], "class"), equals(c("binary","logical")))
    expect_that(attr(b[length(b):1], "signed"), equals(FALSE))
    expect_that(attr(b[length(b):1], "littleEndian"), equals(FALSE))
    expect_that(attr(sb[length(sb):1], "signed"), equals(TRUE))
    expect_that(attr(lb[length(lb):1], "littleEndian"), equals(TRUE))
})

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
