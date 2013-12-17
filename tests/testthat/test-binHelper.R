context("Test binPrefix2Byte")

test_that("Return value", {
    expect_that(binPrefix2Byte(0.5, "KiB"), is_equivalent_to(512))
    expect_that(binPrefix2Byte(1, "KiB"), is_equivalent_to(2^10))
    expect_that(binPrefix2Byte(1, "MiB"), is_equivalent_to(2^20))
    expect_that(binPrefix2Byte(1, "GiB"), is_equivalent_to(2^30))
    expect_that(binPrefix2Byte(1, "TiB"), is_equivalent_to(2^40))
    expect_that(binPrefix2Byte(1, "PiB"), is_equivalent_to(2^50))
    expect_that(binPrefix2Byte(1, "EiB"), is_equivalent_to(2^60))
    expect_that(binPrefix2Byte(1, "ZiB"), is_equivalent_to(2^70))
    expect_that(binPrefix2Byte(1, "YiB"), is_equivalent_to(2^80))
})

#test_that("Warnings", {
#    expect_that(binPrefix2Byte(1,"LiB"), throws_error("Unknown binary prefix"))
#    expect_that(binPrefix2Byte(1.2,"KiB"), shows_message("ceiling called: returns the smallest integer not less than the corresponding element of ret"))
#})


context("Test bytesNeeded")
cat("no test available")

context("Test Byte")

test_that("Return value", {
    expect_that(Byte(), is_identical_to(8))
})
