context("Test binaryPrefix")

test_that("Return value", {
    expect_that(binaryPrefix(0.5, "KiB"), is_equivalent_to(512))
    expect_that(binaryPrefix(1, "KiB"), is_equivalent_to(2^10))
    expect_that(binaryPrefix(1, "MiB"), is_equivalent_to(2^20))
    expect_that(binaryPrefix(1, "GiB"), is_equivalent_to(2^30))
    expect_that(binaryPrefix(1, "TiB"), is_equivalent_to(2^40))
    expect_that(binaryPrefix(1, "PiB"), is_equivalent_to(2^50))
    expect_that(binaryPrefix(1, "EiB"), is_equivalent_to(2^60))
    expect_that(binaryPrefix(1, "ZiB"), is_equivalent_to(2^70))
    expect_that(binaryPrefix(1, "YiB"), is_equivalent_to(2^80))
})

context("Test bytesNeeded")

context("Test byte")

test_that("Return value", {
    expect_that(byte(), is_identical_to(8))
})
