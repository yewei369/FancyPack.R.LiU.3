context("euclidean")

test_that("Generate correct GCD", {
    expect_equal(euclidean(123612, 13892347912), 4)
    expect_equal(euclidean(100, 1000), 100)
    expect_equal(euclidean(4, 8), 4)
})


test_that("Generate errors at wrong inputs", {
    expect_error(euclidean("hundred", 1000))  
    expect_error(euclidean(100, TRUE))
})
