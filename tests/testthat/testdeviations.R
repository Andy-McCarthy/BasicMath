context("sum of squares")

test_that("SST",{
  expect_equal(SST(c(2,8,8)),24)
})

context("standard deviation")

test_that("Standard Deviation is a measure of variability", {
  expect_equal(stdevs(c(1,2,3)),1)
  expect_equal(stdevp(c(1,2,3)),(2/3)^0.5)
  expect_equal(avgdist(c(1,2,3)),2/3)
})

test_that("Special Cases of St Dev",{
  expect_equal(avgdist(c(4,4,4,4)),0)
  expect_equal(stdevs(3),NaN)
  expect_equal(stdevp(3),0)
})
