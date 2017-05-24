context("sum of squares")

test_that("SST",{
  expect_equal(SST(c(2,8,8)),24)
  expect_equal(SST(c(NA,4,2,1,1)),NA_real_)
  expect_equal(SST(c(NA,4,2,1,1),na.rm = T),6)
})

context("standard deviation")

test_that("Standard Deviation is a measure of variability", {
  expect_equal(stdeviation(c(1,2,3)),1)
  expect_equal(stdeviation(c(1,2,3), sample=F),(2/3)^0.5)
  expect_equal(avgdist(c(1,2,3)),2/3)
})

test_that("Special Cases of St Dev",{
  expect_equal(stdeviation(3),NaN)
  expect_equal(stdeviation(3, sample=F),0)
  expect_equal(avgdist(c(4,4,4,4)),0)
})
