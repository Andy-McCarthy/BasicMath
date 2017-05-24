context("means")

test_that("arithmeticmean is the average", {
  expect_equal(arithmeticmean(c(2,9,4,-7,-5,-2)),1/6)
  expect_equal(arithmeticmean(1:8),4.5)
  expect_equal(arithmeticmean(3),3)
})

test_that("geometricmean", {
  expect_equal(geometricmean(c(2,9,3,1)),54^0.25)
  expect_equal(geometricmean(3),3)
  expect_equal(geometricmean(c(-1,3,5,6)),NaN)
})
