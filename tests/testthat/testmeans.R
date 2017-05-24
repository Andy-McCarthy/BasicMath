context("means")

test_that("arithmeticmean is the average", {
  expect_equal(arithmeticmean(c(2,9,4,-7,-5,-2)),1/6)
  expect_equal(arithmeticmean(c(NA,4,-5,11,NA,-4)),NA_real_)
  expect_equal(arithmeticmean(c(NA,4,-5,11,NA,-4), na.rm = T),1.5)
  expect_equal(arithmeticmean(3),3)
})

test_that("geometricmean", {
  expect_equal(geometricmean(c(2,9,3,1)),54^0.25)
  expect_equal(geometricmean(3),3)
  expect_equal(geometricmean(c(NA,2,5,2,5,NA)),NA_real_)
  expect_equal(geometricmean(c(NA,2,5,2,5,NA),na.rm=T),100^0.25)
  expect_warning(geometricmean(c(-1,3,5,6)))
})
