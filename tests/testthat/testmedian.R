context("median")

test_that("Median is the middle most", {
  expect_equal(med(c(1,9,4,5,7)),5)
  expect_equal(med(c(11,15,6,4,20,17)),13)
  expect_equal(med(c("carrot","apple","banana")),"banana")
  expect_equal(med(c(7,NA,4,2,NA,11)),NA_real_)
  expect_equal(med(c(7,NA,4,2,NA,11),na.rm=T),5.5)
  expect_equal(med(c(8,7,NA,4,2,NA,11),na.rm=T),7)
})
