context("median")

test_that("Median is the middle most", {
  expect_equal(med(c(1,9,4,5,7)),5)
  expect_equal(med(c(11,15,6,4,20,17)),13)
  expect_equal(med(c("carrot","apple","banana")),"banana")
})
