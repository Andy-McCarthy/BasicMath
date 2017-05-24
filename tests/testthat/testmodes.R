context("mode")

test_that("Mode is the most frequently occurring",{
  expect_equal(modes(c(3,9,6,6,7,4,3,9,6)),6)
  expect_equal(modes(c(3,3,3,3)),3)
  expect_equal(modes(c(3,3,6,6,9,9,4)),c(3,6,9))
  expect_warning(modes(c("g","g","h")))
})
