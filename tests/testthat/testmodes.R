context("mode")

test_that("Mode is the most frequently occurring",{
  expect_equal(modes(c(3,9,6,6,7,4,3,9,6)),6)
  expect_equal(modes(c(3,3,3,3)),3)
  expect_equal(modes(c(3,3,6,6,9,9,4)),c(3,6,9))
  expect_warning(modes(c("g","g","h")))
  expect_equal(modes(c(NA,NA,NA,3,5,8,3,8),order = T,na.rm = T),c(3,8))
  expect_equal(modes(c(NA,NA,NA,8,3,5,8,3),order = F,na.rm = T),c(8,3))
  expect_equivalent(modes(c(NA,NA,NA,3,5,8,3,8),order = T,na.rm = F),NA_real_)
  expect_equivalent(modes(c(NA,NA,NA,3,5,8,3,8),order = F,na.rm = F),NA_real_)
  expect_equal(modes(c(5,2,1,3,4,6,NA,NA),order = T,na.rm = T),c(1,2,3,4,5,6))
})
