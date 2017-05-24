outcome <- t(c(3,1,1,2))
colnames(outcome) <- c("1","2","3","4")

outcome2 <- t(c(1,1,3))
colnames(outcome2) <- c("a","b","c")

context("basictable")

test_that("basictable returns a table",{
  expect_equal(basictable(c(1,1,1,3,4,4,2)),outcome)
  expect_equal(basictable(c("a","b","c","c","c")), outcome2)
})
