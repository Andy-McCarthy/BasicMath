outcome <- t(c(3,1,1,2))
colnames(outcome) <- c("1","2","3","4")

outcome2 <- t(c(1,1,3))
colnames(outcome2) <- c("a","b","c")

outcome3 <- t(c(1,2,1,3))
colnames(outcome3) <- c("1","4","8",NA)

outcome4 <- t(c(2,1,3,1))
colnames(outcome4) <- c("4","1",NA,"8")

outcome5 <- t(c(1,2,1))
colnames(outcome5) <- c("1","4","8")

outcome6 <- t(c(2,1,1))
colnames(outcome6) <- c("4","1","8")

context("basictable")

test_that("basictable returns a table",{
  expect_equal(basictable(c(1,1,1,3,4,4,2)),outcome)
  expect_equal(basictable(c("a","b","c","c","c")), outcome2)
  expect_equal(basictable(c(4,1,NA,8,NA,4,NA),order=T,na.rm = F),outcome3)
  expect_equal(basictable(c(4,1,NA,8,NA,4,NA),order=F,na.rm = F),outcome4)
  expect_equal(basictable(c(4,1,NA,8,NA,4,NA),order=T,na.rm = T),outcome5)
  expect_equal(basictable(c(4,1,NA,8,NA,4,NA),order=F,na.rm = T),outcome6)
})
