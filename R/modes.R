#' @title Calculating the Mode
#'
#' @description Determines which number(s) in the vector are the most frequently occurring.
#'
#' @param input a vector of numbers
#' @param order whether the modes should be sorted
#'
#' @return A vector of the most frequently occurring numbers
#'
#' @examples a <- c(1,2,3,3)
#' modes(a)
#'
#' b <- c(1,2,2,2,3,3,3,4,4)
#' modes(b)
#'
#' @export
modes <- function(input, order = TRUE) {

  # tabulate entries in the vector
  vect <- basictable(input, order)

  # entries
  entries <- as.double(colnames(vect))

  # determine maximum number of entries
  max.app <- max(vect)

  # find numbers that have maximum number of entries
  m <- rep(NA,1)
  for (i in 1:length(vect)) {
    if (vect[i] == max.app) {m <- c(m,entries[i])}
  }
  # break off the NA on the front of m
  m <- m[2:length(m)]

  return(m)
}
