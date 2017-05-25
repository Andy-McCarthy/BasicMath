#' @title Calculating the Median
#'
#' @description Calculates the median of a set of numbers. The function sorts the numbers in
#' the vector and selects the middle number (if the number of entries is odd) or takes the average
#' of the two middle numbers (if the number of entries is even).
#'
#' @param input a vector of numbers
#' @param na.rm true or false for whether to remove NA's before performing the calculation
#'
#' @return A number
#'
#' @examples
#' a <- c(5,9,4,1,11)
#' med(a)
#'
#' b <- c(5,9,4,1,11,2)
#' med(b)
#'
#' @export

med <- function(input, na.rm = FALSE) {

  # store length of input vector
  N <- length(input)

  # remove NAs
  NAs <- F
  to.keep <- rep(T,N)
  for (i in 1:N) {
    if (is.na(input[i])) {
      to.keep[i] <- F
      NAs <- T
      }
  }

  if (na.rm == T) {
    input <- input[to.keep]
    N <- length(input)
  }

  # sort vector
  input <- sort(input)

  # determine if there are an even number or odd number of observations
  if (N %% 2 == 1) {
    # odd number of observations
    m <- input[N/2 + 1/2]
  } else {
    # even number of observations
    m <- (input[N/2] + input[N/2 + 1])/2
  }

  # if there are NA's that aren't removed by the user
  if(NAs == T & na.rm == F) {m <- NA_real_}

  return(m)
}
