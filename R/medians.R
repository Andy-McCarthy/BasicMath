#' @title Calculating the Median
#'
#' @description Calculates the median of a set of numbers. The function sorts the numbers in
#' the vector and selects the middle number (if the number of entries is odd) or takes the average
#' of the two middle numbers (if the number of entries is even)
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
med <- function(input) {

  # sort vector------------------------------------------------------------------
  sorted <- F
  while (sorted == F) {
    sorted <- T
    for (i in 1:(length(input)-1)) {
      for (j in (i+1):length(input)) {
        if (input[i] > input[j]) {
          holder <- input[i]
          input[i] <- input[j]
          input[j] <- holder
          sorted <- F
          break
          break
        }
      }
    }
  }

  # determine if there are an even number or odd number of observations----------
  if (length(input) %% 2 == 1) {
    # odd number of observations
    m <- input[length(input)/2 + 1/2]
  } else {
    # even number of observations
    m <- (input[length(input)/2] + input[length(input)/2 + 1])/2
  }

  return(m)
}
