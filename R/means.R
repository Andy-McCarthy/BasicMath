#' @title Calculating the Arithmetic Mean
#'
#' @description Calculates the arithmetic mean of a set of numbers. The function adds up
#' the numbers in the vector and divides by the number of elements in the vector.
#'
#' @param input a vector of numbers
#' @param na.rm true or false for whether to remove NA's before performing the calculation
#'
#' @return A number
#'
#' @examples
#' a <- c(-4,9,1,-6)
#' arithmeticmean(a)
#'
#' b <- c(-4,NA,NA,9,1,-6)
#' arithmeticmean(b)
#'
#' d <- c(-4,NA,NA,9,1,-6)
#' arithmeticmean(d,na.rm=T)
#'
#' @export

arithmeticmean <- function(input, na.rm = FALSE) {

  # initialize the sum
  total <- 0

  # store the length of the vector input
  N <- length(input)

  # calculate the sum of all the numbers in the vector
  if (na.rm == TRUE) {
    # if NA's should be removed
    for (i in 1:length(input)) {
      if (!is.na(input[i])) {
        total <- total + input[i]
      } else {
        # if there is an NA, lessen the number of observations by 1
        N <- N-1
      }
    }
  } else {
    # if NA's should not be removed
    for (i in 1:length(input)) {
        total <- total + input[i]
    }
  }

  # divide by the number of observations
  arithmetic <- total/N

  return(arithmetic)
}

#' @title Calculating the Geometric Mean
#'
#' @description Calculates the geometric mean of a set of positive numbers. The function calculates
#' the product of every number in the vector and takes the nth root, where n is the number of
#' elements in the vector.
#'
#' @param input a vector of numbers
#' @param na.rm true or false for whether to remove NA's before performing the calculation
#'
#' @return A number
#'
#' @examples
#' a <- c(3,9,5,8)
#' geometricmean(a)
#'
#' b <- c(NA,3,NA,7,9,2)
#' geometricmean(b)
#'
#' d <- c(NA,3,NA,7,9,2)
#' geometricmean(d,na.rm=T)
#'
#' @export

geometricmean <- function(input, na.rm = FALSE) {

  # deliver a warning if there are negative numbers or zeros in the vector
  i <- 1
  good <- TRUE
  while (good == TRUE) {
    # check for negatives or zeros
    if (!is.na(input[i])) {
      if (input[i] <= 0) {
        good <- FALSE
        warning("geometricmean should not be used with zeros or negative numbers")
      }
    }
    if (i == length(input)) {good <- FALSE}

    #iterate counter
    i <- i + 1
  }

  # initialize the product
  total <- 1

  # store the length of the vector input
  N <- length(input)

  # calculate the product of all the numbers in the vector
  if (na.rm == TRUE) {
    for (i in 1:length(input)) {
      # if NA's are to be removed
      if (!is.na(input[i])) {
      total <- total * input[i]
      } else {
        # if there is an NA, lessen the number of observations by 1
        N <- N-1
      }
    }
  } else {
    # if NA's are not to be removed
    for (i in 1:N) {
      total <- total * input[i]
    }
  }

  # deliver a warning if every entry in the vector is an NA
  if (N == 0) {
    warning("There are no usable data points in the input vector")
  }

  # take the nth root of the product
  geometric <- total^(1/N)

  return(geometric)
}
