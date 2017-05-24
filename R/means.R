#' @title Calculating the Arithmetic Mean
#'
#' @description Calculates the arithmetic mean of a set of numbers. The function adds up
#' the numbers in the vector and divides by the number of elements in the vector.
#'
#' @return A number
#'
#' @examples
#' a <- c(-4,9,1,-6)
#' arithmeticmean(a)
#'
#' @export
arithmeticmean <- function(input) {
  # initialize the sum
  total <- 0

  # calculate the sum of all the numbers in the vector
  for (i in 1:length(input)) {
    total <- total + input[i]
  }

  # divide by the number of observations
  arithmetic <- total/length(input)

  return(arithmetic)
}

#' @title Calculating the Geometric Mean
#'
#' @description Calculates the geometric mean of a set of positive numbers. The function calculates
#' the product of every number in the vector and takes the nth root, where n is the number of
#' elements in the vector.
#'
#' @return A number
#'
#' @examples
#' a <- c(3,9,5,8)
#' geometricmean(a)
#'
#' @export
geometricmean <- function(input) {
  # initialize the product
  total <- 1

  # calculate the product of all the numbers in the vector
  for (i in 1:length(input)) {
    total <- total * input[i]
  }

  # take the nth root of the product
  geometric <- total^(1/length(input))

  return(geometric)
}
