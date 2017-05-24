#-------------------------------SST-----------------------------------------

#' @title Calculate the Total Sum of Squares
#'
#' @description Calculate the total sum of squares (SST) by subtracting the mean from each
#' number in the data set, squaring each re-centered observation, and then summing up the results.
#'
#' @param input a vector of numbers
#'
#' @return A number
#'
#' @examples a <- c(4,12,8,7)
#' SST(a)
#'
#' @export
SST <- function(input) {

  # calculate the mean of the vector
  amean <- arithmeticmean(input)

  # sum of squared errors
  total <- 0
  for (i in 1:length(input)) {
    total <- total + (input[i] - amean)^2
  }

  return(total)
}

#---------------------------stdevs-----------------------------------------

#' @title Calculating the Sample Standard Deviation
#'
#' @description Calculates the sample standard deviation, using n - 1 degrees of freedom.
#'
#' @param input a vector of numbers
#'
#' @return A number
#'
#' @examples a <- c(2,4,6,8)
#' stdevs(a)
#'
#' @export
stdevs <- function(input) {

  # calculate SST
  total <- SST(input)

  # divide by degrees of freedom
  variance <- total/(length(input) - 1)

  # calculate standard deviation
  stdeviation <- variance^0.5

  return(stdeviation)
}

#----------------------stdevp----------------------------------------------

#' @title Calculating the Population Standard Deviation
#'
#' @description If you have the entire population in your data set, instead of dividing the SST
#' by n-1, one divides by n before taking the square root.
#'
#' @param input a vector of numbers
#'
#' @return A number
#'
#' @examples a <- c(2,4,6,8)
#' stdevp(a)
#'
#' @export
stdevp <- function(input) {

  # calculate SST
  total <- SST(input)

  # divide by number of observations
  variance <- total/(length(input))

  # calculate standard deviation
  stdeviation <- variance^0.5

  return(stdeviation)
}

#-------------------------avgdist------------------------------------------

#' @title Calculating Average Distance from the Mean
#'
#' @description Calculates the average distance from the mean by converting each number in the
#' vector to an absolute value and then taking the arithmetic mean.
#'
#' @param input a vector of numbers
#'
#' @return A number
#'
#' @examples a <- c(5,7,1,-3,-9)
#' avgdist(a)
#'
#' @export
avgdist <- function(input) {

  # calculate absolute distances from the mean
  new.input <- abs(input - arithmeticmean(input))

  # find the mean of the absolute distances
  ad <- arithmeticmean(new.input)

  return(ad)
}
