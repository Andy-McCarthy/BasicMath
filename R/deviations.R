#-------------------------------SST-----------------------------------------

#' @title Calculate the Total Sum of Squares
#'
#' @description Calculate the total sum of squares (SST) by subtracting the mean from each
#' number in the data set, squaring each re-centered observation, and then summing up the results.
#'
#' @param input a vector of numbers
#' @param na.rm true or false for removing NA's before the calculation is performed
#'
#' @return A number
#'
#' @examples a <- c(4,12,8,7)
#' SST(a)
#'
#' b <- c(4,NA,12,NA,7)
#' SST(b)
#'
#' d <- c(4,NA,12,8,NA,7)
#' SST(d,na.rm=T)
#'
#' @export

SST <- function(input, na.rm = FALSE) {

  # calculate the mean of the vector
  amean <- arithmeticmean(input, na.rm)

  # sum of squared errors
  total <- 0
  if (na.rm == T) {
    for (i in 1:length(input)) {
      # If NA's are removed
      if (!is.na(input[i])) {
      total <- total + (input[i] - amean)^2
      }
    }
  } else {
    # If NA's are not removed
    for (i in 1:length(input)) {
      total <- total + (input[i] - amean)^2
    }
  }

  return(total)
}

#---------------------------stdevs-----------------------------------------

#' @title Calculating the Standard Deviation
#'
#' @description Calculates the standard deviation. By selecting sample = TRUE, the user chooses
#' to calculate the sample standard deviation, where the sum of square is divided by n-1. By
#' selecting sample = FALSE, the calculation is treated as a population standard deviation, and
#' the sum of squares is divided by n.
#'
#' @param input a vector of numbers
#' @param na.rm true or false for removing NAs from the vector before calculations
#' @param sample true or false where true represents a sample standard deviation and false
#' represents a population standard deviation
#'
#' @return A number
#'
#' @examples a <- c(2,4,6,8)
#' stdeviation(a)
#'
#' b <- c(NA,2,4,NA,8)
#' stdeviation(b)
#'
#' d <- c(NA,2,4,NA,8)
#' stdeviation(d,na.rm=T)
#'
#' e <- c(NA,2,4,8,NA)
#' stdeviation(e,na.rm=T,sample=F)
#'
#' @export

stdeviation <- function(input, na.rm = FALSE, sample = TRUE) {

  # store length of vector
  N <- length(input)

  # calculate SST
  total <- SST(input, na.rm)

  # change degrees of freedom if NA's removed
  if (na.rm == TRUE) {
    N <- N - sum(is.na(input))
  }

  if (sample == T) {
    # sample standard deviation
    variance <- total/(N - 1)
  } else {
    # population standard deviation
    variance <- total/N
  }

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
#' @param na.rm true or false for whether to remove NA's before performing the calculation
#'
#' @return A number
#'
#' @examples a <- c(5,7,1,-3,-9)
#' avgdist(a)
#'
#' b <- c(NA,4,7,-2,-12)
#' avgdist(b)
#'
#' d <- c(NA,4,NA,-2,-12)
#' avgdist(d,na.rm=T)
#'
#' @export

avgdist <- function(input, na.rm = FALSE) {

  # calculate absolute distances from the mean
  new.input <- abs(input - arithmeticmean(input, na.rm))

  # find the mean of the absolute distances
  ad <- arithmeticmean(new.input, na.rm)

  return(ad)
}
