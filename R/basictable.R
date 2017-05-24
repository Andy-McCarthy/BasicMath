#' @title Constructing a Frequency Table
#'
#' @description Constructs a table which lists each unique entry in a vector and how many times
#' it appears in the vector
#'
#' @param input a vector of numbers
#' @param order whether the modes should be sorted, with true being the default
#'
#' @return A vector
#'
#' @examples
#' a <- c(3,1,3,2,2,4,2,3,4)
#' basictable(a)
#'
#' b <- c(6,9,1,6,2,2)
#' basictable(b, order = FALSE)
#'
#' @export

basictable <- function(input, order = TRUE, na.rm = FALSE) {

  # store length of vector
  N <- length(input)

  # sort vector and decide whether to remove NA's
  if (order == TRUE) {
    if (na.rm == T) {last <- NA} else {last <- T}
    input <- sort(input, na.last = last)
  } else {
    if (na.rm == T) {
      i <- 1
      while (i <= length(input)) {
        if(is.na(input[i])) {input <- input[-i]} else {i <- i+1}
      }
    }
  }

  # find unique entries in the table
  values <- unique(input)

  # calculate number of each entry
  NAs <- F
  numEnt <- rep(0,length(values))
  for (i in 1:N) {
    for (j in 1:length(numEnt)) {
      if (is.na(input[i]) | is.na(values[j])) {NAs <- T} else {
        if (input[i] == values[j]) {
          numEnt[j] <- numEnt[j] + 1
        }
      }
    }
  }

  # create table of entries
  tabular <- rbind(numEnt)
  colnames(tabular) <- values
  row.names(tabular) <- NULL

  # fill in number of NAs
  for (k in 1:length(numEnt)) {
    if (tabular[k] == 0) {tabular[k] <- sum(is.na(input))}
  }

  return(tabular)
}
