#' 
#' @title Generates a new column shifted up or down
#' @description This function shifts a vector up or down by a number of rows specified
#' @details This function is used when the analyst needs to generate a new column
#' by shifting an existing column up or down by a number of rows. This is similar to a
#' lag function in SAS. A negative integer shifts the column down, a positive number
#' shifts the column up.
#' @param xvect a character, the name of the vector to process.
#' @param shiftLen a positive or negative integer specifying the number of rows to
#' shift up or down by. 
#' @return a new vector with shifted values
#' @author Bishop, T.
#' @export
#' 
rowShiftDS <- function(xvect, shiftLen = 1L) {
  
  # generate a vector of integers that represent the indices that we want for
  # the shifted vector
  
  r <- (1L + shiftLen):(length(xvect) + shiftLen)
  
  # set negative indices to NA
  
  r[r<1] <- NA
  
  # use new index vector to return the shifted values
  
  return(xvect[r])
  
}