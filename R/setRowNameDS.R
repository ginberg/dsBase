#'
#' @title Changes the names of rows of a data frame
#' @description Changes the names of rows of a data frame
#' @details Needs sorting out so that there is a function to set the name and a function
#' to get the name
#' @param x a character corresponding the name of a data frame on the server side
#' @param names a vector of characters equal in length to the number of rows in x
#' @return a numeric, the statistical mean
#' @author Bishop, T.
#' @export
#'

setRowNamesDS <- function(x,names){
  
  rownames(x) <- names
  output <- x
  return(output)
  
}