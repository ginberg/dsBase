#'
#' @title Returns unique rows of a data frame based on selected columns
#' @description Returns unique rows of a data frame based on selected columns
#' @details Needs checking to see if it will work with a matrix. All columns are returne
#' For columns outside the selection, the first value is taken
#' @param x a data frame
#' @param f a vector of column names for checking uniqueness
#' @return all columns, but only rows unique in the selected columns
#' @author Bishop, T.
#' @export
#'


uniqueDS <- function(x,f){
  
  output <- subset(x, !duplicated(x[,f]))
  return(output)
  
}