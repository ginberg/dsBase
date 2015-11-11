#' @title Calculates the difference between rows for a target column in grouped data
#' @description This function calculates the difference between rows for a target 
#' column in grouped data. For non-grouped data it would be possible to use rowShift
#' @details This function is used when the analyst needs to calculate the difference
#' between rows for a column. The data should be grouped by an identifying column:
#'  for non-grouped data it is possible to use the rowShift function and a subtraction
#'  to perform the same operation 
#' @param input a character, the name of the data frame to process
#' @param id_col a character, the name of the field in the data frame that groups
#'  the data
#' @param target_col a character, the name of the field in the data frame
#' @return a new vector containing the deltas
#' @author Bishop, T.
#' @export
#' 

deltaDS <- function(input,id_col,target_col){  
  
  output  <- lapply(split(input, input[[id_col]]), function(x){
    
    size<-nrow(x)
    delta <- numeric(size)
    for (i in 1:size){
      
      if(i==1){
        delta[i] <- 0
      }
      else{
        delta[i] <- x[[target_col]][i]-x[[target_col]][i-1]
      }
      
    }
    return(delta)
  })
  
  new_delta <- unlist(output,use.names=F)
  
  return(new_delta)
}