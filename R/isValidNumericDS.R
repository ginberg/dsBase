#'
#' @title Checks if an input variable is valid
#' @description Tells if an object on the server side is valid.
#' @details This function checks if an object is valid. The object is considered as valid if has more
#' than two unique values (excluding missing values) or if has only two unique values but the counts
#' of each unique category is more than the protection filter set for table cell counts. 
#' @param obj, a vector of a variable
#' @return a binary value, 0 if input valid is valid or 1 if not.
#' @author Burton, P., Avraam, D.,
#' @export
#'
isValidNumericDS <- function(obj){

  unique.values.noNA <- unique(obj[complete.cases(obj)])
  valid <- 0

  if(length(unique.values.noNA)==2){
    tabvar <- table(obj)[table(obj)>=1]
    min.category <- min(tabvar)
    nf.tab <- getOption("nfilter.tab")
    if(min.category < nf.tab){
      valid <- 1
    }
  }
 
return(valid)

}

