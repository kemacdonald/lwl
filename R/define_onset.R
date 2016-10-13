#' Define critical onset in iChart
#'
#' This function allows you to set the critical onset for the analysis window in the iChart
#' @param iChart the iChart
#' @param critonset a number
#' @param includeAways logical
#' @export
#' @examples
#' defineOnset(iChart = "iChart", critonset = 300, includeAways=F)

defineOnset <- function(iChart, critonset = 300, includeAways=FALSE) {
  
  F0 <- critonset
  F0 <- which(names(iChart)==F0)
  iChart$Response <- ifelse(iChart[,F0] == "1", "T", ifelse(iChart[,F0] == "0", "D", "A"))
  
  
  if(includeAways) 	iChart$Response <- ifelse(iChart[,F0] == "1", "T", ifelse(iChart[,F0] == "0", "D", "T"))
  
  return(iChart)
  
}
