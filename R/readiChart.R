#' read iChart
#'
#' This function allows you to read in raw eye movement data from the looking-while-listening task
#' @param iChartFile 
#' @keywords iChart
#' @export
#' @examples
#' readiChart(iChartFile = "iChart")

readiChart <- function(iChartFile) {
  
  #temp <- gregexpr("/", iChartFile)
  
  #last_separator <- temp[[1]][length(temp[[1]])]
  
  StudyName <- substr(basename(iChartFile), 1, nchar(iChartFile)-4)
  
  Directory <- paste(dirname(iChartFile),"/" , sep="")	
  
  iChart <- read.table(iChartFile, header=T, fill=T, as.is=T, sep="\t")
  
  iChart$RTOld <- iChart$RT
  
  startWindow <- match("F0", colnames(iChart))
  if(is.na(startWindow)) startWindow <- match("Word.Onset.Frame", colnames(iChart))
  
  # rename frame columns
  colnames(iChart)[startWindow:length(iChart)] <- round(framesToMs(round(0:(length(iChart)-startWindow))))
  
  
  # the original file had one header per subject
  # this line removes all rows that are the same as the header
  # and selects only the columns of interest
  iChart <- iChart[iChart[,"Months"]!="Months",]
  
  
  iChart$Block <- rep(NA, nrow(iChart))
  iChart$Offset <- rep(0, nrow(iChart))
  iChart$StartWindowRT <- rep(NA, nrow(iChart))
  iChart$EndWindowRT <- rep(NA, nrow(iChart))
  iChart$StartWindowAcc <- rep(NA, nrow(iChart))
  iChart$EndWindowAcc <- rep(NA, nrow(iChart))
  iChart$StudyName <- rep(StudyName, nrow(iChart))
  iChart$Directory <- rep(Directory, nrow(iChart))
  
  iChart <- iChart[iChart$Condition != "",]
  
  return(iChart)
  
}