#' Filter iChart
#'
#' This function allows you to filter out bad trials from your iChart
#' @param iChart the iChart
#' @param minRT lower boundary of anlaysis window
#' @param maxRT upper boundary of anlaysis window
#' @param maxfirstgap max first gap length
#' @param maxlonggap max gap length
#' @export


filteriChart <- function(iChart, minRT, maxRT, maxfirstgap, maxlonggap) {

  high_percentileRT <- maxRT
  low_percentileRT <- minRT
  percentileFirstGap <- maxfirstgap
  percentileLongestGap <- maxlonggap

  save_as <- paste(iChart[1, "Directory"], iChart[1, "StudyName"], "_filtering_Criteria.txt", sep="")

  sink(paste(save_as, sep=""))


  goodTrials <- iChart$Response == "D" | iChart$Response == "T"
  T_Trials <- iChart$Response == "T"
  D_Trials <- iChart$Response == "D"

  # first Gap
  if(percentileFirstGap < 1) {
    longestFirstGap <- quantile(iChart$firstGap[goodTrials], percentileFirstGap, na.rm=T)
    print("")
    print(paste(percentileFirstGap, "quantile of First Gaps =", longestFirstGap))
  } else {
    longestFirstGap <- percentileFirstGap
  }

  print("First GAP")
  print(paste("Longest First Gap = ", longestFirstGap))
  print(summary(iChart$firstGap[goodTrials]))

  # longest Gap
  if(percentileLongestGap < 1) {
    longestLongestGap <- quantile(iChart$longestGap[goodTrials], percentileLongestGap, na.rm=T)
    print("")
    print(paste(percentileLongestGap, "quantile of Longest Gaps =", longestLongestGap))
  } else {
    longestLongestGap <- percentileLongestGap
  }

  print("Longest GAP")
  print(paste("Longest Gap = ", longestLongestGap))
  print(summary(iChart$longestGap[goodTrials]))

  # RTs

  if(low_percentileRT < 1) {
    shortestRT_D <- quantile(iChart$RT[D_Trials], low_percentileRT, na.rm=T)
    print(paste(low_percentileRT, "quantile of RTs (D) =", round(shortestRT_D)))

    shortestRT_T <- quantile(iChart$RT[T_Trials], low_percentileRT, na.rm=T)
    print(paste(low_percentileRT, "quantile of RTs (T) =", round(shortestRT_T)))

  } else {
    shortestRT_D <- low_percentileRT
    shortestRT_T <- low_percentileRT

  }

  if(high_percentileRT < 1) {
    longestRT_D <- quantile(iChart$RT[D_Trials], high_percentileRT, na.rm=T)
    print("")
    print(paste(high_percentileRT, "quantile of RTs (D) =", round(longestRT_D)))

    longestRT_T <- quantile(iChart$RT[T_Trials], high_percentileRT, na.rm=T)
    print("")
    print(paste(high_percentileRT, "quantile of RTs (T) =", round(longestRT_T)))

  } else {

    longestRT_D <- high_percentileRT
    longestRT_T <- high_percentileRT

  }

  print("RT D Trials (filtering based on these statistics)")
  print(paste("Longest RT D trials: ", longestRT_D))
  print(paste("Shortest RT D trials: ", shortestRT_D))
  print(summary(iChart$RT[D_Trials]))

  print("RT T Trials")
  print(paste("Longest RT T trials: ", longestRT_T))
  print(paste("Shortest RT T trials: ", shortestRT_T))
  print(summary(iChart$RT[T_Trials]))

  iChart$GoodFirstGap <- iChart$firstGap <= longestFirstGap

  print("Trials Rejected")
  print("")
  print(paste("First Gap = ", sum(iChart$GoodFirstGap,na.rm=T), " out of ", sum(iChart$GoodFirstGap,na.rm=T)+sum(!iChart$GoodFirstGap,na.rm=T), sep=""))


  iChart$GoodRT <- (iChart$RT <= longestRT_D)&(iChart$RT >= shortestRT_D)

  print("")
  print(paste("RT = ", sum(iChart$GoodRT,na.rm=T), " out of ", sum(iChart$GoodRT,na.rm=T)+sum(!iChart$GoodRT,na.rm=T),sep=""))


  iChart$GoodLongestGap <- iChart$longestGap <= longestLongestGap

  print("")
  print(paste("Longest Gap = ", sum(iChart$GoodLongestGap,na.rm=T), " out of ", sum(iChart$GoodLongestGap,na.rm=T)+sum(!iChart$GoodLongestGap, na.rm=T), sep=""))

  sink()


  npar <- length(unique(iChart$Sub.Num))
  save_as <- paste(iChart[1, "Directory"], iChart[1, "StudyName"], "_filterediChart", "_RT_",iChart[1, "StartWindowRT"], "_", iChart[1, "EndWindowRT"], "_minRT_",  shortestRT_D, "_maxRT_", longestRT_D, "_lg_", longestLongestGap, "_fg_", longestFirstGap, "_Acc_", iChart[1, "StartWindowAcc"], "_", iChart[1, "EndWindowAcc"], "_n_", npar, ".txt", sep="")


  write.table(iChart, save_as, sep="\t", row.names=F)


  return(iChart)

}
