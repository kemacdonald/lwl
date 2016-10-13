#' Convert ms to frames in an iChart
#'
#' This function allows you to convert ms to frames for raw eye movement data from the looking-while-listening task
#' @param numeric 
#' @export
#' @examples
#' msToFrames(value)

msToFrames <- function(value) { return(value*3/100) }


#' Convert frames to ms in an iChart
#'
#' This function allows you to convert frames to ms for raw eye movement data from the looking-while-listening task
#' @param numeric 
#' @export
#' @examples
#' framesToMs(value)

framesToMs <- function(value) { return(value*100/3) }