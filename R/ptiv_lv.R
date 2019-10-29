#' Load variance
#'
#' The function calculates the value for the "load variance" privacy measure
#' @param lpo,lpm original and modified load profiles
#' @param level if some specific level should be used instead of mean(lpo)
#' @return value of load variance
#' @references Tan, Onur, Jesus Gomez-Vilardebo, and Deniz Gunduz. 
#' "Privacy-cost trade-offs in demand-side management with storage." 
#' IEEE Transactions on Information Forensics and Security 12.6 (2017)
#' @export
#' @examples
#' d <- squares(1000)
#' lpo <- d[, 1]
#' lpm <- d[, 2]
#' priv.lv(lpo, lpm, level = NULL)
#' 
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' priv.lv(lpo, lpm)


priv.lv <- function(lpo, lpm, level = NULL){
  if(is.null(level)) level <- mean(lpo)
  sum((lpm - level)^2)/length(lpm)
}
