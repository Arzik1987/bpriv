#' Conditional entropy
#'
#' The function estimates the entropy of user load conditioned on the grid load. 
#' Preliminary quantizes their values. 
#' @param lpo,lpm original and modified load profiles
#' @param num.bins number of bins
#' @param bin.size size of bins
#' @param mode if "number", load profile is split into num.bins of equidistant bins;
#' if "size", bin.size is used for defining the size of each bin.
#' @return privacy measure: conditional entropy
#' @export
#' @references Yao, Jiyun, and Parv Venkitasubramaniam. "The privacy analysis 
#' of battery control mechanisms in demand response: Revealing state approach 
#' and rate distortion bounds." IEEE Transactions on Smart Grid 6.5 
#' (2015): 2417-2425.
#' @examples
#' set.seed(1)
#' d <- squares(1000)
#' lpo <- d[, 1]
#' lpm <- d[, 2]
#' priv.ce(lpo, lpm)
#' 
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' priv.ce(lpo, lpm)

priv.ce <- function(lpo, lpm, num.bins = 20, bin.size = 0.01, mode = "number"){
  
  lpo <- discr.sep(lpo, num.bins = num.bins, bin.size = bin.size, mode = mode)
  if(length(unique(lpm)) != 1){
    lpm <- discr.sep(lpm, num.bins = num.bins, bin.size = bin.size, mode = mode)
  } else {
    lpm <- rep(0, length(lpm))
  }
  
  infotheo::condentropy(X = lpo, Y = lpm)
  #/infotheo::entropy(X = lpo)
}

