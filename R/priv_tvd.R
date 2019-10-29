#' Histogramm estimator for total variation distance
#' 
#' The function stimates total variation diatance bethween two distributions estimated
#' from time series data (load profiles). Preliminary quantizes their values. 
#' @param lpo,lpm original and modified load profiles
#' @param num.bins number of bins
#' @param bin.size size of bins
#' @param mode if "number", load profile is split into num.bins of equidistant bins;
#' if "size", bin.size is used for defining the size of each bin.
#' @return total variation distance
#' @references G. Kalogridis, R. Cepeda, S. Z. Denic, T. Lewis, and C. Efthymiou, 
#' "ElecPrivacy: Evaluating the privacy protection of electricity 
#' management algorithms," IEEE Transactions on Smart Grid, 
#' vol. 2, no. 4, pp. 750-758, 2011.
#' @export
#' @examples
#' set.seed(1)
#' d <- squares(10000)
#' priv.tvd(d[, 1], d[, 2])

priv.tvd <- function(lpo, lpm, num.bins = 20, bin.size = 0.01, mode = "number"){

  temp <- discr.together(lpo = lpo, lpm = lpm, num.bins = num.bins, bin.size = bin.size, mode = mode)
  
  levels <- sort(unique(as.numeric(temp)))
  lpo <- table(c(temp[, 1], levels)) - 1
  lpm <- table(c(temp[, 2], levels)) - 1
  
  sum(abs(lpo - lpm))/(2*sum(lpo))

}
