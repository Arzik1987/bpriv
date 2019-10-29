#' Histogram estimator for KL divergence
#' 
#' The function estimates Kullback-Leibler divergence bethween two distributions estimated
#' from time series data (load profiles). Preliminary quantizes their values.
#' @param lpo,lpm original and modified load profiles
#' @param num.bins number of bins
#' @param bin.size size of bins
#' @param mode if "number", load profile is split into num.bins of equidistant bins;
#' if "size", bin.size is used for defining the size of each bin.
#' @return KL divergence estimate
#' @references Kalogridis, Georgios, et al. 
#' "Privacy for smart meters: Towards undetectable appliance load signatures." 
#' 2010 First IEEE International Conference on Smart Grid Communications. 
#' IEEE, 2010.
#' @export
#' @examples
#' set.seed(1)
#' d <- squares(1000)
#' lpo <- d[, 1]
#' lpm <- d[, 2]
#' priv.kl.hist(lpo, lpm)
#' 
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' priv.kl.hist(lpo, lpm, num.bins = 2, mode = "number")

priv.kl.hist <- function(lpo, lpm, num.bins = 20, bin.size = 0.01, mode = "number"){
  
  temp <- discr.together(lpo = lpo, lpm = lpm, num.bins = num.bins, bin.size = bin.size, mode = mode)
  levels <- sort(unique(as.numeric(temp)))
  lpo <- table(c(temp[, 1], levels)) - 1
  lpm <- table(c(temp[, 2], levels)) - 1

  entropy::KL.plugin(lpo, lpm)
}
