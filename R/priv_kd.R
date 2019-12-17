#' K-Divergence Privacy Measure
#'
#' @param lpo,lpm original and modified load profiles
#' @param num.bins number of bins
#' @param bin.size size of bins
#' @param mode if "number", load profile is split into num.bins of equidistant bins;
#' if "size", bin.size is used for defining the size of each bin;
#' if "mulaw", mu-law discretization scheme is used.
#' @return K-Divergence value
#' @export
#' @examples
#' set.seed(1)
#' d <- squares(20)
#' lpo <- d[, 1]
#' lpm <- d[, 2]
#' priv.kd(lpo, lpm)

priv.kd <- function(lpo, lpm, num.bins = 20, bin.size = 0.01, mode = "number"){

  temp <- discr.together(lpo = lpo, lpm = lpm, num.bins = num.bins, bin.size = bin.size, mode = mode)
  levels <- sort(unique(as.numeric(temp)))
  lpo <- table(c(temp[, 1], levels)) - 1
  lpm <- table(c(temp[, 2], levels)) - 1
  lpo <- lpo/sum(lpo)
  lpm <- lpm/sum(lpm)
  
  res <- mapply(function(x, y){
    if(x != 0) x*log(2*x/(x + y)) 
    else 0
    }, x = lpo, y = lpm)
  
  sum(res)
}
