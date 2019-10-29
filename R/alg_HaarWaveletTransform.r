#' Compress lpo using Haar-wavelet transform.
#'
#' The time series is transformed using Haar-wavelet transform, 
#' then the near-zero-values will be rounded to zero to an extend 
#' and the series will be decoded, creating a new, compressed 
#' version of the original series.
#' @param lpo Times series as numeric vector
#' @param level Compression level, from 0 to 1
#' @export
#' 
#' @importFrom stats ts quantile
#' 
#' @examples 
#' set.seed(1)
#' d <- squares(100)
#' lpo <- d[, 1]
#' lpm <- compress.lpo(lpo, 0.9)
#' plot(ts(lpo))
#' lines(ts(lpm), col = "red")

compress.lpo <- function(lpo, level = 0.99) {
  
  l <- length(lpo)
  lpo <- ts(c(lpo, rep(0, 2^ceiling(log(l, base = 2)) - l)))
  trans <- wavelets::dwt(lpo, filter="haar")
  w.vector.length <- l / 2
  w.vector <- numeric()
  for (i in 1:trans@level) {
    w.vector <- append(abs(trans@W[i][[1]]), w.vector)
  }
  limit <- quantile(w.vector, level)
  
  for (i in 1:trans@level) {
    trans@W[i][[1]][abs(trans@W[i][[1]]) <= limit] <- 0
  }
  
  res <- wavelets::idwt(trans)
  res <- res[1:l]
  return (res)
}
