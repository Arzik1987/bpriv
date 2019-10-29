#' Shifts one TS relative to another TS
#'
#' Shifts one time series relative to another time series given
#' @param ts1,ts2 time series of the same length
#' @param n positions to shift
#' @keywords shift
#' @return matrix of size length(ts1) - n, whete ts1 is in the first column and ts2 dalayed n steps
#' is in the second. n rows are removed to exclude missing values
#' @export
#' @examples
#' set.seed(1)
#' x <- squares(3)[, 1]
#' shift(x, x, 1)


shift <- function(ts1, ts2, n){
  if(length(ts1) != length(ts2)) stop("TS are of different lengths")
  lts <- length(ts1)
  if(n > 0) ts1 <- ts1[(n + 1):lts] else ts1 <- ts1[1:(lts + n)]
  if(n > 0) ts2 <- ts2[1:(lts - n)] else ts2 <- ts2[(-n + 1):lts]
  cbind(ts1, ts2)
}
