#' Synthetic data with known MI
#'
#' Generates the data from a set of squares with known theoretical value of Mutual Information
#' @param N number of points in the smallest square. The total number op points 
#' generated will be N*sum(sizes)*p
#' @param sizes a vector with side sizes of each square. The number of points generated
#' from each square is proportional to its side's size
#' @param p number of repetitions
#' @return 2D time series of length N*p*sum(sizes)
#' @export
#' 
#' @importFrom stats runif
#' 
#' @examples
#' set.seed(1)
#' d <- squares(30, c(2, 1), 3)
#' plot(d)
#' plot(ts(d[, 1]))
#' plot(d)

squares <- function(N, sizes = c(2, 1, 1), p = 1){
  print(paste0("the total number of points generated = ", N*p*sum(sizes)))
  print(paste0("true MI value = ", round(get.mi(sizes)/log(2), 4), " bits"))
  npts <- sizes*N*p
  len <- length(sizes)
  
  x <- runif(npts[1])*sizes[1]
  y <- runif(npts[1])*sizes[1]
  d <- cbind (x, y)
  ind <- rep(seq(1, len*p, len), each = sizes[1]*N)
  
  for(i in 2:len){
    x <- runif(npts[i])*sizes[i] + sum(sizes[1:(i - 1)])
    y <- runif(npts[i])*sizes[i] + sum(sizes[1:(i - 1)])
    d <- rbind(d, cbind (x, y))
    ind <- c(ind, rep(seq(i, len*p + i - 1, len), each = sizes[i]*N))
  }
  
d <- d[order(ind),]
d
}



get.mi <- function(sizes){
  pxy <- 1/(sizes*sum(sizes))
  pm <- 1/sum(sizes)
  vols <- sizes^2
  sum(vols*pxy*log(pxy/pm^2))
}

