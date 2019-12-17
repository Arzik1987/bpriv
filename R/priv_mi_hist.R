#' 2D histogram
#'
#' The function omputes 2-dimensional historgamm and two respective one-dimensional ones
#' @param d matrix with two columns with quantized variables
#' @return data frame containing joint relative frequency in the first column and marginal 
#' relative frequencies in the second and in the third
#' @export
#' 
#' @importFrom stats aggregate
#' 
#' @examples
#' set.seed(1)
#' d <- squares(100)
#' hist2d(discr.together(d[, 1], d[, 2], mode = "number"))

hist2d <- function(d) {
  N <- nrow(d)
  d <- as.data.frame(d)
  d$FR <- as.numeric(1)
  
  frx <- aggregate(FR ~ d[, 1], d, sum)
  fry <- aggregate(FR ~ d[, 2], d, sum)
  d <- aggregate(FR ~ ., d, sum)
  
  names(frx) <- c("intsx", "countsx")
  names(fry) <- c("intsy", "countsy")
  names(d) <- c("intsx", "intsy", "counts")
  
  d <- merge(d, frx)
  d <- merge(d, fry)
  
  d[, (ncol(d) - 2):ncol(d)]/N
}


#' 4D histogram
#'
#' computes 4-dimensional historgamm and two two-dimensional histogramms
#' @param d matrix with four columns with quantized variables
#' @return data frame containing joint relative frequency in the first column and 
#' two-dimensional relative frequencies for
#' the columns 1:2 and 3:4 of d in the second and the third column correspondingly
#' @export
#' 
#' @importFrom stats aggregate
#' 
#' @examples
#' set.seed(1)
#' d <- cbind(squares(10000), squares(10000))
#' for(i in 1:ncol(d)){
#'   d[, i] <- discr.sep(d[, i], mode = "number")
#' }
#' hist4d(d)
#' mi.hist.freqs(hist4d(d))

hist4d <- function(d) {
  N <- nrow(d)
  d <- as.data.frame(d)
  names(d) <- c("x1", "x2", "y1", "y2")
  d$FR <- as.numeric(1)
  
  dx <- d[, c(1, 2, 5)]
  dy <- d[, c(3, 4, 5)]
  
  frx <- aggregate(FR ~ ., dx, sum)
  fry <- aggregate(FR ~ ., dy, sum)
  d <- aggregate(FR ~ ., d, sum)
  
  names(frx) <- c("x1", "x2", "countsx")
  names(fry) <- c("y1", "y2", "countsy")
  names(d) <- c("x1", "x2", "y1", "y2", "counts")
  
  d <- merge(d, frx)
  d <- merge(d, fry)
  
  d[, (ncol(d) - 2):ncol(d)]/N
}


#' Histogram MI estimator for i.i.d. assumption
#'
#' computes mutual information (in nats) provided a table of relative frequencies. 
#' Assumes that load profiles are i.i.d. samples of random variables
#' @param freqs table of relative frequencies
#' @references J. X. Chin, G. Giaconi, T. T. De Rubira, 
#' D. Gimduz, and G. Hug, "Considering time correlation in the estimation of privacy loss for 
#' consumers with smart meters," in 20th Power Systems Computation Conference, PSCC 2018.
#' @return mutual information value
#' @export
#' @examples
#' set.seed(1)
#' d <- squares(1000)
#' 
#' # if one quantizes jointly
#' d.together <- discr.together(d[, 1], d[, 2], mode = "number")
#' h <- hist2d(d.together)
#' mi.hist.freqs(h)
#' 
#' # if one quantizes separately
#' d.sep <- d
#' for(i in 1:ncol(d)){
#'   d.sep[, i] <- discr.sep(d[, i], mode = "number")
#' }
#' h <- hist2d(d.sep)
#' mi.hist.freqs(h)
#' 
#' # check if any of implementations coincides with the one from the existing package
#' entropy::mi.empirical(entropy::discretize2d(d[, 1], d[, 2], 20, 20))/log(2)

mi.hist.freqs <- function(freqs){
  mi <- 0
  for(i in 1:nrow(freqs)){
    mi <- mi - freqs[i, 1]*log(freqs[i, 2]*freqs[i, 3]/freqs[i, 1])
  }
  mi/log(2)
}


#' Histogram MI estimator for stationary Markov process assumption
#'
#' computes mutual information (in nats) provided a table of relative frequencies. 
#' Assumes that load profiles follow first order stationary Markow process
#' @param lpo,lpm discretized original and modified load profiles
#' @references J. X. Chin, G. Giaconi, T. T. De Rubira, 
#' D. Gimduz, and G. Hug, "Considering time correlation in the estimation of privacy loss for 
#' consumers with smart meters," in 20th Power Systems Computation Conference, PSCC 2018.
#' @return mutual information value
#' @export
#' @examples
#' 
#' set.seed(1)
#' d <- squares(10000)
#' lpo <- discr.sep(d[, 1], mode = "number")
#' lpm <- discr.sep(d[, 2], mode = "number")
#' mi.ms.hist(lpo, lpm)

mi.ms.hist <- function(lpo, lpm){
  N <- length(lpo)
  mi1 <- mi.hist.freqs(hist2d(cbind(lpo, lpm)))
  mi2 <- mi.hist.freqs(hist4d(cbind(shift(lpo, lpo, 1), shift(lpm, lpm, 1))))
  mi <- ((N - 1)*mi2 - (N - 2)*mi1)/N
  mi
}


#' Histogram MI estimator for non-stationary Markov processes
#'
#' computes mutual information (in nats) provided a table of relative frequencies. 
#' Assumes that load profiles follow non-stationary first order Markow process with features
#' @param lpo,lpm discretized original and modified load profiles
#' @param features the vector of feature values
#' #' @references J. X. Chin, G. Giaconi, T. T. De Rubira, 
#' D. Gimduz, and G. Hug, "Considering time correlation in the estimation of privacy loss for 
#' consumers with smart meters," in 20th Power Systems Computation Conference, PSCC 2018.
#' @return mutual information value
#' @export
#' @examples
#' set.seed(1)
#' d <- squares(250)
#' lpo <- discr.sep(d[, 1], mode = "number")
#' lpm <- discr.sep(d[, 2], mode = "number")
#' 
#' features <- c(rep(1, 500), rep(2, 250), rep(3, 250))
#' mi.mns.hist(lpo, lpm, features)
#' 
#' features <- rep(1:10, each = 100)
#' mi.mns.hist(lpo, lpm, features)
#' 
#' features <- rep(1:40, each = 25)
#' mi.mns.hist(lpo, lpm, features)
#' 
#' features <- rep(1:250, each = 4)
#' mi.mns.hist(lpo, lpm, features)

mi.mns.hist <- function(lpo, lpm, features){
  iters <- sort(unique(features))
  N <- length(iters)
  
  if(sum(iters == 1:N) < N) stop("The features vector should contain only integers. 
                                 And all integers from 1 to max(features)")
  
  if(N < 2) stop(paste0("The number of distinct feature values should be at least 3, currently it is: ", N))
  mi1 <- mi2 <- 0
  
  for(i in 2:(N - 1)){
    lpo.t <- lpo[features == i]
    lpm.t <- lpm[features == i]
    mi1 <- mi1 + mi.hist.freqs(hist2d(cbind(lpo.t, lpm.t)))
  }
  
  for(i in 2:N){
    d <- cbind(lpo, lpm)
    d1 <- d[features == (i - 1), ]
    d2 <- d[features == i, ]
    d <- rmix(d1, d2)
    mi2 <- mi2 + mi.hist.freqs(hist4d(d))
  }
  
  mi <- (mi2 - mi1)/N
  mi
}

#' Histogram MI estimator for different assumptions
#'
#' computes mutual information (in nats) between two load profiles (i.e., time series)
#' under various assumptions and variables transformations (= choices of binning schemes)
#' @param lpo,lpm original and modified load profiles
#' @param regime assumption on the type of process; "iid" - 
#' independently identically distributed, "ms" - first order Markov stationary,
#' "mns" - first order Markov non-stationary with features,
#' "bin" - the MI with i.i.d. assumption and two bins (num.bins = 2)
#' @param num.bins number of bins
#' @param bin.size size of bins
#' @param mode if "number", load profile is split into num.bins of equidistant bins;
#' if "size", bin.size is used for defining the size of each bin.
#' @param features features for non-stationary Markow process
#' @references J. X. Chin, G. Giaconi, T. T. De Rubira, 
#' D. Gimduz, and G. Hug, "Considering time correlation in the estimation of privacy loss for 
#' consumers with smart meters," in 20th Power Systems Computation Conference, PSCC 2018.
#' @return mutual information value
#' @export
#' @examples
#' set.seed(1)
#' d <- squares(1000)
#' lpo <- d[, 1]
#' lpm <- d[, 2]
#' priv.mi.hist(lpo, lpm, regime = "iid", num.bins = 20)
#' # binary version:
#' priv.mi.hist(lpo, lpm, regime = "iid", num.bins = 2)
#' priv.mi.hist(lpo, lpm, regime = "bin")
#' 
#' priv.mi.hist(lpo, lpm, regime = "ms", num.bins = 20)
#' 
#' set.seed(1)
#' d <- squares(250)
#' lpo <- d[, 1]
#' lpm <- d[, 2] 
#' features <- c(rep(1, 500), rep(2, 250), rep(3, 250))
#' priv.mi.hist(lpo, lpm, regime = "mns", features = features)

priv.mi.hist <- function(lpo, lpm, regime = c("iid", "ms", "mns", "bin"), 
                         num.bins = 20, bin.size = 0.01, mode = "number", features = NA){
  
  if(length(unique(lpm)) == 1) return(0)
  
  if(regime == "bin"){
    regime <- "iid"
    num.bins <- 2
    mode <- "number"
  }
  
  lpo <- discr.sep(lp = lpo, num.bins = num.bins, bin.size = bin.size, mode = mode)
  lpm <- discr.sep(lp = lpm, num.bins = num.bins, bin.size = bin.size, mode = mode)
  
  if(regime == "iid"){
    res <- mi.hist.freqs(hist2d(cbind(lpo, lpm)))
  } else if(regime == "ms"){
    res <- mi.ms.hist(lpo, lpm)
  } else if(regime == "mns"){
    res <- mi.mns.hist(lpo, lpm, features)
  }
  res
}




