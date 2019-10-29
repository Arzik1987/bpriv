
#' kNN MI estimator for stationary Markov processes
#'
#' computes mutual information between two load profiles (i.e., time series)
#' under the assumtion of the first-order Markov process
#' @param lpo,lpm original and modified load profiles
#' @param k number of nearest neighbor to consider
#' @return mutual information value
#' @export
#' 
#' @importFrom stats runif
#' 
#' @examples
#' set.seed(1)
#' d <- squares(1000)
#' lpo <- d[, 1]
#' lpm <- d[, 2]
#' mi.ms.knn(lpo, lpm, 2)
#' 
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' mi.ms.knn(lpo, lpm, k = 1)
#' mi.ms.knn(lpo, lpm, k = 4)


mi.ms.knn <- function(lpo, lpm, k){
  N <- length(lpo)
  
  mi1 <- FNN::mutinfo(lpo, lpm, k)
  mi2 <- FNN::mutinfo(shift(lpo, lpo, 1), shift(lpm, lpm, 1), k)
  mi <- ((N - 1)*mi2 - (N - 2)*mi1)/N
  mi/log(2)
}

#' kNN MI estimator for non-stationary Markov processes
#'
#' computes mutual information between two load profiles (i.e., time series)
#' under the assumtion of the non-stationary Markov
#' process with features
#' @param lpo,lpm original and modified load profiles
#' @param k number of nearest neighbor to consider
#' @param features vector of feature values (integers)
#' @return mutual information value
#' @export
#' @examples
#' set.seed(1)
#' d <- squares(25)
#' features <- rep(1:10, each = 10)
#' mi.mns.knn(d[, 1], d[, 2], k = 2, features)
#' features <- rep(1:4, each = 25)
#' mi.mns.knn(d[, 1], d[, 2], k = 2, features)
#' 
#' features <- rep(1:4, each = 4)
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' mi.mns.knn(lpo, lpm, k = 1, features)
#' mi.mns.knn(lpo, lpm, k = 2, features)


mi.mns.knn <- function(lpo, lpm, k, features){
  iters <- sort(unique(features))
  N <- length(iters)
  
  if(sum(iters == 1:N) < N) stop("The features vector should contain only integers. 
                                 And all integers from 1 to max(features)")
  
  mi1 <- mi2 <- 0
  
  for(i in 2:(N - 1)){
    lpo.t <- lpo[features == i]
    lpm.t <- lpm[features == i]
    mi1 <- mi1 + FNN::mutinfo(lpo.t, lpm.t, k)
  }
  
  for(i in 2:N){
    d <- cbind(lpo, lpm)
    d1 <- d[features == (i - 1), ]
    d2 <- d[features == i, ]
    d <- rmix(d1, d2)
    mi2 <- mi2 + FNN::mutinfo(d[, 1:2], d[, 3:4], k)
  }
  
  mi <- (mi2 - mi1)/N
  mi/log(2)
}

#' kNN MI estimator for different assumptions
#'
#' computes mutual information between two load profiles (i.e., time series)
#' under various assumptions and variables transformations
#' @param lpo,lpm original and modified load profiles
#' @param regime assumption on the type of process; "iid" - 
#' independently identically distributed, "ms" - first order Markov stationary,
#' "mns" - first order Markov non-stationary with features
#' @param k number of nearest neighbor to consider
#' @param features features for non-stationary Markow process
#' @param noise the amplitude of the noise to add in case of duplicated points
#' @return mutual information value
#' @export
#' @references J. X. Chin, G. Giaconi, T. T. De Rubira, D. Gimduz, and G. Hug, 
#' "Considering time correlation in the estimation of privacy loss for 
#' consumers with smart meters," in 20th Power Systems Computation 
#' Conference, PSCC 2018, 2018.
#' @examples
#' set.seed(1)
#' d <- squares(100)
#' features <- rep(1:10, each = 40)
#' priv.mi.knn(d[, 1], d[, 2], regime = "iid", k = 2, features = features)
#' priv.mi.knn(d[, 1], d[, 2], regime = "ms", k = 2, features = features)
#' priv.mi.knn(d[, 1], d[, 2], regime = "mns", k = 2, features = features) 
#' 
#' features <- rep(1:4, each = 4)
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' priv.mi.knn(lpo, lpm, regime = "iid", k = 2, features = features)
#' priv.mi.knn(lpo, lpm, regime = "ms", k = 2, features = features)
#' priv.mi.knn(lpo, lpm, regime = "mns", k = 2, features = features) 


priv.mi.knn <- function(lpo, lpm, regime = c("iid", "ms", "mns"), 
                    k = 2, features = rep(1, length(lpo)), noise = 10^-8){
  
  if(max(c(table(lpo), table(lpm))) >= k){
    lpo <- lpo + runif(length(lpo), -noise, noise)
    lpm <- lpm + runif(length(lpm), -noise, noise)
  }
  
  if(regime == "iid"){
    res <- FNN::mutinfo(lpo, lpm, k)/log(2)
  } else if(regime == "ms"){
    res <- mi.ms.knn(lpo, lpm, k)
  } else if(regime == "mns"){
    res <- mi.mns.knn(lpo, lpm, k, features)
  }
  res
}





