#' Nearest neighbour for KL divergence
#' 
#' The function computes Kullback-Leibler divergence bethween two distributions estimated
#' from time series data (load profiles) usung nearest neigbor estimator. 
#' @param lpo,lpm original and modified load profiles
#' @param k number of nearest neigbors
#' @param noise the amplitude of the noise to add in case of duplicated points
#' @return KL divergence estimate
#' @references Kalogridis, Georgios, et al. 
#' "Privacy for smart meters: Towards undetectable appliance load signatures." 
#' 2010 First IEEE International Conference on Smart Grid Communications. 
#' IEEE, 2010.
#' @export
#' 
#' @importFrom stats runif
#' 
#' @examples
#' set.seed(1)
#' d <- squares(1000)
#' lpo <- d[, 1]
#' lpm <- d[, 2]
#' priv.kl.knn(lpo, lpm)
#' 
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' priv.kl.knn(lpo, lpm, k = 2)
#' priv.kl.knn(lpo, lpm, k = 4)

priv.kl.knn <- function(lpo, lpm, k = 2, noise = 10^-8){
  
  if(sum(c(table(lpo) > k, table(lpm) > k)) > 0){ # add noice when more than k points coincide
    lpo <- lpo + runif(length(lpo), -noise, noise)
    lpm <- lpm + runif(length(lpm), -noise, noise)
  }
  
  FNN::KL.divergence(lpo, lpm, k = k)[k]
}
