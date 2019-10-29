#' Nearest neighbor estimator entropy ratio
#'
#' Calculates privacy as the ratio of load provile entropy after 
#' and before modification.
#' @param lpo,lpm original and modified load profiles
#' @param k number of nearest neigbors
#' @param regime character. If "zeros", zero values are included in calculation; if "nozeros" - not included.
#' @param noise the amplitude of the noise to add in case of duplicated points
#' @return knn entropy ratio estimate
#' @references S. McLaughlin, P. McDaniel, and W. Aiello, 
#' "Protecting consumer privacy from electric load monitoring," 
#' Proceedings of the 18th ACM conference on Computer and 
#' communications security - CCS '11, p. 87, 2011.
#' @export
#' 
#' @importFrom stats runif
#' 
#' @examples
#' set.seed(1)
#' d <- squares(1000)
#' lpo <- d[, 1]
#' lpm <- d[, 2]
#' priv.er.knn(lpo, lpm, k = 1, regime = "zeros")
#' priv.er.knn(lpo, lpm, k = 2, regime = "zeros")
#' priv.er.knn(lpo, lpm, k = 2, regime = "nozeros")
#' 
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' priv.er.knn(lpo, lpm, k = 1)
#' priv.er.knn(lpo, lpm, k = 4)

priv.er.knn <- function(lpo, lpm, k = 2, regime = c("zeros", "nozeros"), noise = 10^-8){
  
  if(length(unique(lpm)) == 1) return(0)
  
  zo <- zm <- 1
  if(regime == "nozeros"){
    zo <- sum(lpo != 0)
    zm <- sum(lpm != 0)
    lpo <- lpo[lpo != 0]
    lpm <- lpm[lpm != 0]
  }
  
  if(length(lpm) <= k | length(lpo) <= k) return(-1)
  
  if(sum(c(table(lpo) > k, table(lpm) > k)) > 0){ # add noice when more than k points coincide
    lpo <- lpo + runif(length(lpo), -noise, noise)
    lpm <- lpm + runif(length(lpm), -noise, noise)
  }

  (FNN::entropy(lpm, k = k)[k]*zm)/(FNN::entropy(lpo, k = k)[k]*zo)
}
