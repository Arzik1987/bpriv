#' Relative feature mass
#'
#' Calculates privacy measure based on feature mass.
#' @param lpo,lpm original and modified load profiles
#' @param thr threshold value. The value of load profile exceeding thr is a feature
#' @param regime character specifying output. "fm" - feature mass, "rfm - relative
#' feature mass, "ed" - event detection accuracy.
#' @return value of feature mass based privacy measure
#' @export
#' @references Relative feature mass: S. McLaughlin, P. McDaniel, and W. Aiello,
#' "Protecting consumer privacy from electric load monitoring," 
#' Proceedings of the 18th ACM conference on Computer and 
#' communications security - CCS '11, p. 87, 2011.
#' 
#' Feature mass: Yang, Lei, et al. "Cost-effective and privacy-preserving 
#' energy management for smart meters." 
#' IEEE Transactions on Smart Grid 6.1 (2014): 486-495.
#' 
#' Event detection: Zhao, Jing, et al. "Achieving differential privacy 
#' of data disclosure in the smart grid." IEEE INFOCOM 2014-IEEE 
#' Conference on Computer Communications. IEEE, 2014.
#' 
#' @examples
#' set.seed(1)
#' d <- squares(1000)
#' lpo <- d[, 1]
#' lpm <- d[, 2]
#' priv.fm(lpo, lpm, thr = 1.5, regime = "ed")
#' priv.fm(lpo, lpm, thr = 1.5, regime = "fm")
#' priv.fm(lpo, lpm, thr = 1.5, regime = "rfm")
#' 
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' priv.fm(lpo, lpm, thr = 1.6, regime = "ed")
#' priv.fm(lpo, lpm, thr = 1.6, regime = "fm")
#' priv.fm(lpo, lpm, thr = 1.6, regime = "rfm")

priv.fm <- function(lpo, lpm, thr = 0, regime = c("fm", "rfm", "ed")){
  
  res <- 0
  
  if(regime == "fm"){
    res <- sum(abs(lpm) > thr) # feature mass
  } else if(regime == "rfm"){
    if(sum(abs(lpo) > thr) > 0) res <- sum(abs(lpm) > thr)/sum(abs(lpo) > thr) # relative feature mass  
  } else if(regime == "ed"){
    if(sum(abs(lpm) > thr) > 0) res <- sum((abs(lpo) > thr) & (abs(lpm) > thr))/sum(abs(lpm) > thr) #event detection
  }

  return(res)
  
}
