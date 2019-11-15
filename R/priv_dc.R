#' Privacy measure: "Coefficient of determination"
#' 
#' Calculates privacy measure called "Coeficient of determination". 
#' This coincides with coefficient of determination betweed shifted
#' vectors in its common meaning only if option "reg" is used.
#' @param lpo,lpm original and modified load profiles
#' @param n number of tinestamps to consider in the search of best alingment
#' @param regime character. If "reg" the linear regression is fitted to derive the predicted values as in [1].
#' if "lpo", the predicted values coincide with the original load profile ("user load") as in [2].
#' @return privacy measure: determination coefficient
#' @export
#' 
#' @importFrom stats cor lm
#' 
#' @references [1] G. Kalogridis, S. Z. Denic, T. Lewis, and R. Cepeda,
#' "Privacy protection system and metrics for hiding electrical events,"
#' International Journal of Security and Networks, vol. 6, no. 1, p. 14, 2011.
#' 
#' [2] Chen, Zhi, and Lei Wu. "Residential appliance DR energy management 
#' with electric privacy protection by online stochastic optimization." 
#' IEEE Transactions on Smart Grid 4.4 (2013): 1861-1869
#' @examples
#' set.seed(1)
#' d <- squares(1000)
#' lpo <- d[, 1]
#' lpm <- d[, 2]
#' priv.dc(lpo, lpm, n = 20, regime = "reg")
#' priv.dc(lpo, lpm, n = 20, regime = "lpo")

priv.dc <- function(lpo, lpm, n = 10, regime = c("reg", "lpo")){
  
  if(length(unique(lpm)) == 1) return(0)
  
  if(regime == "reg"){
    cors <- sapply(-n:n, function(x) {
      a <- shift(lpo, lpm, x)
      cor(a[, 1], a[, 2])
    })
    cors <- cors[!is.na(cors)]
    best.shift <- (-n:n)[which(cors == max(cors))][1]
    tmp <- shift(lpo, lpm, best.shift)
    lpo <- tmp[, 1]
    lpm <- tmp[, 2]
    
    fit <- lm(lpm ~ lpo)
    res <- summary(fit)$r.squared
    # lpm.hat <- fit$fitted.values
  } else if(regime == "lpo"){
    ess <- sum((lpm - mean(lpm))^2)
    rss <- sum((lpo - lpm)^2)
    res <- 1 - rss/(rss + ess)
  }
  
  return(res)

}
