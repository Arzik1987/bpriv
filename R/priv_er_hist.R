#' Histogramm estimator entropy ratio
#'
#' Calculates privacy as the ratio of load provile entropy after 
#' and before modification.
#' @param lpo,lpm original and modified load profiles
#' @param num.bins number of bins
#' @param bin.size size of bins
#' @param mode if "number", load profile is split into num.bins of equidistant bins;
#' if "size", bin.size is used for defining the size of each bin.
#' @param regime character. If "zeros", zero values are included in calculation; if "nozeros" - not included.
#' @return entropy ratio estimate
#' @export
#' @references S. McLaughlin, P. McDaniel, and W. Aiello, 
#' "Protecting consumer privacy from electric load monitoring," 
#' Proceedings of the 18th ACM conference on Computer and 
#' communications security - CCS '11, p. 87, 2011.
#' @examples
#' set.seed(1)
#' d <- squares(1000)
#' lpo <- d[, 1]
#' lpm <- d[, 2]
#' priv.er.hist(lpo, lpm, bin.size = 0.01, mode = "size")
#' 
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' priv.er.hist(lpo, lpm, num.bins = 3)
#' priv.er.hist(lpm, lpo, num.bins = 3)

priv.er.hist <- function(lpo, lpm, num.bins = 20, bin.size = 0.01, mode = "number", regime = c("zeros", "nozeros")){

  if(length(unique(lpm)) == 1) return(0)
  
  zo <- zm <- 1
  if(regime == "nozeros"){
    zo <- sum(lpo != 0)
    zm <- sum(lpm != 0)
    lpo <- lpo[lpo != 0]
    lpm <- lpm[lpm != 0]
  }
  
  if(length(unique(lpm)) == 1) return(0)
  
  lpo <- table(discr.sep(lp = lpo, num.bins = num.bins, bin.size = bin.size, mode = mode))
  lpm <- table(discr.sep(lp = lpm, num.bins = num.bins, bin.size = bin.size, mode = mode))

  (entropy::entropy.empirical(lpm)*zm)/(entropy::entropy.empirical(lpo)*zo)
}
