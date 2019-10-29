#' Discretization of numerical vector
#'
#' The function quantizes a single numerical vector
#' @param lp vectors to be quantized
#' @param num.bins number of bins
#' @param bin.size size of bins
#' @param mode if "number", load profile is split into num.bins of equidistant bins;
#' if "size", bin.size is used for defining the size of each bin.
#' @return bin index for discretized vector lp
#' @export
#' @references for use case of fixed number of bins, see e.g., J. X. Chin, G. Giaconi, T. T. De Rubira, 
#' D. Gimduz, and G. Hug, "Considering time correlation in the estimation of privacy loss for 
#' consumers with smart meters," in 20th Power Systems Computation Conference, PSCC 2018, 2018;
#' 
#' for use of pre-defined step size of quantization, see G. Kalogridis, C. Efthymiou, S. Z. Denic, 
#' T. A. Lewis, and R. Cepeda, "Privacy for Smart Meters: Towards Undetectable Appliance Load 
#' Signatures," 2010 First IEEE International Conference on Smart Grid Communications, pp. 232-237, 2010.
#' @examples
#' set.seed(1)
#' lpo <- squares(20, c(1, 1, 1))[, 1]
#' discr.sep(lpo, mode = "number")
#' discr.sep(lpo, mode = "size")

discr.sep <- function(lp, num.bins = 20, bin.size = 0.01,
                    mode = c("number", "size")){

  if(mode == "number"){
    lp_min <- min(lp)
    lp_max <- max(lp)
    breaks <- seq(lp_min, lp_max, (lp_max - lp_min)/num.bins)
  }else if(mode == "size"){
    lp_min <- min(lp) - min(lp)%%bin.size
    lp_max <- (max(lp)%/%bin.size + 1)*bin.size
    breaks = seq(lp_min, lp_max, by = bin.size)
    if(length(breaks) < 3) warning("less than 3 breaks are generated")
  }else{
    stop("such mode is not supported")
  }

  as.numeric(cut(lp, breaks, include.lowest = TRUE))
}


#' Discretization of two numerical vectors
#'
#' The function takes two numerical vectors and quantizes them using the same bins
#' @param lpo,lpm vectors to be quantized
#' @param num.bins number of bins
#' @param bin.size size of bins
#' @param mode if "number", load profile is split into num.bins of equidistant bins;
#' if "size", bin.size is used for defining the size of each bin.
#' @return list, containing bin indexes discretized vectors lpo and lpm
#' @export
#' @references for use case of fixed number of bins, see e.g., J. X. Chin, G. Giaconi, T. T. De Rubira, 
#' D. Gimduz, and G. Hug, "Considering time correlation in the estimation of privacy loss for 
#' consumers with smart meters," in 20th Power Systems Computation Conference, PSCC 2018, 2018;
#' 
#' for use of pre-defined step size of quantization, see G. Kalogridis, C. Efthymiou, S. Z. Denic, 
#' T. A. Lewis, and R. Cepeda, "Privacy for Smart Meters: Towards Undetectable Appliance Load 
#' Signatures," 2010 First IEEE International Conference on Smart Grid Communications, pp. 232-237, 2010.
#' @examples
#' set.seed(1)
#' tmp <- squares(20, c(1, 1, 1))
#' plot(discr.together(tmp[, 1], tmp[, 2], mode = "number", num.bins = 6))
#' discr.together(tmp[, 1], tmp[, 2], mode = "size")

discr.together <- function(lpo, lpm, num.bins = 20, bin.size = 0.01,
                      mode = c("number", "size")){
  lp <- c(lpo, lpm)
  if(mode == "number"){
    lp_min <- min(lp)
    lp_max <- max(lp)
    breaks <- seq(lp_min, lp_max, (lp_max - lp_min)/num.bins)
  }else if(mode == "size"){
    lp_min <- min(lp) - min(lp)%%bin.size
    lp_max <- (max(lp)%/%bin.size + 1)*bin.size
    breaks = seq(lp_min, lp_max, by = bin.size)
    if(length(breaks) < 3) print("less than 3 breaks are generated")
  }else{
    stop("such mode is not supported")
  }
  
  cbind(cut(lpo, breaks, include.lowest = TRUE),
       cut(lpm, breaks, include.lowest = TRUE))
}
