#' Mixes outcones for non-stationary Markov process with features
#'
#' For two consecutive feature values produces the combinations of different outcomes for estimating 
#' 4-dimensional probabilities
#' @param d1,d2 two column tables for outcomes of two consequitive periods of load profiles defined by 
#' feature values
#' @return the table of outcomes
#' @references J. X. Chin, G. Giaconi, T. T. De Rubira, D. Gimduz, and G. Hug, 
#' "Considering time correlation in the estimation of privacy loss for consumers with smart meters," 
#' in 20th Power Systems Computation Conference, PSCC 2018, 2018.
#' @export
#' @examples
#' x <- c("a", "b", "c")
#' y <- 1:3
#' d1 <- cbind(x, y)
#' x <- c("d", "e", "f")
#' y <- 4:6
#' d2 <- cbind(x, y)
#' rmix(d1, d2)

rmix <- function(d1, d2){
  a <- 1:nrow(d1)
  b <- 1:nrow(d2)
  ab <- expand.grid(a, b)
  d <- cbind(d1[ab[, 1],], d2[ab[, 2],])
  d <- d[, c(1, 3, 2, 4)]
  d
}
