#' Best-effort algorithm
#'
#' Modifies the energy demand series using a rechargable battery 
#' and applying best-effort algorithm to obscure actual energy usage.
#' @param lpo Energy demand series as nummeric vector, unit: kWh
#' @param max.be Battery's maximum capacity, unit: kWh
#' @param max.bp Battery's maximum charing and discharging rate, unit: kW
#' @param be Battery's current state of charge; 0 is empty, 1 is full
#' @param interval Demand metering interval, unit: minute
#' @param regime If "first" (default) our interpretation of original algorithm [1] is used.
#' Otherwise, the algorith works as described in [2].
#' @export
#' @details In this algorithm, the external load is kept 
#' unchanged unless the battery is too low or too high. 
#' In either case, the battery will be fully discharged/charged. 
#' While the battery is empty/full, the external load is set to 
#' be equal to the demanding load and only kept unchanged when 
#' the battery is capable of charging/discharging again.
#' @references [1] Kalogridis, Georgios, et al. "Privacy for smart meters: 
#' Towards undetectable appliance load signatures." 2010 First IEEE 
#' International Conference on Smart Grid Communications. IEEE, 2010.
#' 
#' [2] Yang, W. et al. 2012. Minimizing private data disclosures in the smart grid. 
#' Proceedings of the 2012 ACM conference on Computer and 
#' communications security - CCS '12. (2012), 415.
#' @examples 
#' set.seed(1)
#' d <- squares(20)
#' lpo <- d[, 1]/100
#' lpm1 <- alg.be(lpo)
#' lpm2 <- alg.be(lpo, regime = "second")
#' plot(ts(lpo))
#' lines(ts(lpm1), col = "red")
#' lines(ts(lpm2), col = "blue")
#' 
#' lpm1 <- alg.be(lpo, max.be = 0.01, max.bp = 10)
#' lpm2 <- alg.be(lpo, max.be = 0.01, max.bp = 10, regime = "second")
#' plot(ts(lpo))
#' lines(ts(lpm1), col = "red")
#' lines(ts(lpm2), col = "blue")
#' 
#' lpm <- alg.be(lpo, max.bp = 0)
#' plot(ts(lpo))
#' lines(ts(lpm), col = "red")

alg.be <- function(lpo, max.be = 1, max.bp = 0.5, be = 0.5, interval = 1, regime = "first") {
  be <- max.be * be
  
  interval <- interval/60             # Change interval unit to hour
  lpo <- lpo/interval                 # user load in kW
  lpm <- rep(NA, length(lpo))         # grid load in kW
  lpm[1] <- lpo[1]
  bp <- 0
  
  for(i in 2:length(lpo)){
    bp <- lpo[i - 1] - lpo[i] + bp    # the recommended charging rate
    soc <- be + interval*bp           # SOC which would have been obtained withour restrictions
    
    if(regime == "first"){              # as in [1]
      soc <- ifelse(soc < 0, 0, soc)  # the SOC after the next period
      soc <- ifelse(soc > max.be, max.be, soc)
      bp <- min(abs(soc - be)/interval, max.bp)*sign(soc - be)
      lpm[i] <- lpo[i] + bp           # change in the measured load
      be <- be + interval*bp          # update SOC
    } else {                            # as in [2]
      if(soc >= 0 & soc <= max.be){
        bp <- min(abs(soc - be)/interval, max.bp)*sign(soc - be)
        lpm[i] <- lpo[i] + bp         # change in the measured load
        be <- be + interval*bp        # update SOC
      } else {                        # do not charge the battery
        bp <- 0
        lpm[i] <- lpo[i]
      }
    }
  }
  
  return(lpm*interval)
  
}

