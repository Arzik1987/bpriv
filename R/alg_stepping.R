#' Stepping algorithm
#'
#' Moderates a energy demand series using a rechargable battery 
#' and applying lazy stepping algorithm to obscure actual energy usage.
#' @param lpo Energy demand series as nummeric vector, unit: kWh
#' @param max.be Battery's maximum capacity, unit: kWh
#' @param max.bp Battery's maximum charing rate, unit: kW
#' @param be Battery's current capacity level; 0 is empty, 1 is full
#' @param interval Demand metering interval, unit: minute
#' @param regime Defines the charging/discharging strategy within the steping
#' framework. Possible values are "ls1", "ls2", "lc" and "rc" corresponding 
#' to two versions of lasy stepping, lasy charging and random charging algorithms
#' respectively (see the details and the reference).
#' @export
#' @details Lazy stepping algorithms try to maintain the external 
#' load unless it is pushed to change. There are three cases in 
#' which changes must occur: (1) When maintaining the load results 
#' in overcharging the battery, in which case the battery must be 
#' discharged; (2) When maintaining the load results in low battery, 
#' in which case the battery must be charged; and (3) when power 
#' demand is either too low or too high for the previous external load. 
#' We consider two alternatives, which we call LS1 and LS2. 
#' LS1 charges the battery if it is below half, and discharges 
#' it otherwise. LS2 randomly charges or discharges the battery.
#' LC tries to maintain charging state 
#' of battery until the battery is too low or too high. 
#' One advantage of the LC algorithm is that this reduces 
#' the number of charge/discharge cycles for the battery. 
#' One disadvantage of the LC algorithm is that it is generally 
#' easy to predict charging state of battery at some point.
#' In RC algorithm, the charging state of the
#' battery is set randomly each time a load demand comes in.
#' @return A moderated energy series
#' @references Yang, Weining, et al. "Minimizing private data 
#' disclosures in the smart grid." Proceedings of the 2012 
#' ACM conference on Computer and communications security. ACM, 2012.
#' @examples 
#' set.seed(1)
#' d <- squares(20)
#' lpo <- d[, 1]/100
#' lpm.ls1 <- alg.stepping(lpo)
#' lpm.ls2 <- alg.stepping(lpo, regime = "ls2")
#' lpm.lc <- alg.stepping(lpo, regime = "lc")
#' lpm.rc <- alg.stepping(lpo, regime = "rc")
#' 
#' plot(ts(lpo))
#' lines(ts(lpm.ls1), col = "red")
#' lines(ts(lpm.ls2), col = "blue")
#' lines(ts(lpm.lc), col = "orange")
#' lines(ts(lpm.rc), col = "purple")


alg.stepping <- function(lpo, max.be = 1, max.bp = 0.5, be = 0.5, interval = 1, regime = "ls1"){
  be <- max.be * be
  
  interval <- interval/60             # Change interval unit to hour
  lpo <- lpo/interval                 # user load in kW
  lpm <- rep(NA, length(lpo))         # grid load in kW
  
  # cs <- sample(0:1, size = 1)
  cs <- 0                             # charging signal (0 - discharging, 1 - charging)
  h <- ifelse(cs > 0, ceiling(lpo[1]/max.bp), floor(lpo[1]/max.bp))
  
  for(i in 1:length(lpo)){

    if(regime == "lc"){               # try to keep cs constant
      h <- ifelse(cs > 0, ceiling(lpo[i]/max.bp), floor(lpo[i]/max.bp))   
    }
    if(regime == "rc"){               # randomly choose cs with probabilities depending on SOC
      cs <- sample(0:1, size = 1, prob = c(be/max.be, (1 - be/max.be)))
      h <- ifelse(cs > 0, ceiling(lpo[i]/max.bp), floor(lpo[i]/max.bp))
    }
    
    bp <- h*max.bp - lpo[i]           # battery charging rate kWh
    
    if(abs(bp) > max.bp){             # should not enter this if regime = "lc" or "rc"
      print("ls")
      if(regime == "ls1"){            # choose charging if SOC is less than half capacity
        cs <- ifelse(be/max.be < 0.5, 1, 0)
      } else {
        cs <- sample(0:1, size = 1)   # choose cs at random
      }
      h <- ifelse(cs > 0, ceiling(lpo[i]/max.bp), floor(lpo[i]/max.bp))
    }
    
    bp <- h*max.bp - lpo[i]           # battery charging rate kWh 
    soc <- be + interval*bp           # SOC that would be achieved
    
    if(soc < 0){
      cs <- 1                         # charge if othervise the battery is empty
      h <- ifelse(cs > 0, ceiling(lpo[i]/max.bp), floor(lpo[i]/max.bp))
    }
    if(soc > max.be){
      cs <- 0                         # discharge is otherwise the battery is full
      h <- ifelse(cs > 0, ceiling(lpo[i]/max.bp), floor(lpo[i]/max.bp))
    }
    
    bp <- h*max.bp - lpo[i]
    be <- be + interval*bp
    lpm[i] <- lpo[i] + bp
    
    if(be > max.be | be < 0){         # seemingly, the battery is quite small or the interval is quite big
      stop("The battery charging rate/capacity ratio or the measuring interval is too large")
    }
  }
  return(lpm*interval)
}
