#' NILL algorithm
#'
#' Moderates a energy demand series using a rechargable battery 
#' and applying NILL algorithm to obscure actual energy usage.
#' @param lpo Energy demand series as nummeric vector, unit: kWh
#' @param max.be Battery's maximum capacity, unit: kWh
#' @param max.bp Battery's maximum charing and discharging rate, unit: kW
#' @param be Battery's current state of charge; 0 is empty, 1 is full
#' @param alpha Coefficient used to reset external load constant
#' @param voltage Used to convert between Amp and Watt, unit: volt
#' @param interval Demand metering interval, unit: minute
#' @param kst The initial value of the target load in the stable state, unit = kW.
#' if "first", the first value of demand time series is used. 
#' If "avg", the average value of demand time series is used.
#' @export
#' @details The NILL algorithm has three states. In stable state (ST), 
#' NILL tries to maintain external load at some value kst. When the 
#' battery is full, it moves to the high recovery state (HR), in 
#' which the external load will be set lower than the demand and 
#' the battery will be gradually discharged. The opposite case takes 
#' place when in stable state the battery is too low.
#' @references 
#' [1] McLaughlin, Stephen, Patrick McDaniel, and William Aiello. 
#' "Protecting consumer privacy from electric load monitoring." 
#' Proceedings of the 18th ACM conference on Computer and 
#' communications security. ACM, 2011
#' 
#' [2] Yang, Weining, et al. "Minimizing private data 
#' disclosures in the smart grid." Proceedings of the 2012 ACM 
#' conference on Computer and communications security. ACM, 2012.
#' @examples 
#' set.seed(1)
#' d <- squares(20)
#' lpo <- d[, 1]/100
#' lpm <- alg.nill(lpo, kst = "avg")
#' lpm1 <- alg.be(lpo)
#' plot(ts(lpo))
#' lines(ts(lpm), col = "blue")
#' lines(ts(lpm1), col = "red") # compare to Best Effort algorithm

alg.nill <- function(lpo, max.be = 1, max.bp = 0.5, be = 0.5, alpha = 0.5, 
                     voltage = 110, interval = 1, kst = "avg"){
  be <- max.be*be
  
  interval <- interval/60             # Change interval unit to hour
  lpo <- lpo/interval                 # user load in kW
  lpm <- rep(NA, length(lpo))         # grid load in kW

  if(kst == "avg"){                   # calculate the initial value of kst
    kst = mean(lpo)
  } else if(kst == "first"){
    kst = lpo[1]
  }
  
  n <- length(lpo)
  i <- 1
  while(i <= n){
    bp <- min(abs(kst - lpo[i]), max.bp)*sign(kst - lpo[i]) # use the battery to maximum possibility as in [2]
    soc <- be + interval*bp
    
    if(soc > max.be){                 # if the battery will be overcharged
      res <- hrs(i, lpo, lpm, voltage, max.be, max.bp, interval, be, soc)
      kst <- (1 - alpha)*kst + alpha*mean(lpo[i:res[[1]]])
      i <- res[[1]]
      lpm <- res[[2]]
      be <- res[[3]]
    } else if(soc < 0){               # if the battery does not have enough energy
      res <- hls(i, lpo, lpm, voltage, max.be, max.bp, interval, be, soc)
      kst <- (1 - alpha)*kst + alpha*mean(lpo[i:res[[1]]])
      i <- res[[1]]
      lpm <- res[[2]]
      be <- res[[3]]
    } else {                          # staying in the normal state
      be <- soc
      lpm[i] <- lpo[i] + bp
    }
    
    i <- i + 1
    print(i)
  }
  
  return(lpm*interval)
  
}

hrs <- function(i, lpo, lpm, voltage, max.be, max.bp, interval, be, soc){   # the SOC is too high
  
  n <- length(lpo)
  kh <- max(0, lpo[i] - 0.5*voltage/1000)         # set the target load slightly less than the current demand as in [2] ([1] recomments 1-5 instead of 0.5)
  while(i <= n & ((lpo[i] - kh)*1000/voltage) < 5 & soc/max.be > 0.5){      # if demand rises, or the battery is charged enough, as in [2]
    bp <- min(abs(kh - lpo[i]), max.bp)*sign(kh - lpo[i])
    soc <- be + interval*bp
    
    if(soc > max.be){                     # if demand drops further
      kh <- max(0, lpo[i] - 0.5*voltage/1000)
      bp <- min(abs(kh - lpo[i]), max.bp)*sign(kh - lpo[i])
      soc <- be + interval*bp
    }
    if(soc < 0){                          # if the battery is very small, so that can be discharged during the next period
      soc <- 0
      bp <- min(abs(soc - be)/interval, max.bp)*sign(soc - be)
    }
    
    be <- soc
    lpm[i] <- lpo[i] + bp
    i <- i + 1
  }
  return(list(i - 1, lpm, be))
}

hls <- function(i, lpo, lpm, voltage, max.be, max.bp, interval, be, soc){   # the SOC is too low
  
  n <- length(lpo)
  kl <- max.bp                            # will now return to the normal mode, if maximal charging rate is small
  while(i <= n & soc/max.be < 0.8){       # if the battery is charged to 80% of the capacity as in [1], [2]
    bp <- min(abs(kl - lpo[i]), max.bp)*sign(kl - lpo[i])
    soc <- be + interval*bp
    
    if(soc < 0){                          #  if load exceeds the maximal charging rate
      bp <- 0
      soc <- be
    }
    if(soc > max.be){                     # if the battery is so small, that it will be overcharged
      soc <- max.be
      bp <- min(abs(soc - be)/interval, max.bp)*sign(soc - be)
    }
  
    be <- soc
    lpm[i] <- lpo[i] + bp
    i <- i + 1
  }
  return(list(i - 1, lpm, be))
}


