#' Lazy stepping algorithm
#'
#' Moderates a energy demand series using a rechargable battery 
#' and applying lazy stepping algorithm to obscure actual energy usage.
#' @param lpo Energy demand series as nummeric vector, unit: kWh
#' @param max.be Battery's maximum capacity, unit: kWh
#' @param min.be Battery's minimum capacity, unit: kWh
#' @param max.bc Battery's maximum charing rate, unit: kW
#' @param max.bd Battery's maximum discharging rate, unit: kW
#' @param be Battery's current capacity level; 0 is empty, 1 is full
#' @param interval Demand metering interval, unit: minute
#' @param mode Used to set battery status when charging/discharging 
#' rate cannot keep up with power demand. Mode 1 has battery charging 
#' when it's below half-full. Mode 2 has battery charging randomly.
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
#' @return A moderated energy series
#' @references Yang, Weining, et al. "Minimizing private data 
#' disclosures in the smart grid." Proceedings of the 2012 
#' ACM conference on Computer and communications security. ACM, 2012.
#' @examples 
#' set.seed(1)
#' d <- squares(20)
#' lpo <- d[, 1]/100
#' lpm <- lazy.stepping.moderate(lpo)
#' plot(ts(lpo))
#' lines(ts(lpm), col = "red")

lazy.stepping.moderate <- function(lpo, max.be = 1, min.be = 0, max.bc = 0.5, max.bd = 0.5, be = 0.5, interval = 1, mode = 1) {
    be <- (max.be - min.be) * be + min.be
    interval <- interval / 60
    loads <- lpo / interval
    beta <- min(c(max.bc, max.bd))
    charging.state <- 0
    battery <- 1:length(loads)
    battery[1] <- be
    moderated.metered.loads <- 1:length(loads)
    states <- 1:length(loads)
    demand <- loads[1]
    h <- compute.h(charging.state, demand, beta)
    h.series <- 1:length(loads)
    metered.load <- h * beta

    for (i in 1:length(loads)) {
        demand <- loads[i]
        load.to.cover <- demand - metered.load

        # The new demand is too high or low for the last ext. load
        if (demand >= (h + 1) * beta || demand <= (h - 1) * beta) {
          # First, set charging state
          # Charging state now is used to determine the new ext. load;
          # It will be eventually changed later according to difference between ext. load and demand
          if (mode == 1) {
            if (be < max.be / 2) {
              charging.state <- 1
            } else {
              charging.state <- 0
            }
          } else {
            charging.state <- sample(c(0,1), size=1, prob=c(0.5,0.5))
          }
          # Second, compute corresponding ext. load
          h <- compute.h(charging.state, demand, beta)
          metered.load <- h * beta
        }

        # Use difference between load demand and "external load" to determine needed battery energy
        used.battery.energy <- (demand - metered.load) * interval

        # If demand is greater than ext. load -> battery is used to provide engery for devices
        # Else -> battery needs charging using power grid to "cover" the difference
        if (used.battery.energy > 0) {
          charging.state <- 0
        } else if (used.battery.energy < 0) {
          charging.state <- 1
        }

        if (charging.state == 1) {
          res <- ls.charging(be=be, used.battery.energy=used.battery.energy, demand=demand, metered.load=metered.load, beta=beta, max.be=max.be, min.be=min.be, interval=interval)
        }

        if (charging.state == 0) {
          res <- ls.discharging(be=be, used.battery.energy=used.battery.energy, demand=demand, metered.load=metered.load, beta=beta, max.be=max.be, min.be=min.be, interval=interval)
        }

        h.series[i] <- h
        states[i] <- charging.state <- res[3]
        moderated.metered.loads[i] <- metered.load <- res[2]
        battery[i] <- be <- res[1]
    }
    lpo <- moderated.metered.loads * interval
    return (lpo)
}

ls.discharging <- function(be, used.battery.energy, demand, metered.load, beta, max.be, min.be, interval) {
  if (be - used.battery.energy < min.be) {
    h <- compute.h(0, demand, beta)
    metered.load <- h * beta
    while ((demand - metered.load > 0)) {
      h <- h + 1
      metered.load <- h * beta
    }
    used.battery.energy <- (demand - metered.load) * interval
    return (ls.charging(be=be, used.battery.energy=used.battery.energy, demand=demand, metered.load=metered.load, beta=beta, max.be=max.be, min.be=min.be, interval=interval))
  } else {
    be <- be - used.battery.energy
  }
  return (c(be, metered.load, 0))
}

ls.charging <- function(be, used.battery.energy, demand, metered.load, beta, max.be, min.be, interval) {
  # Battery shouldn't be overloaded
  if (be - used.battery.energy > max.be) {

    # Now the new metered load should be defined. But if it's not small enough for the battery to be discharged? Decrease h then!
    h <- compute.h(1, demand, beta)
    metered.load <- h * beta
    while (h > 0 && (demand - metered.load) < 0) {
      h <- h - 1
      metered.load <- h * beta
    }
    used.battery.energy <- (demand - metered.load) * interval
    return (ls.discharging(be=be, used.battery.energy=used.battery.energy, demand=demand, metered.load=metered.load, beta=beta, max.be=max.be, min.be=min.be, interval=interval))
  } else {
    # Now battery capacity should increase
    be <- be - used.battery.energy
  }
  return (c(be, metered.load, 1))
}

compute.h <- function(state, demand, beta) {
    if (state == 0) {
        if (demand %% beta == 0) {
            h <- (demand / beta) - 1
        } else {
            h <- floor(demand / beta)
        }
    }

    if (state == 1) {
        h <- ceiling(demand / beta)
    }

    return (h)
}
