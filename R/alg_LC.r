#' Lazy charging algorithm
#'
#' Moderates a energy demand series using a rechargable battery 
#' and applying lazy charging algorithm to obscure actual energy usage.
#' @param lpo Energy demand series as nummeric vector, unit: kWh
#' @param max.be Battery's maximum capacity, unit: kWh
#' @param min.be Battery's minimum capacity, unit: kWh
#' @param max.bc Battery's maximum charing rate, unit: kW
#' @param max.bd Battery's maximum discharging rate, unit: kW
#' @param be Battery's current capacity level; 0 is empty, 1 is full
#' @param interval Demand metering interval, unit: minute
#' @export
#' @references Yang, Weining, et al. "Minimizing private data 
#' disclosures in the smart grid." Proceedings of the 2012 
#' ACM conference on Computer and communications security. ACM, 2012.
#' @details This algorithm tries to maintain charging state 
#' of battery until the battery is too low or too high. 
#' One advantage of the LC algorithm is that this reduces 
#' the number of charge/discharge cycles for the battery. 
#' One disadvantage of the LC algorithm is that it is generally 
#' easy to predict charging state of battery at some point.
#' @examples 
#' set.seed(1)
#' d <- squares(20)
#' lpo <- d[, 1]/100
#' lpm <- lazy.charging.moderate(lpo)
#' plot(ts(lpo))
#' lines(ts(lpm), col = "red")

lazy.charging.moderate <- function(lpo, max.be = 1, min.be = 0, max.bc = 0.5, max.bd = 0.5, be = 0.5, interval = 1) {
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

        # If battery's power can't keep up with demand -> increase metered load
        if (load.to.cover > max.bd) {
           h <- compute.h(charging.state, demand, beta)
           metered.load <- h * beta
           while (demand - metered.load > max.bd) {
             h <- h + 1
             metered.load <- h * beta
           }
        } else if (load.to.cover < 0 && abs(load.to.cover) > max.bc) {
            h <- compute.h(charging.state, demand, beta)
            metered.load <- h * beta
            while (demand - metered.load < 0 && abs(demand - metered.load) > max.bc && h > 0) {
              h <- h - 1
              metered.load <- h * beta
            }
        }

        # Try to keep charging state unchanged
        if (demand - metered.load > 0 && charging.state == 1) {
          while (demand - metered.load > 0) {
              h <- h + 1
              metered.load <- h * beta
          }
        } else if (demand - metered.load < 0 && charging.state == 0) {
          while (demand - metered.load < 0 && h > 0) {
              h <- h - 1
              metered.load <- h * beta
          }
        }
        used.battery.energy <- (demand - metered.load) * interval

        if (charging.state == 1) {
          res <- lc.charging(be=be, used.battery.energy=used.battery.energy, demand=demand, metered.load=metered.load, beta=beta, max.be, min.be=min.be, interval=interval)
        }

        if (charging.state == 0) {
          res <- lc.discharging(be=be, used.battery.energy=used.battery.energy, demand=demand, metered.load=metered.load, beta=beta, max.be, min.be=min.be, interval=interval)
        }

        h.series[i] <- h
        states[i] <- charging.state <- res[3]
        moderated.metered.loads[i] <- metered.load <- res[2]
        battery[i] <- be <- res[1]
    }
    lpo <- moderated.metered.loads * interval
    return (lpo)
}

lc.discharging <- function(be, used.battery.energy, demand, metered.load, beta, max.be, min.be, interval, ...) {
  if (be - used.battery.energy < min.be) {
    h <- compute.h(0, demand, beta)
    metered.load <- h * beta
    while ((demand - metered.load > 0)) {
      h <- h + 1
      metered.load <- h * beta
    }
    used.battery.energy <- (demand - metered.load) * interval
    return (lc.charging(be=be, used.battery.energy=used.battery.energy, demand=demand, metered.load=metered.load, beta=beta, max.be, min.be=min.be, interval=interval))
  } else {
    be <- be - used.battery.energy
  }
  return (c(be, metered.load, 0))
}

lc.charging <- function(be, used.battery.energy, demand, metered.load, beta, max.be, min.be, interval, ...) {
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
    return (lc.discharging(be=be, used.battery.energy=used.battery.energy, demand=demand, metered.load=metered.load, beta=beta, max.be, min.be=min.be, interval=interval))
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
