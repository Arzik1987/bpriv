#' NILL algorithm
#'
#' Moderates a energy demand series using a rechargable battery 
#' and applying NILL algorithm to obscure actual energy usage.
#' @param lpo Energy demand series as nummeric vector, unit: kWh
#' @param max.be Battery's maximum capacity, unit: kWh
#' @param min.be Battery's minimum capacity, unit: kWh
#' @param max.bc Battery's maximum charing rate, unit: kW
#' @param max.bd Battery's maximum discharging rate, unit: kW
#' @param be Battery's current capacity level; 0 is empty, 1 is full
#' @param alpha Coefficient used to reset external load constant
#' @param voltage Used to convert between Amp and Watt, unit: vol
#' @param interval Demand metering interval, unit: minute
#' @export
#' @details The NILL algorithm has three states. In stable state (ST), 
#' NILL tries to maintain external load at some value Kst. When the 
#' battery is full, it moves to the high recovery state (HR), in 
#' which the external load will be set lower than the demand and 
#' the battery will be gradually discharged. The opposite case takes 
#' place when in stable state the battery is too low.
#' @references 
#' McLaughlin, Stephen, Patrick McDaniel, and William Aiello. 
#' "Protecting consumer privacy from electric load monitoring." 
#' Proceedings of the 18th ACM conference on Computer and 
#' communications security. ACM, 2011
#' 
#' Yang, Weining, et al. "Minimizing private data 
#' disclosures in the smart grid." Proceedings of the 2012 ACM 
#' conference on Computer and communications security. ACM, 2012.
#' @examples 
#' set.seed(1)
#' d <- squares(20)
#' lpo <- d[, 1]/100
#' lpm <- nill.moderate(lpo)
#' plot(ts(lpo))
#' lines(ts(lpm), col = "red")

nill.moderate <- function(lpo, max.be = 1, min.be = 0, max.bc = 0.5, 
                          max.bd = 0.5, be = 0.5, alpha = 0.5, voltage = 110, interval = 1) {
    be <- (max.be - min.be) * be + min.be
    interval <- interval / 60
    loads <- lpo / interval
    K.ST <- loads[1]
    moderated.metered.loads <- 1:length(loads)
    moderated.metered.loads[1] <- loads[1]
    battery <- 1:length(loads)
    battery[1] <- be
    states <- 1:length(loads)

    states[1] <- state <- 0
    total.metered.load <- 0
    state.times <- 0

    for (i in 2:length(lpo)) {
        demand <- loads[i]
        if (state == 0) {
            output = stable.output(max.be, min.be, max.bc, max.bd, be, demand, state, total.metered.load, state.times, interval, K.ST)
        } else if (state == -1) {
            output = low.recovery.output(max.be, min.be, max.bc, max.bd, be, demand, state, total.metered.load, state.times, interval, K.ST)
        } else if (state == 1) {
            output = high.recovery.output(max.be, min.be, max.bc, max.bd, be, demand, state, total.metered.load, state.times, interval, K.H, K.ST)
        }
        battery[i] <- be <- output[1]
        moderated.metered.loads[i] <- output[2]
        states[i] <- state <- output[3]
        total.metered.load <- output[4]
        state.times <- output[5]

        if (state == 1) {
            K.H <- output[6]
        }

        if (state == 0) {
            K.ST <- output[6]
        }
    }
    lpo <- moderated.metered.loads * interval
    return (lpo)

}

stable.output <- function(max.be, min.be, max.bc, max.bd, be, demand, state, total.metered.load, state.times, interval, K.ST, voltage=110) {
    if (state != 0) {
        state <- 0
        state.times <- 0
        total.metered.load <- 0
    }
    POWER.TO.COMPARE.1 <- 0.5 * voltage * (1/1000)

    if (demand > K.ST + max.bd) { # Case S3
        needed.battery.energy <- max.bd * interval
        if (be > 0.8 * max.be + 0.2 * min.be) {
          while (needed.battery.energy > be) {
            needed.battery.energy = needed.battery.energy * 0.8
          }
        }
        if (needed.battery.energy > be) { # Case S32
            return(low.recovery.output(max.be, min.be, max.bc, max.bd, be, demand, state, total.metered.load, state.times, interval, K.ST))
        } else {
            be <- be - needed.battery.energy
            metered.load <- demand - max.bd
        }
    } else if (demand < K.ST - max.bc) { # Case S4
        needed.battery.energy <- (-1) * max.bc * interval
        if (abs(needed.battery.energy) > max.be - be) { # Case S41
            K.H <- demand - POWER.TO.COMPARE.1
            return(high.recovery.output(max.be, min.be, max.bc, max.bd, be, demand, state, total.metered.load, state.times, interval, K.H, K.ST))
        } else {
            be <- be + abs(needed.battery.energy)
            metered.load <- demand + max.bc
        }
    } else {
        needed.battery.energy <- (demand - K.ST) * interval
        if (be > 0.8 * max.be + 0.2 * min.be) {
          while (needed.battery.energy > be) {
            needed.battery.energy = needed.battery.energy * 0.8
          }
        }
        if (needed.battery.energy > be) { # Case S2
            return(low.recovery.output(max.be, min.be, max.bc, max.bd, be, demand, state, total.metered.load, state.times, interval, K.ST))
        } else if (needed.battery.energy < 0 && abs(needed.battery.energy) > max.be - be) { # Case S1
            if (demand - POWER.TO.COMPARE.1 <= 0) {
                K.H <- demand
            } else {
                K.H <- demand - POWER.TO.COMPARE.1
            }
            return(high.recovery.output(max.be, min.be, max.bc, max.bd, be, demand, state, total.metered.load, state.times, interval, K.H, K.ST))
        } else { # Case S5
            be <- be - needed.battery.energy
            metered.load <- K.ST
        }
    }
    state.times <- state.times + 1
    total.metered.load <- total.metered.load + metered.load
    return (c(be, metered.load, state, total.metered.load, state.times, K.ST))
}

low.recovery.output <- function(max.be, min.be, max.bc, max.bd, be, demand, state, total.metered.load, state.times, interval, K.ST) {
  if (state != -1) {
        state <- -1
        state.times <- 0
        total.metered.load <- 0
    }
    K.L <- max.bc
    if (be > 0.8 * max.be + 0.2 * min.be) {
        avg.load <- total.metered.load / state.times
        K.ST <- 0.5 * (K.ST + avg.load)
        return (stable.output(max.be, min.be, max.bc, max.bd, be, demand, state, total.metered.load, state.times, interval, K.ST))
    } else if (demand > K.L) {
        needed.battery.energy <- 0
        metered.load <- demand
    } else {
        metered.load <- K.L
        needed.battery.energy <- (demand - K.L) * interval
        be <- be - needed.battery.energy
    }
    state.times <- state.times + 1
    total.metered.load <- total.metered.load + metered.load
    return (c(be, metered.load, state, total.metered.load, state.times))
}

high.recovery.output <- function(max.be, min.be, max.bc, max.bd, be, demand, state, total.metered.load, state.times, interval, K.H, K.ST, voltage=110) {
    if (state != 1) {
        state <- 1
        state.times <- 0
        total.metered.load <- 0
    }
    POWER.TO.COMPARE.1 <- 0.5 * voltage * (1/1000)
    POWER.TO.COMPARE.2 <- 5 * voltage * (1/1000)
    needed.battery.energy <- (demand - K.H) * interval
    if (needed.battery.energy < 0 && abs(needed.battery.energy > max.be - be)) {
        if (demand - POWER.TO.COMPARE.1 < 0) {
            K.H <- demand
        } else {
            K.H <- demand - POWER.TO.COMPARE.1
        }
        metered.load <- K.H
        be <- be + (demand - K.H) * interval
    } else if (be < 0.5 * (max.be + min.be)) {
        avg.load <- total.metered.load / state.times
        K.ST <- 0.5 * (K.ST + avg.load)
        return (stable.output(max.be, min.be, max.bc, max.bd, be, demand, state, total.metered.load, state.times, interval, K.ST))
    } else if (demand > K.H + POWER.TO.COMPARE.2) {
        avg.load <- total.metered.load / state.times
        K.ST <- 0.5 * (K.ST + avg.load)
        return (stable.output(max.be, min.be, max.bc, max.bd, be, demand, state, total.metered.load, state.times, interval, K.ST))
    } else {
        metered.load <- K.H
        be <- be - (demand - K.H) * interval
    }
    state.times <- state.times + 1
    total.metered.load <- total.metered.load + metered.load
    return (c(be, metered.load, state, total.metered.load, state.times, K.H))
}
