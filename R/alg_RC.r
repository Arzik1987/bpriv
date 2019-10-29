#' Random charging algorithm
#'
#' Moderates a energy demand series using a rechargable battery
#' and applying random charging algorithm to obscure actual energy usage.
#' @param lpo Energy demand series as nummeric vector, unit: kWh
#' @param max.be Battery's maximum capacity, unit: kWh
#' @param min.be Battery's minimum capacity, unit: kWh
#' @param max.bc Battery's maximum charing rate, unit: kW
#' @param max.bd Battery's maximum discharging rate, unit: kW
#' @param be Battery's current capacity level; 0 is empty, 1 is full
#' @param interval Demand metering interval, unit: minute
#' @export
#' @details In this algorithm, the charging state of the
#' battery is set randomly each time a load demand comes in.
#' @references Yang, Weining, et al. "Minimizing private data
#' disclosures in the smart grid." Proceedings of the 2012 ACM
#' conference on Computer and communications security. ACM, 2012.
#' @examples
#' set.seed(1)
#' d <- squares(20)
#' lpo <- d[, 1]/100
#' lpm <- random.charging.moderate(lpo)
#' plot(ts(lpo))
#' lines(ts(lpm), col = "red")

random.charging.moderate <- function(lpo, max.be=1, min.be=0, max.bc=0.5, max.bd=0.5, be=0.5, interval=1) {
    be <- (max.be - min.be) * be + min.be
    set.seed(1)
    interval <- interval / 60
    loads <- lpo / interval
    beta <- min(c(max.bc, max.bd))
    prob <- (be - min.be) / (max.be - min.be)
    charging.state <- sample(c(0,1), size=1, prob=c(prob, 1-prob))
    battery <- 1:length(loads)
    moderated.metered.loads <- 1:length(loads)
    states <- 1:length(loads)
    probs <- 1:length(loads)

    for (i in 1:length(loads)) {
        demand <- loads[i]

        if (charging.state == 1) {
            res <- rc.charging(be=be, demand=demand, beta=beta, max.be=max.be, min.be=min.be, interval=interval)
        }

        if (charging.state == 0) {
            res <- rc.discharging(be=be, demand=demand, beta=beta, max.be=max.be, min.be=min.be, interval=interval)
        }
        probs[i] <- prob
        states[i] <- charging.state <- res[3]
        moderated.metered.loads[i] <- metered.load <- res[2]
        battery[i] <- be <- res[1]
        prob <- (be - min.be) / (max.be - min.be)
        charging.state <- sample(c(0,1), size=1, prob=c(prob, 1-prob))
    }
    lpo <- moderated.metered.loads * interval
    return (lpo)
}

rc.charging <- function(be, demand, beta, max.be, min.be, interval) {
    h <- ceiling(demand / beta)
    metered.load <- h * beta
    while (demand - metered.load > 0) {
        h <- h + 1
        metered.load <- h * beta
    }
    used.battery.energy <- (demand - metered.load) * interval
    if (be - used.battery.energy > max.be) { ## Force discharging because battery too high
        return (rc.discharging(be=be, demand=demand, beta=beta, max.be=max.be, min.be=min.be, interval=interval))
    } else {
        be <- be - used.battery.energy
    }
    return (c(be, metered.load, 1))
}

rc.discharging <- function(be, demand, beta, max.be, min.be, interval) {
    if (demand != 0 && demand %% beta == 0) {
        h <- (demand / beta) - 1
    } else {
        h <- floor(demand / beta)
    }
    metered.load <- h * beta
    while (demand - metered.load < 0 && h > 0) {
        h <- h - 1
        metered.load <- h * beta
    }

    used.battery.energy <- (demand - metered.load) * interval
    if (be - used.battery.energy < min.be) { ## Force discharging state because battery too low
        return (rc.charging(be=be, demand=demand, beta=beta, max.be=max.be, min.be=min.be, interval=interval))
    } else {
        be <- be - used.battery.energy
    }
    return (c(be, metered.load, 1))
}
