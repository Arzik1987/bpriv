#' Best-effort algorithm
#'
#' Moderates a energy demand series using a rechargable battery 
#' and applying best-effort algorithm to obscure actual energy usage.
#' @param lpo Energy demand series as nummeric vector, unit: kWh
#' @param max.be Battery's maximum capacity, unit: kWh
#' @param max.bp Battery's maximum charing and discharging rate, unit: kW
#' @param be Battery's current capacity level; 0 is empty, 1 is full
#' @param interval Demand metering interval, unit: minute
#' @export
#' @details In this algorithm, the external load is kept 
#' unchanged unless the battery is too low or too high. 
#' In either case, the battery will be fully discharged/charged. 
#' While the battery is empty/full, the external load is set to 
#' be equal to the demanding load and only kept unchanged when 
#' the battery is capable of charging/discharging again.
#' @references Kalogridis, Georgios, et al. "Privacy for smart meters: 
#' Towards undetectable appliance load signatures." 2010 First IEEE 
#' International Conference on Smart Grid Communications. IEEE, 2010.
#' @examples 
#' set.seed(1)
#' d <- squares(20)
#' lpo <- d[, 1]/100
#' lpm <- best.effort.moderate(lpo)
#' plot(ts(lpo))
#' lines(ts(lpm), col = "red")

best.effort.moderate <- function(lpo, max.be = 1, max.bp = 0.5, be = 0.5, interval = 1) {
  be <- max.be * be
  bp <- max.bp

  # Change interval unit to hour
  interval <- interval / 60

  loads <- lpo / interval

  moderated.metered.loads <- 1:length(loads)
  moderated.metered.loads[1]  <- loads[1]
  battery <- 1:length(loads)
  battery[1] <- max.be
  used.battery <- 1:length(loads)
  used.battery[1] <- 0
  diffs <- 1:length(loads)
  diffs[1] <- 0
  load.diff <- loads[2] - loads[1]

  for (i in 2:length(lpo)) {
    if (load.diff == 0) {
      metered.load <- loads[i-1]
      used.battery.energy <- 0
    } else {
      if (abs(load.diff) > max.bp) { # If demand load is not within battery power
        temp.load <- max.bp * sign(load.diff) # Predicted used battery load
        needed.battery.energy <- temp.load * interval
        temp.be <- be - needed.battery.energy
        if (0 <= temp.be && temp.be <= max.be) {
          metered.load <- abs(load.diff - temp.load)
          be <- temp.be
          used.battery.energy <- needed.battery.energy
        } else {
          if (temp.be < 0) {
            used.battery.energy <- be
            be <- 0
          }
          if (temp.be > max.be) {
            used.battery.energy <- be - max.be
            be <- max.be
          }
          used.grid.energy <- lpo[i] - used.battery.energy
          metered.load <- used.grid.energy / interval
        }
      } else {
        needed.battery.energy <- load.diff * interval # Battery energy needed to equalize previous metered load
        temp.be <- be - needed.battery.energy
        if (0 <= temp.be && temp.be <= max.be) { # If there's enough battery energy
          be <- temp.be
          metered.load <- moderated.metered.loads[i-1]
          used.battery.energy <- needed.battery.energy
        } else {
          if (temp.be < 0) {
            used.battery.energy <- be
            be <- 0
          }
          if (temp.be > max.be) {
            used.battery.energy <- be - max.be
            be <- max.be
          }
          used.grid.energy <- lpo[i] - used.battery.energy
          metered.load <- used.grid.energy / interval
        }
      }
    }
    battery[i] <- be
    used.battery[i] <- used.battery.energy
    moderated.metered.loads[i] <- metered.load
    load.diff <- loads[i+1] - metered.load
    diffs[i] <- load.diff
  }
  lpo <- moderated.metered.loads * interval
  return(lpo)
}
