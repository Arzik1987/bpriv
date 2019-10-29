#' Reconstruction-based privacy measure
#'
#' @param lpo,lpm original and modified load profiles
#' @param regime if "v" value leaks is used; if "w" - wavelet filtering is used
#' @return the value of the reconstruction privacy measure
#' @export
#' 
#' @importFrom stats sd
#' 
#' @examples
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' priv.rc(lpo, lpm, regime = "v")
#' priv.rc(lpm, lpo, regime = "v")
#' priv.rc(lpo, lpm, regime = "w")
#' priv.rc(lpm, lpo, regime = "w")

priv.rc <- function(lpo, lpm, regime = c("v", "w")) {
  if(!(regime %in% c("v", "w"))) stop("Incorrecte value of regime parameter")
  if(regime == "v") return(priv.rc.v(lpo, lpm))
  if(regime == "w") return(priv.rc.w(lpo, lpm))
}


#' Attempt to reconstruct the original profile using wavelet filtering
#'
#' @param lpo,lpm original and modified load profiles
#' @return Predicted original time series
#' @export
#' 
#' @importFrom stats sd
#' 
#' @examples
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' priv.rc.w(lpo, lpm)
#' priv.rc.w(lpm, lpo)
#' 
#' set.seed(1)
#' d <- squares(17)
#' priv.rc.w(d[, 1], d[, 2])
#' priv.rc.w(d[, 2], d[, 1])

priv.rc.w <- function(lpo, lpm) {
  if (length(unique(lpm)) == 1)
    return(0)
  res <- wavelet.filtering(lpm)
  sd.moderated <- sd(lpo-lpm)
  sd.attacked <- sd(lpo-res)
  removed.uncertainty <- (sd.moderated - sd.attacked) / sd.moderated
  if (removed.uncertainty < 0) removed.uncertainty <- 0
  return(removed.uncertainty)
}

#' Find correlation between the perturbed time series and original one.
#'
#' @param lpo Original time series
#' @param lpm Modified time series
#' @return Time series representing correlation.
#' @export
#' @examples
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' priv.rc.v(lpo, lpm)
#' priv.rc.v(lpm, lpo)
#' 
#' set.seed(1)
#' d <- squares(17)
#' priv.rc.v(d[, 1], d[, 2])
#' priv.rc.v(d[, 2], d[, 1])

priv.rc.v <- function(lpo, lpm) {
  if (length(unique(lpm)) == 1)
    return(0)
  res <- value.leaks(lpo, lpm)
  sd.moderated <- sd(lpo-lpm)
  sd.attacked <- sd(lpo-res)
  if (sd.moderated == 0) return(0)
  removed.uncertainty <- (sd.moderated - sd.attacked) / sd.moderated
  if (removed.uncertainty < 0) removed.uncertainty <- 0
  return(removed.uncertainty)
}



wavelet.filtering <- function(original) {
  result = c()
  last.position = 0
  number.records = length(original)
  max.length = 2 ** floor(log(number.records, 2))

  while (max.length != 0) {
    current = rep(0.0, max.length)
    current[1:max.length] = original[1:max.length + last.position]

    current = denoiseD4(current, 0)
    result[1:max.length + last.position] = current[1:max.length]

    last.position = last.position + max.length
    max.length = (2 ** floor(log(number.records - last.position, 2))) * (last.position != number.records)
  }

  return(result)
}

value.leaks <- function(original, perturbed) {
  m.original = mean(original)
  m.transformed = mean(perturbed)

  alpha = purrr::reduce2(original, perturbed, function(a, o, p) a + (o - m.original) * (p - m.transformed), .init=0.0)
  denominator = purrr::reduce(perturbed, function(d, p) d + (p - m.transformed) ** 2, .init=0.0)
  if (denominator == 0) return(perturbed * 0)
  alpha = alpha / denominator
  beta = m.original - alpha * m.transformed
  result = alpha * perturbed + beta
  return(result)
}

denoiseD4 <- function(ts, dummy) {
  N = length(ts)
  n = N
  while (n >= 4) {
    ts = transform(ts, n)
    n = floor(n / 2)
  }

  ts = remove.noise(ts, dummy)

  n = 4
  while (n <= N) {
    ts = inv.transform(ts, n)
    n = floor(n * 2)
  }

  return(ts)
}

remove.noise <- function(wavelets, dummy) {
  l = length(wavelets)
  values = sapply(wavelets[(floor(l/2) + 1):(l - floor(dummy/2))], function(x) abs(x))
  values = sort(values)
  mean = values[floor(length(values)/2 + 1)]
  wavelets = sapply(wavelets, function(x) x * (abs(x) >= mean))
  return(wavelets)
}

transform <- function(a, n) {
  
  sqrt.3 = sqrt(3)
  denom = 4 * sqrt(2)
  
  # Forward transform scaling coefficients
  h0 = (1 + sqrt.3) / denom
  h1 = (3 + sqrt.3) / denom
  h2 = (3 - sqrt.3) / denom
  h3 = (1 - sqrt.3) / denom
  
  # Forward transofrm wavelet coefficients
  g0 = h3
  g1 = -h2
  g2 = h1
  g3 = -h0
  
  # Inverse transform coefficients for smoothing values
  Ih0 = h2
  Ih1 = g2
  Ih2 = h0
  Ih3 = g0
  
  # Inverse transform for wavelet values
  Ig0 = h3
  Ig1 = g3
  Ig2 = h1
  Ig3 = g1
  
  if (n >= 4) {
    half = floor(n / 2)
    tmp = rep(0, n)
    i = 1
    j = 1

    while (j <= n - 3) {
      tmp[i] = a[j] * h0 + a[j + 1] * h1 + a[j + 2] * h2 + a[j + 3] * h3
      tmp[i + half] = a[j] * g0 + a[j + 1] * g1 + a[j + 2] * g2 + a[j + 3] * g3
      i = i + 1
      j = j + 2
    }

    tmp[i] = a[n - 1] * h0 + a[n] * h1 + a[1] * h2 + a[2] * h3
    tmp[i + half] = a[n - 1] * g0 + a[n] * g1 + a[1] * g2 + a[2] * g3
    a[1:n] = tmp
  }
  return(a)
}

inv.transform = function(a, n) {
  
  sqrt.3 = sqrt(3)
  denom = 4 * sqrt(2)
  
  # Forward transform scaling coefficients
  h0 = (1 + sqrt.3) / denom
  h1 = (3 + sqrt.3) / denom
  h2 = (3 - sqrt.3) / denom
  h3 = (1 - sqrt.3) / denom
  
  # Forward transofrm wavelet coefficients
  g0 = h3
  g1 = -h2
  g2 = h1
  g3 = -h0
  
  # Inverse transform coefficients for smoothing values
  Ih0 = h2
  Ih1 = g2
  Ih2 = h0
  Ih3 = g0
  
  # Inverse transform for wavelet values
  Ig0 = h3
  Ig1 = g3
  Ig2 = h1
  Ig3 = g1
  
  if (n >= 4) {
    half = floor(n / 2)
    half.pls1 = half + 1
    tmp = rep(0, n)

    tmp[1] = a[half] * Ih0 + a[n] * Ih1 + a[1] * Ih2 + a[half.pls1] * Ih3
    tmp[2] = a[half] * Ig0 + a[n] * Ig1 + a[1] * Ig2 + a[half.pls1] * Ig3

    j = 3
    i = 1
    while (i <= half - 1) {
      tmp[j] = a[i] * Ih0 + a[i + half] * Ih1 + a[i + 1] * Ih2 + a[i + half.pls1] * Ih3
      j = j + 1
      tmp[j] = a[i] * Ig0 + a[i + half] * Ig1 + a[i + 1] * Ig2 + a[i + half.pls1] * Ig3
      j = j + 1
      i = i + 1
    }
    a[1:n] = tmp
  }
  return(a)
}
