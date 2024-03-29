% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oth_discr.R
\name{discr.together}
\alias{discr.together}
\title{Discretization of two numerical vectors}
\usage{
discr.together(
  lpo,
  lpm,
  num.bins = 20,
  bin.size = 0.01,
  mode = c("number", "size")
)
}
\arguments{
\item{lpo, lpm}{vectors to be quantized}

\item{num.bins}{number of bins}

\item{bin.size}{size of bins}

\item{mode}{if "number", load profile is split into num.bins of equidistant bins;
if "size", bin.size is used for defining the size of each bin.}
}
\value{
list, containing bin indexes discretized vectors lpo and lpm
}
\description{
The function takes two numerical vectors and quantizes them using the same bins
}
\examples{
set.seed(1)
tmp <- squares(20, c(1, 1, 1))
plot(discr.together(tmp[, 1], tmp[, 2], mode = "number", num.bins = 6))
discr.together(tmp[, 1], tmp[, 2], mode = "size")
}
\references{
for use case of fixed number of bins, see e.g., J. X. Chin, G. Giaconi, T. T. De Rubira, 
D. Gimduz, and G. Hug, "Considering time correlation in the estimation of privacy loss for 
consumers with smart meters," in 20th Power Systems Computation Conference, PSCC 2018, 2018;

for use of pre-defined step size of quantization, see G. Kalogridis, C. Efthymiou, S. Z. Denic, 
T. A. Lewis, and R. Cepeda, "Privacy for Smart Meters: Towards Undetectable Appliance Load 
Signatures," 2010 First IEEE International Conference on Smart Grid Communications, pp. 232-237, 2010.
}
