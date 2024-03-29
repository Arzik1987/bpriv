% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oth_shift.R
\name{shift}
\alias{shift}
\title{Shifts one TS relative to another TS}
\usage{
shift(ts1, ts2, n)
}
\arguments{
\item{ts1, ts2}{time series of the same length}

\item{n}{positions to shift}
}
\value{
matrix of size length(ts1) - n, whete ts1 is in the first column and ts2 dalayed n steps
is in the second. n rows are removed to exclude missing values
}
\description{
Shifts one time series relative to another time series given
}
\examples{
set.seed(1)
x <- squares(3)[, 1]
shift(x, x, 1)
}
\keyword{shift}
