% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/priv_mi_knn.R
\name{mi.mns.knn}
\alias{mi.mns.knn}
\title{kNN MI estimator for non-stationary Markov processes}
\usage{
mi.mns.knn(lpo, lpm, k, features)
}
\arguments{
\item{lpo, lpm}{original and modified load profiles}

\item{k}{number of nearest neighbor to consider}

\item{features}{vector of feature values (integers)}
}
\value{
mutual information value
}
\description{
computes mutual information between two load profiles (i.e., time series)
under the assumtion of the non-stationary Markov
process with features
}
\examples{
set.seed(1)
d <- squares(25)
features <- rep(1:10, each = 10)
mi.mns.knn(d[, 1], d[, 2], k = 2, features)
features <- rep(1:4, each = 25)
mi.mns.knn(d[, 1], d[, 2], k = 2, features)

features <- rep(1:4, each = 4)
lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
mi.mns.knn(lpo, lpm, k = 1, features)
mi.mns.knn(lpo, lpm, k = 2, features)
}
