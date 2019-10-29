#' Cluster similarity
#'
#' Calculates privacy measure based on cluster similarity.
#' @param lpo,lpm original and modified load profiles
#' @param n number of clusters used. Should be greater than 2. If not given an optimized number will be computed.
#' @return privacy measure: cluster similarity. If lpm is constant, the similarity is zero.
#' @export
#' @references Kalogridis, Georgios, et al. "Elecprivacy: Evaluating the privacy protection of electricity 
#' management algorithms." IEEE Transactions on Smart Grid 2.4 (2011): 750-758.
#' @examples
#' set.seed(1)
#' d <- squares(1000)
#' lpo <- d[, 1]
#' lpm <- d[, 2]
#' priv.cs(lpo, lpm)
#' priv.cs(lpo, lpm, n = 20)
#' 
#' lpo <- c(1, 1.5, 1.9, 2.1, 2, 1.8, 1.5, 1.5, 1.7, 1.9, 2, 1.9, 1.7, 1.5, 1.2, 1)
#' lpm <- c(0.7, 2, 1.5, 2.5, 1.6, 1.7, 1.4, 1, 1.9, 1.4, 2.1, 2.7, 1.8, 2.2, 0.6, 1.5)
#' priv.cs(lpo, lpm)

priv.cs <- function(lpo, lpm, n = -1){
  if (length(unique(lpm)) == 1){
    return(0)
  }
  
  if(n < 2 & n != -1){
    stop("n should be greater than 1")
  }

  if(n == -1){
    nmax <- min(length(lpm) - 1, 25)
    test.range <- c(3:nmax)
    avg.sil <- function(n){
      km.res <- cluster::clara(as.matrix(lpo), k = n)
      ss <- cluster::silhouette(km.res)
      mean(ss[, 3])
    }
    avg.sil.vals <- sapply(test.range, avg.sil)
    # factoextra::fviz_nbclust(as.matrix(lpo), clara). # check correctness
    
    n <- test.range[which(avg.sil.vals == max(avg.sil.vals))]
    if(n > length(unique(lpm))){
      n = length(unique(lpm))
    }
  }
  
  tmp = 0
  while(tmp == 0){
    if(n == 1){
      tmp = 1
      return(-1)
    }
    y.cl <- cluster::clara(lpm, k = n)
    if(length(y.cl$medoids) != length(unique(y.cl$medoids))){
      n = n - 1
    } else {
      tmp = 1
    }
  }
  
  x.cl <- cluster::clara(lpo, k = n)
  x.sorted.centers <- sort(x.cl$medoids)
  x.sorted.index <- sapply(x.sorted.centers, function(x) which(x.cl$medoids == x))

  y.cl <- cluster::clara(lpm, k = n)
  y.sorted.centers <- sort(y.cl$medoids)
  y.sorted.index <- sapply(y.sorted.centers, function(x) which(y.cl$medoids == x))

  numerator <- mapply(function(x, y) (which(x == x.sorted.index) == which(y == y.sorted.index)), x.cl$cluster, y.cl$cluster)
  ind.num <- !((x.cl$cluster == x.sorted.index[1]) | (y.cl$cluster == y.sorted.index[1]))
  numerator <- sum(numerator[ind.num])
  
  denominator <- sum(!(x.cl$cluster == x.sorted.index[1]))
  
  res <- numerator/denominator
  
  return(res)
}



