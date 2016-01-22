separability.measures <- function ( Vector.1 , Vector.2 ) {
  # convert vectors to matrices in case they are not
  Matrix.1 <- as.matrix (Vector.1)
  Matrix.2 <- as.matrix (Vector.2)
  # define means
  mean.Matrix.1 <- colMeans ( Matrix.1 )
  mean.Matrix.2 <- colMeans ( Matrix.2 )
  # define difference of means
  mean.difference <- mean.Matrix.1 - mean.Matrix.2
  # define covariances for supplied matrices
  cv.Matrix.1 <- cov ( Matrix.1 )
  cv.Matrix.2 <- cov ( Matrix.2 )
  # define the halfsum of cv's as "p"
  p <- ( cv.Matrix.1 + cv.Matrix.2 ) / 2
  # --%<------------------------------------------------------------------------
  # calculate the Bhattacharryya index
  bh.distance <- 0.125 *t ( mean.difference ) %*% p^ ( -1 ) %*% mean.difference +
    0.5 * log (det ( p ) / sqrt (det ( cv.Matrix.1 ) * det ( cv.Matrix.2 )
    )
    )
  # --%<------------------------------------------------------------------------
  # calculate Jeffries-Matusita
  # following formula is bound between 0 and 2.0
  jm.distance <- 2 * ( 1 - exp ( -bh.distance ) )
  # also found in the bibliography:
  # jm.distance <- 1000 * sqrt (   2 * ( 1 - exp ( -bh.distance ) )   )
  # the latter formula is bound between 0 and 1414.0
  # --%<------------------------------------------------------------------------
  # calculate the divergence
  # trace (is the sum of the diagonal elements) of a square matrix
  trace.of.matrix <- function ( SquareMatrix ) {
    sum ( diag ( SquareMatrix ) ) }
  # term 1
  divergence.term.1 <- 1/2 * trace.of.matrix (( cv.Matrix.1 - cv.Matrix.2 ) %*% 
                                                ( cv.Matrix.2^ (-1) - cv.Matrix.1^ (-1) )
  )
  # term 2
  divergence.term.2 <- 1/2 * trace.of.matrix (( cv.Matrix.1^ (-1) + cv.Matrix.2^ (-1) ) %*%
                                                ( mean.Matrix.1 - mean.Matrix.2 ) %*%
                                                t ( mean.Matrix.1 - mean.Matrix.2 )
  )
  # divergence
  divergence <- divergence.term.1 + divergence.term.2
  # --%<------------------------------------------------------------------------
  # and the transformed divergence
  transformed.divergence  <- 2 * ( 1 - exp ( - ( divergence / 8 ) ) )
  indices <- data.frame(
    jm=jm.distance,bh=bh.distance,div=divergence,tdiv=transformed.divergence)
  return(indices)
}


# OTHER IMPLEMENTATION from http://stats.stackexchange.com/questions/106325/jeffries-matusita-distance-for-14-variables
# faster to test several variable  
#
# Compute the Mahalanobis distance between two vectors.
#
mahalanobis <- function(m1, m2, sigma) {m <- m1 - m2; m %*% solve(sigma, m)}
#
# Compute the Bhattacharyya distance between two multivariate normal distributions
# given by their means and covariance matrices.
#
bhattacharyya <- function(m1, s1, m2, s2) {
  d <- function(u) determinant(u, logarithm=TRUE)$modulus # Log determinant of matrix u
  s <- (s1 + s2)/2                                        # mean covariance matrix
  mahalanobis(m1, m2, s)/8 + (d(s) - d(s1)/2 - d(s2)/2)/2
}
#
# Re-express the Bhattacharyya distance as the Jeffries-Matusita distance.
#
jeffries.matusita <- function(...) sqrt(2*(1-exp(-bhattacharyya(...))))


#------------------------------------------------------------------------------------#
#
# Given a set of bands (as the columns of x) and a grouping variable in `class`,
# compute the class means and covariance matrices (the "signatures").
#
#classes.stats <- by(df[, 2:4], df$CROP, function(y) list(mean=apply(y, 2, mean), cov=cov(y)))
#------------------------------------------------------------------------------------#
#
# Compute the J-M distances between the classes.
#
#distances <- matrix(0.0, length(unique(df$CROP)), length(unique(df$CROP)))
#colnames(distances)<-unique(df$CROP)
#rownames(distances)<-unique(df$CROP)
#
#for (i in 2:length(unique(df$CROP))) {
#  m1 <- classes.stats[[i]]$mean; s1 <- classes.stats[[i]]$cov
#  for (j in 1:(i-1)) {
#    m2 <- classes.stats[[j]]$mean; s2 <- classes.stats[[j]]$cov
#    distances[i,j] <- distances[j,i] <- jeffries.matusita(m1,s1,m2,s2)
# }
#}
#print(distances)
