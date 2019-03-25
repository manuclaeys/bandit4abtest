#'Calculates, for each colonne of S (selected arm) an
#'upper bound according to the Hoeffding inequality
#'(dependent on the iter iteration).
#'It is possible to adjust this bound via an
#'alpha parameter (default alpha = 1).
#'Returns a vector of calculated upper bounds
#'
#'@param S  Numeric matrix
#'@param iter Integer value
#'@param alpha  Numeric value (optional)
#'@param K Integer value (optional)
#'
#'@return Numeric vector
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame( cbind(K1,K2) )
#'## Number of arms
#'K=2
#'## Init the S Matrix
#'S <- GenerateMatrixS(K)
#'S
#'## play arms uniformly
#'for(i in 1:nrow(visitor_reward)){
#'S <- PlayArm(i,arm=(i%%K+1),S,visitor_reward)
#'}
#'## Results
#'S
#'ProbaMaxForUCB(S=S,iter=i+1)
#'@export
ProbaMaxForUCB <- function(S, iter, alpha=1, K=ncol(S)) {
  choice <- c()
  for (j in 1:K) choice[j] <- S[1,j] + alpha * sqrt( (2*log(iter))/S[2,j])
  return (choice)
}
