#'Returns the arm with the highest bound
#'
#'Calculates, for each colonne of S (selected arm) an
#'upper bound according to the Hoeffding inequality
#'(dependent on the iter iteration) via \code{\link{ProbaMaxForUCB}} function
#'It is possible to adjust this bound via an
#'alpha parameter (default alpha = 1).
#'Returns the arm with the highest bound
#'See also \code{\link{ProbaMaxForUCB}}
#'
#'@param S  Numeric matrix
#'@param iter Integer value (optional)
#'@param alpha  Numeric value (optional)
#'@param K Integer value (optional)
#'
#'@return Integer value
#'
#'@examples
#'## Generates 1000 numbers from 2 binomial distributions
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
#'ConditionForUCB(S=S,iter=i+1)
#'@export
ConditionForUCB <- function(S, iter, alpha=1, K=ncol(S)) {
  return (which.max(ProbaMaxForUCB(S, iter, alpha, K)))
}
