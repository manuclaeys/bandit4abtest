#'Returns the arm with the highest bound
#'
#'Calculates, for each colonne of S (selected arm) an
#'upper bound according to the Hoeffding inequality
#'(dependent on the iter iteration) via \code{\link{proba_max_For_UCB}} function
#'It is possible to adjust this bound via an
#'alpha parameter (default alpha = 1).
#'Returns the arm with the highest bound
#'See also \code{\link{proba_max_For_UCB}}
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
#'visitorReward <- as.data.frame( cbind(K1,K2) )
#'## Number of arms
#'K=2
#'## Init the S Matrix
#'S <- generate_Matrix_S(K)
#'S
#'## play arms uniformly
#'for(i in 1:nrow(visitorReward)){
#'S <- play_arm(i,arm=(i%%K+1),S,visitorReward)
#'}
#'## Results
#'S
#'proba_max_For_UCB(S=S,iter=i+1)
#'condition_For_UCB(S=S,iter=i+1)
#'@export
condition_For_UCB <- function(S,iter,alpha=1,K=ncol(S)){
  return(which.max(proba_max_For_UCB (S,iter,alpha,K)))
}
