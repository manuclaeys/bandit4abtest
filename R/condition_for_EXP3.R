#'ConditionForEXP3
#'
#'Choose randomly the next arm to play according to the list proba
#'
#'@param S  Numeric matrix
#'@param K Integer value (optional)
#'@param proba Numeric list
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
#'ConditionForEXP3(S=S, proba = list(0.2, 0.8))
#'@export

ConditionForEXP3 <- function(S, K=ncol(S), proba) {
  
  return(sample(1:K, size=1, replace=TRUE, prob = proba))
  
} 

