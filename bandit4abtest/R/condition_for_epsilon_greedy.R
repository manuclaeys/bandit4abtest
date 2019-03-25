#'ConditionForEpsilonGreedy
#'
#'choose the best with 1 - espsilon probability. 1 : best arm , 2 : other arm
#'
#'@param S  Numeric matrix
#'@param epsilon  Numeric value (optional)
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
#'ConditionForEpsilonGreedy(S=S)
#'@export
ConditionForEpsilonGreedy <- function(S, epsilon=0.25, K=ncol(S)) {

  #choose the best with 1 - espsilon probability. 1 : best arm , 2 : other arm
  res <- sample(c(1,2), 1, replace = T, prob = c(1-epsilon,epsilon ))

  #the best one have been choose
  if (res==1) return (as.integer(which.max(S[1,])))

  #randomly select another arm
  return (sample(c(1:K)[-which.max(S[1,])] , 1))
}
