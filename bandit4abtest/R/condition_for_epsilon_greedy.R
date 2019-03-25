#'condition_For_epsilonGreedy
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
#'condition_For_epsilonGreedy(S=S)
#'@export
condition_For_epsilonGreedy <- function(S,epsilon=0.25,K= ncol(S)){

  #choose the best with 1 - espsilon probability. 1 : best arm , 2 : other arm
  res <-  sample(c(1,2) , 1, replace = T, prob=c(1-epsilon,epsilon ))

  #the best one have been choose
  if(res==1) return(as.integer(which.max(S[1,])))

  #randomly select another arm
  return(sample(c(1:K)[-which.max(S[1,])] , 1))

}
