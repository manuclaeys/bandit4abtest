#'condition_For_thompson_sampling
#'
#'Samples for each arm an average according to its probability distribution from the beta law (according to number of sucess and trials in S matrix)see function
#'see \code{\link{rbeta}}.
#'Give the arm with the highest average score.
#'Returns the best estimated arm and associated probability.
#'
#'@param S :numerical matrix of results
#'@param K :number of arm (optional)
#'@param alpha :a parameter (optional)
#'@param beta :a parameter (optional)
#'
#'@return list of choice (number of best arm) and probability (probablity to be the best)
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
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
#'## Choose next arm with thompson sampling policy
#'condition_For_thompson_sampling(S)
#'#Density
#'plot(density( rbeta(100, 1 +  S[1,1]*S[2,1], 1 + S[2,1] - S[1,1]*S[2,1])))
#'plot(density( rbeta(100, 1 +  S[1,2]*S[2,2], 1 + S[2,2] - S[1,2]*S[2,2])))
#'@export
condition_For_thompson_sampling  <- function(S, K=ncol(S), alpha=1,beta=1){

  temp.count <- list()
  temp.distrib <- list()

  for(i in 1:K) temp.count[i] <- 0
  for(j in 1:K){
    #Sample a mean from a beta distribution of means
    temp.distrib[j] <-  rbeta(1, alpha +  S[1,j]*S[2,j], beta + S[2,j] - S[1,j]*S[2,j])
  }

  return(list("choice" = which.max(temp.distrib)  ,"proba"=  max(unlist(temp.distrib))))
}
