#'KL-UCB algorithm
#'
#'Kullback-Leibler Upper Confidence Bound (KL-UCB) bandit strategy.
#'Generate a matrix to save the results (S).

#'Returns the calculation time.
#'Review the estimated, actual averages and number of choices for each arm.
#'See also  \code{\link{ConditionForKLUCB}}, \code{\link{kl_bernoulli}}, \code{\link{kl_ucb_bernoulli}},
#' \code{\link{kl_ucb_gaussian}}, \code{\link{GenerateMatrixS}}, and \code{\link{PlayArm}}.
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param precision Numeric value (optional)
#'@param c Numeric value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item S:numerical matrix of results ,
#'  \item choice: choices of KL-UCB,
#'  \item proba: probability of the chosen arms,
#'  \item time: time of cumputation,
#'  \item theta_hat: mean estimated of each arm
#'  \item theta: real mean of each arm
#'  }
#'
#'
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame( cbind(K1,K2) )
#'klucb_alloc <- KLUCB(visitor_reward)
#'klucb_alloc$S
#'klucb_alloc$time
#'klucb_alloc$theta
#'klucb_alloc$theta_hat
#'@import tictoc
#'@export
#######  KL-UCB  ############

KLUCB <- function(visitor_reward, K = ncol(visitor_reward), precision=1e-6, c=0){
  
  choice <- c()
  indice <- c()
  S <- GenerateMatrixS(K)
  
  tic()
  
  for (i in 1:K){
    choice[i] <- i
    S <- PlayArm(iter=i, arm=i, S=S, visitor_reward)
  }
  
  for (j in (K+1):nrow(visitor_reward)){
    
    t=sum(S[2,])
    indice <- kl_ucb_bernoulli(S[1,], d=(log(t) + c*(log(log(t)))) / S[2,] , precision=precision, S=S,
                               max_iteration = 50, visitor_reward=visitor_reward[j,])
    choice[j] <- which.max(indice)
    S <- PlayArm(iter=j, arm=choice[j], S=S, visitor_reward)

  }
  
  time <- toc()

  #coef estimate
  th_hat = S[1,]
  
  #real coef
  th = colMeans(visitor_reward)
  
  message("th_hat")
  message(th_hat)
  message("th real")
  message(th)
  
  
  return(list('S'=S, 'time'=(time$toc - time$tic),'choice'= choice,'theta_hat'=th_hat,'theta'=th))
}
