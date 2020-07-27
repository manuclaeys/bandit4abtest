#'EXP3 algorithm
#'
#'Exponential  Weights  for  Exploration  and  Exploitation (EXP3) bandit strategy. Uses a list of weigths which evolve according
#'to arm's reward. The gamma parameter is a coefficient for balancing between exploitation and exploration.
#'Control data in visitor_reward with \code{\link{IsRewardAreBoolean}}
#'Stop if something is wrong.
#'Generate a matrix to save the results (S).
#' \itemize{ At each iteration
#'  \item Update weight parameter for each arm
#'  \item Choose randomly an arm according to the distribution of proba
#'  \item Receives a reward in visitor_reward for the arm and associated iteration
#'  \item Updates the results matrix S.
#'  }
#'Returns the calculation time.
#'Review the estimated, actual averages and number of choices for each arm.
#'See also  \code{\link{ConditionForEXP3}}, \code{\link{GenerateMatrixS}}, and \code{\link{PlayArm}}.
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param gamma Numeric value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item S:numerical matrix of results ,
#'  \item choice: choices of EXP3,
#'  \item proba: probability of the chosen arms,
#'  \item time: time of cumputation,
#'  \item theta_hat: mean estimated of each arm
#'  \item theta: real mean of each arm
#'  \item weight : weight coefficient of each arm
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
#'EXP3_alloc <- EXP3(visitor_reward)
#'EXP3_alloc$S
#'EXP3_alloc$time
#'EXP3_alloc$theta
#'EXP3_alloc$theta_hat
#'@import tictoc
#'@export
#######  EXP3  ############


EXP3 <- function(visitor_reward, K=ncol(visitor_reward), gamma=0.05){

  #Control
  BanditRewardControl(visitor_reward = visitor_reward, K = K)
  
  #Data formating
  visitor_reward <- as.matrix(visitor_reward)
  
  weight <- rep(1, times=K)
  proba <- rep(0, times=K)
  choice <- c()
  reward <- c()
  estimated_reward <- c()
  S <- GenerateMatrixS(K)
  
  tic()
  
  ###Initialisation

  for (h in 1:K) {
    
    weight_sum <- sum(weight)
    S <- PlayArm(iter=h, arm=h, S=S, visitor_reward)      
    proba[h] <- (1 - gamma) * (weight[h]/weight_sum) + (gamma/K)
    reward[h] <- visitor_reward[h,h]
   # print(proba)
   
    estimated_reward[h] <- reward[h]/proba[h]
    #update weight
    weight[h] <- weight[h]*exp(gamma*estimated_reward[h]/K)
    
    #print(S)
    choice[1:K] <- c(1:K)
    
  }

  
  for (i in (K+1):nrow(visitor_reward)) {
  
    for (j in 1:K){
      weight_sum <- sum(weight)
      proba[j] <- (1 - gamma) * (weight[j]/weight_sum) + (gamma/K)
     # print(proba)
    }

  choice[i] <- ConditionForEXP3(S=S, proba = proba)
  reward[i] <- visitor_reward[i,choice[i]]
  S <- PlayArm(iter=i, arm=choice[i], S, visitor_reward)
  
  estimated_reward[i] <- reward[i]/proba[choice[i]]

  #update weight
  weight[choice[i]] <- weight[choice[i]]*exp(gamma*estimated_reward[i]/K)
  
  }
  
  time <- toc()
  
  #coef estimate
  th_hat=S[1,]
  
  #real coef
  th = colMeans(visitor_reward)
  
  cat("th_hat", fill=2)
  cat(th_hat, fill=TRUE)
  cat("th real", fill=2)
  cat(th, fill=TRUE)
  
  return(list('S'=S,'proba'=proba, 'time'=(time$toc - time$tic),'choice'= choice,'theta_hat'=th_hat,'theta'=th, 'weight'=weight))
  
}

