#'thompson_sampling
#'
#'A thompson sampling (TS) bandit strategy implemented by sampling, in each round, averages from a posterior
#'distribution  \code{\link{condition_For_thompson_sampling}}, and choosing the action that maximizes the expected reward given the
#'sampled average. Conceptually, this means that the player instantiates their beliefs
#'randomly in each round, and then acts optimally according to them.
#'Control data in visitorReward with \code{\link{is_reward_are_boolean}}
#'Stop if something is wrong.
#'Generate a matrix to save the results (S).
#' \itemize{ At each iteration
#'  \item Sample an averages from a posterior in S for each arm (beta distribution with alpha and beta parameters)
#'  \item Choose the arm with the highest average
#'  \item Receives a reward in visitorReward for the arm and associated iteration
#'  \item Updates the results matrix S.
#'  }
#'Returns the calculation time.
#'Review the estimated, actual averages and number of choices for each arm.
#'See also  \code{\link{condition_For_thompson_sampling}}, \code{\link{generate_Matrix_S}}, and \code{\link{play_arm}}.
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitorReward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'@param beta Numeric value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item S:numerical matrix of results ,
#'  \item choice: choices of TS,
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
#'visitorReward <- as.data.frame( cbind(K1,K2) )
#'thompson_sampling(visitorReward)
#'@export
#######  thompson_sampling  ############
thompson_sampling  <- function(visitorReward, K=ncol(visitorReward), alpha=1,beta=1){
  #control data
  is_reward_are_boolean(visitorReward)
  #return time elaps
  library(tictoc)

  #data formating
  visitorReward <- as.matrix(visitorReward)

  #keep list of choice
  choice <- c()
  #keep the list of probability
  proba <- c()

  S <- generate_Matrix_S(K)

  tic()

  if (K >= nrow(visitorReward)) {

    warning(" more arm than visitors !")

    for(j in 1:nrow(visitorReward)){
      S <- play_arm(j,j,S,visitorReward)        #handle case where there is more arm than visitors
    }

    choice[1:nrow(visitorReward)] <- c(1:nrow(visitorReward))
    proba[1:nrow(visitorReward)] <- 1/K

    if (K>nrow(visitorReward)) {
      S[,c(j:K) ] <- 0
    }

    return(list('S'=S,'choice'= choice))

  }else{

    # initialisation
    for(j in 1:K){
      S <- play_arm(iter=j,arm=j,S=S,visitorReward)
      choice[1:K] <- c(1:K)
      proba[1:K] <- 1/K
    }

    for(i in (K+1):nrow(visitorReward)){
      #sample an average from posterior distribution
      temp <- condition_For_thompson_sampling(S,K,alpha=alpha,beta=beta)
      #save the choosen arm
      choice[i] <- temp$choice
      #save probability sampled
      proba[i] <- temp$proba
      #remove temporal variable
      rm(temp)
      #update S
      S <- play_arm(iter=i,arm=choice[i] ,S,visitorReward)
    }

    time <- toc()

    #coef estimate
    th_hat=S[1,]

    #real coef
    th = colMeans(visitorReward)

    message("th_hat")
    message(th_hat)
    message("th real")
    message(th)

    return(list('S'=S,'choice'= choice, 'proba' = proba,'time'=(time$toc - time$tic),'theta_hat'=th_hat,'theta'=th))

  }
}
