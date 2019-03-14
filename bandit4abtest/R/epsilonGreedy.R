#'epsilonGreedy algorithm
#'
#'Control data in visitorReward with \code{\link{bandit_reward_control}}
#'Stop if something is wrong.
#'Generate a matrix to save the results (S).
#'At each iteration play the best arm with a probability of 1-epsilon and
#'other arm with probability epsilon
#'Returns the calculation time.
#'Return the estimated and actual averages and number of choices for each arm.
#'See also \code{\link{condition_epsilonGreedy}}, \code{\link{generate_Matrix_SK}},
#'and \code{\link{play_arm}}.
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitorReward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param epsilon Numeric value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item S:numerical matrix of results ,
#'  \item choice: choices of epsilonGreedy,
#'  \item time: time of cumputation,
#'  \item theta_hat: mean estimated of each arm
#'  \item theta: real mean of each arm
#'  }
#'
#'@examples
#'## Generates 10000 numbers from 2 binomial  distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitorReward <- as.data.frame(cbind(K1,K2) )
#'#Run epsilon Greedy algorithm
#'epsilonGreedy_alloc  <- epsilonGreedy(visitorReward,epsilon  = 0.25)
#'epsilonGreedy_alloc$S
#'barplot(table(epsilonGreedy_alloc$choice),main = "Histogram of choices",xlab="arm")
#'epsilonGreedy_alloc$time
#'epsilonGreedy_alloc$theta_hat
#'epsilonGreedy_alloc$theta
#'@import tictoc
#'@export
#######espilon-greedy############
epsilonGreedy <- function(visitorReward, K=ncol(visitorReward) ,epsilon  = 0.25){

  #control
  bandit_reward_control(visitorReward = visitorReward, K= K)

  #data formating
  visitorReward <- as.matrix(visitorReward)

  #keep list of choice
  choice <- c()
  S <- generate_Matrix_S(K)
  tic()

  if(K >= nrow(visitorReward)){
    warning(" more arm than visitors !")

    for(j in 1:nrow(visitorReward)){
      S <- play_arm(j,j,S,visitorReward)        #handle case where there is more arm than visitors

    }
    choice[1:nrow(visitorReward)] <- c(1:nrow(visitorReward))

    if(K>nrow(visitorReward)){
      S[,c(j:K) ] <- 0
    }

    return(list('S'=S,'choice'= choice))

  }else{

    ###initialisation
    for(j in 1:K){

      S <- play_arm(iter=j,arm=j,S=S,visitorReward)         #check if an arm have already been played
      # message(S)
      choice[1:K] <- c(1:K)

    }

    for(i in (K+1):nrow(visitorReward)){
      #    message(S)
      choice[i] <- condition_For_epsilonGreedy(S,epsilon)
      #   message( choice[i])
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

    return(list('S'=S,'choice'= choice,'time'=(time$toc - time$tic),'theta_hat'=th_hat,'theta'=th))

  }
}



