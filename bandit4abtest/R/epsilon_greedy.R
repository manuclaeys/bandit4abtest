#'EpsilonGreedy algorithm
#'
#'Control data in visitor_reward with \code{\link{BanditRewardControl}}
#'Stop if something is wrong.
#'Generate a matrix to save the results (S).
#'At each iteration play the best arm with a probability of 1-epsilon and
#'other arm with probability epsilon
#'Returns the calculation time.
#'Return the estimated and actual averages and number of choices for each arm.
#'See also \code{\link{ConditionForEpsilonGreedy}}, \code{\link{GenerateMatrixS}},
#'and \code{\link{PlayArm}}.
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param epsilon Numeric value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item S:numerical matrix of results ,
#'  \item choice: choices of EpsilonGreedy,
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
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#Run epsilon Greedy algorithm
#'epsilon_greedy_alloc  <- EpsilonGreedy(visitor_reward,epsilon  = 0.25)
#'epsilon_greedy_alloc$S
#'barplot(table(epsilon_greedy_alloc$choice),main = "Histogram of choices",xlab="arm")
#'epsilon_greedy_alloc$time
#'epsilon_greedy_alloc$theta_hat
#'epsilon_greedy_alloc$theta
#'@import tictoc
#'@export
#######espilon-greedy############
EpsilonGreedy <- function(visitor_reward, K=ncol(visitor_reward), epsilon=0.25) {

  #control
  BanditRewardControl(visitor_reward = visitor_reward, K = K)

  #data formating
  visitor_reward <- as.matrix(visitor_reward)

  #keep list of choice
  choice <- c()
  S <- GenerateMatrixS(K)
  tic()

  if (K >= nrow(visitor_reward)) {
    warning(" more arm than visitors !")

    for (j in 1:nrow(visitor_reward)) {
      S <- PlayArm(j, j, S, visitor_reward)        #handle case where there is more arm than visitors
    }
    choice[1:nrow(visitor_reward)] <- c(1:nrow(visitor_reward))

    if (K>nrow(visitor_reward)) {
      S[,c(j:K) ] <- 0
    }

    return(list('S'=S,'choice'= choice))

  } else {

    ###initialisation
    for (j in 1:K) {

      S <- PlayArm(iter=j, arm=j, S=S, visitor_reward)         #check if an arm have already been played
      # message(S)
      choice[1:K] <- c(1:K)

    }

    for (i in (K+1):nrow(visitor_reward)) {
      # message(S)
      choice[i] <- ConditionForEpsilonGreedy(S, epsilon)
      # message( choice[i])
      S <- PlayArm(iter=i, arm=choice[i], S, visitor_reward)

    }

    time <- toc()

    #coef estimate
    th_hat=S[1,]

    #real coef
    th = colMeans(visitor_reward)

    message("th_hat")
    message(th_hat)
    message("th real")
    message(th)

    return(list('S'=S,'choice'= choice,'time'=(time$toc - time$tic),'theta_hat'=th_hat,'theta'=th))

  }
}
