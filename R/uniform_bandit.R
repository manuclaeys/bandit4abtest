#'Uniform algorithm
#'
#'Control data in visitor_reward with \code{\link{BanditRewardControl}}
#'Stop if something is wrong.
#'Generate a matrix to save the results (S).
#' \itemize{ At each iteration
#'  \item Choose alternatively an arm
#'  \item Receives a reward in visitor_reward for the arm and associated iteration
#'  \item Updates the results matrix S.
#'  }
#'Returns the calculation time.
#'Return the estimated and number of choices for each arm.
#'See also  \code{\link{GenerateMatrixS}} and \code{\link{PlayArm}}.
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item S:numerical matrix of results ,
#'  \item choice: choices of UCB,
#'  \item time: time of cumputation,
#'  }
#'
#'@examples
#'## Generates 10000 numbers from 2 binomial  distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#Run Uniform algorithm
#'uniform_bandit_alloc  <- UniformBandit(visitor_reward)
#'uniform_bandit_alloc$S
#'uniform_bandit_alloc$time
#'@import tictoc
#'@export
#UniformBandit
UniformBandit <- function(visitor_reward, K=ncol(visitor_reward)) {

  #control
  BanditRewardControl(visitor_reward = visitor_reward, K = K)

  #data formating
  visitor_reward <- as.matrix(visitor_reward)

  #keep list of choice
  choice <- c()
  S <- GenerateMatrixS(K)

  tic()

  if (K > nrow(visitor_reward)) {
    warning(" more arm than visitors !")

    for (j in 1:nrow(visitor_reward)) {
      S <- PlayArm(j,j,S)        #handle case where there is more arm than visitors
    }
    choice[1:nrow(visitor_reward)] <- c(1:nrow(visitor_reward))
    return (list('S'=S,'choice'= choice))

  } else {

    #play an arm alternatively
      for (i in 1:nrow(visitor_reward)) {
        choice[i] <- ((i-1)%%K + 1)
        S <- PlayArm(i, arm=(i%%K+1), S, visitor_reward)
      }
    time <- toc()

    return (list('S'=S,'choice'= choice,'time'=(time$toc - time$tic)))

  }
}
