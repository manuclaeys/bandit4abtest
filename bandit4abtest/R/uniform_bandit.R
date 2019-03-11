#'Uniform algorithm
#'
#'Control data in visitorReward with \code{\link{bandit_reward_control}}
#'Stop if something is wrong.
#'Generate a matrix to save the results (S).
#' \itemize{ At each iteration
#'  \item Choose alternatively an arm
#'  \item Receives a reward in visitorReward for the arm and associated iteration
#'  \item Updates the results matrix S.
#'  }
#'Returns the calculation time.
#'Return the estimated and number of choices for each arm.
#'See also  \code{\link{generate_Matrix_SK}} and \code{\link{play_arm}}.
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitorReward Dataframe of integer or numeric values
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
#'visitorReward <- as.data.frame(cbind(K1,K2) )
#'#Run Uniform algorithm
#'uniform_bandit_alloc  <- uniform_bandit(visitorReward)
#'uniform_bandit_alloc$S
#'uniform_bandit_alloc$time

#'@export
#uniform_bandit
uniform_bandit <- function(visitorReward, K=ncol(visitorReward) ){

  #control
  bandit_reward_control(visitorReward = visitorReward, K= K)

  #return time elaps
  library(tictoc)

  #data formating
  visitorReward <- as.matrix(visitorReward)

  #keep list of choice
  choice <- c()
  S <- generate_Matrix_S(K)

  tic()

  if(K > nrow(visitorReward)){
    warning(" more arm than visitors !")

    for(j in 1:nrow(visitorReward)){
      S <- play_arm(j,j,S)        #handle case where there is more arm than visitors

    }
    choice[1:nrow(visitorReward)] <- c(1:nrow(visitorReward))
    return(list('S'=S,'choice'= choice))

  }else{

    #play an arm alternatively
      for(i in 1:nrow(visitorReward)){
        choice[i] <- ((i-1)%%K + 1)
        S <- play_arm(i,arm=(i%%K+1),S,visitorReward)
      }
    time <- toc()

    return(list('S'=S,'choice'= choice,'time'=(time$toc - time$tic)))

  }
}
