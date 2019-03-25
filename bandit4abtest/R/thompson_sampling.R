#'ThompsonSampling
#'
#'A thompson sampling (TS) bandit strategy implemented by sampling, in each round, averages from a posterior
#'distribution  \code{\link{ConditionForThompsonSampling}}, and choosing the action that maximizes the expected reward given the
#'sampled average. Conceptually, this means that the player instantiates their beliefs
#'randomly in each round, and then acts optimally according to them.
#'Control data in visitor_reward with \code{\link{IsRewardAreBoolean}}
#'Stop if something is wrong.
#'Generate a matrix to save the results (S).
#' \itemize{ At each iteration
#'  \item Sample an averages from a posterior in S for each arm (beta distribution with alpha and beta parameters)
#'  \item Choose the arm with the highest average
#'  \item Receives a reward in visitor_reward for the arm and associated iteration
#'  \item Updates the results matrix S.
#'  }
#'Returns the calculation time.
#'Review the estimated, actual averages and number of choices for each arm.
#'See also  \code{\link{ConditionForThompsonSampling}}, \code{\link{GenerateMatrixS}}, and \code{\link{PlayArm}}.
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
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
#'visitor_reward <- as.data.frame( cbind(K1,K2) )
#'ThompsonSampling(visitor_reward)
#'@import tictoc
#'@export
#######  ThompsonSampling  ############
ThompsonSampling  <- function(visitor_reward, K=ncol(visitor_reward), alpha=1, beta=1) {
  #control data
  IsRewardAreBoolean(visitor_reward)

  #data formating
  visitor_reward <- as.matrix(visitor_reward)

  #keep list of choice
  choice <- c()
  #keep the list of probability
  proba <- c()

  S <- GenerateMatrixS(K)

  tic()

  if (K >= nrow(visitor_reward)) {

    warning(" more arm than visitors !")

    for (j in 1:nrow(visitor_reward)) {
      S <- PlayArm(j, j, S, visitor_reward)        #handle case where there is more arm than visitors
    }

    choice[1:nrow(visitor_reward)] <- c(1:nrow(visitor_reward))
    proba[1:nrow(visitor_reward)] <- 1/K

    if (K > nrow(visitor_reward)) {
      S[,c(j:K) ] <- 0
    }

    return (list('S'=S,'choice'= choice))

  } else {

    # initialisation
    for (j in 1:K) {
      S <- PlayArm(iter=j, arm=j, S=S, visitor_reward)
      choice[1:K] <- c(1:K)
      proba[1:K] <- 1/K
    }

    for (i in (K+1):nrow(visitor_reward)) {
      #sample an average from posterior distribution
      temp <- ConditionForThompsonSampling(S, K, alpha=alpha, beta=beta)
      #save the choosen arm
      choice[i] <- temp$choice
      #save probability sampled
      proba[i] <- temp$proba
      #remove temporal variable
      rm(temp)
      #update S
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

    return (list('S'=S,'choice'= choice, 'proba' = proba,'time'=(time$toc - time$tic),'theta_hat'=th_hat,'theta'=th))

  }
}
