#' Assign an arm to an item
#'
#' The variable arm represents the selected arm
#' visitor_reward is a dataframe of rewards
#' iter is the current iteration
#' S is the matrix of results for each arm (tests and empirical mean)
#' In the matrix S :
#' \itemize{ Retrieves the reward associated with the iter instant in the reward dataframe
#'  \item Updates the average reward of the chosen arm with the reward obtained
#'  \item Adds a test to the selected arm (arm)
#'  \item Returns the updated S matrix
#'  }
#'@param iter Integer value
#'@param arm  Integer value
#'@param S  Numeric matrix
#'@param visitor_reward  Numeric matrix
#'
#'@return S Numeric matrix
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame( cbind(K1,K2) )
#'## Number of arms
#'K=2
#'## Init the S Matrix
#'S <- GenerateMatrixS(K)
#'S
#'## play arms uniformly
#'for(i in 1:nrow(visitor_reward)){
#'S <- PlayArm(i,arm=(i%%K+1),S,visitor_reward)
#'}
#'## Results
#'S
#'@export
PlayArm <- function(iter, arm, S, visitor_reward) {
  #mean
  S[1,arm] <- ((S[1,arm] * S[2,arm] + visitor_reward[iter,arm]) / (S[2,arm] + 1))
  #play
  S[2,arm] = S[2,arm] + 1
  return (S)
}
