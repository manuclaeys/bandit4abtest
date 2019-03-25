#' Assign an arm to an item
#'
#' The variable arm represents the selected arm
#' visitorReward is a dataframe of rewards
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
#'@param visitorReward  Numeric matrix
#'
#'@return S Numeric matrix
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
#'@export
play_arm <- function(iter,arm,S,visitorReward){
  #mean
  S[1,arm] <- ((  S[1,arm] *  S[2,arm] + visitorReward[iter,arm] ) /  ( S[2,arm] + 1 ) )
  #play
  S[2,arm] =  S[2,arm] + 1
  return(S)
}

