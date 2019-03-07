#'data_control_K
#'
#'Control arm and data for bandit
#'Check if a dataframe gets an equal number of colonms than K possible arms.
#'Check if K \geq 2. Print a message  and stop if this two conditions are not respected.
#'Else return TRUE.
#'
#'@param visitorReward Dataframe of integer or numeric values
#'@param K  Integer value (optional)
#'
#'@return Logical value
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rnorm(100, 30, .05)
#'## Define a dataframe of rewards
#'visitorReward <- as.data.frame(K1)
#'## Control
#'data_control_K(visitorReward)
#'K2 <- rnorm(100, 21, .05)
#'## Define a dataframe of rewards
#'visitorReward <- as.data.frame(cbind(K1,K2) )
#'## Control
#'data_control_K(visitorReward,K=3)
#'## Control
#'data_control_K(visitorReward,K=2)
#'
#'@export
data_control_K <- function( visitorReward, K=ncol(visitorReward)){
  #arm must be superior to 2
  if(K<2){
    stop("arm must be superior or equal to 2")
    return(FALSE)
    }

  #each arm get a result
  if(ncol(visitorReward) != K){
    stop("each arm need a result")
    return(FALSE)
    }
  return(TRUE)
}

