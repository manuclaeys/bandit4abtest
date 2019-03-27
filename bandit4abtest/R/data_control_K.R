#'DataControlK
#'
#'Control arm and data for bandit
#'Check if a dataframe gets an equal number of colonms than K possible arms.
#'Check if K geq 2. Print a message  and stop if this two conditions are not respected.
#'Else return TRUE.
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K  Integer value (optional)
#'
#'@return Logical value
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rnorm(100, 30, .05)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(K1)
#'## Control
#'#DataControlK(visitor_reward)
#'K2 <- rnorm(100, 21, .05)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'## Control
#'DataControlK(visitor_reward,K=2)
#'
#'@export
DataControlK <- function(visitor_reward, K=ncol(visitor_reward)) {
  #arm must be superior to 2
  if (K < 2) {
    stop("arm must be superior or equal to 2")
    return (FALSE)
  }

  #each arm get a result
  if (ncol(visitor_reward) != K) {
    stop("each arm need a result")
    return (FALSE)
  }
  return (TRUE)
}
