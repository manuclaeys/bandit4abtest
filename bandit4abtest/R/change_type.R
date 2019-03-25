#'Change all colomns of a dataframe into
#'numerical type
#'
#'@param visitor_reward  a dataframe
#'
#'@return dataframe of numerical value
#'
#'@examples
#'## Generates 1000 numbers from 2 binomial distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame( cbind(as.character(K1),as.character(K2)) )
#'typeof(visitor_reward[,1])
#'## Change type
#'visitor_reward <- ChangeType(visitor_reward)
#'typeof(visitor_reward[,1])
#'@export
ChangeType <- function(visitor_reward) {
  for (i in 1:ncol(visitor_reward)) visitor_reward[,i] <- as.double(as.character(visitor_reward[,i]))
  return (visitor_reward)
}
