#'Change all colomns of a dataframe into
#'numerical type
#'
#'@param visitorReward  a dataframe
#'
#'@return dataframe of numerical values
#'
#'@examples
#'## Generates 1000 numbers from 2 binomial distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitorReward <- as.data.frame( cbind(as.character(K1),as.character(K2)) )
#'typeof(visitorReward[,1])
#'## Change type
#'visitorReward <- change_type(visitorReward)
#'typeof(visitorReward[,1])
#'@export
change_type <- function(visitorReward){
  for(i in 1:ncol(visitorReward)) visitorReward[,i] <- as.double(as.character(visitorReward[,i]))
  return(visitorReward)
}
