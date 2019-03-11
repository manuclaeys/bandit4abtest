#'is_reward_are_boolean
#'
#'Check if a reward is defined as logical value.
#'Print a message and stop if this condition is not respected.
#'Else return TRUE.
#'
#'@param visitorReward Dataframe of integer or numeric values
#'
#'@return Logical value
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rnorm(100, 30, .05)
#'K2 <- rnorm(100, 35, .05)
#'## Define a dataframe of rewards
#'visitorReward <- as.data.frame(cbind(K1,K2))
#'is_reward_are_boolean(visitorReward)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'visitorReward <- as.data.frame(cbind(K1,K2))
#'is_reward_are_boolean(visitorReward)
#'
#'@export
is_reward_are_boolean <- function(visitorReward){
  for(i in 1:ncol(visitorReward)){
    if(typeof(visitorReward[,i]) != "integer" ){
      stop( paste("The",i,"colomn of reward data is not a integer/double data so it can be define as a boolean data",sep=" "))
      return(FALSE)
   }
  }
return(TRUE)
}
