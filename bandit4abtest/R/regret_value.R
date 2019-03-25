#'Return regret of chosen arm
#'
#'Return for a given vector the difference between
#'the highest value of reward and the reward obtained by the chosen arm.
#'Can be equal of 0
#'
#'@param arm  Integer value
#'@param vec_visitorReward Numeric vector
#'
#'@return Numeric value
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rnorm(100, 30, .05)
#'K2 <- rnorm(100, 21, .05)
#'## Define a dataframe of rewards
#'visitorReward <- as.data.frame(cbind(K1,K2) )
#'#regret of arm 1 for the fist item
#'regretValue(1,visitorReward[1,])
#'#'#regret of arm 1 for the fist item
#'regretValue(2,visitorReward[1,])
#'@export
regretValue <- function(arm,vec_visitorReward){
  return( as.numeric(vec_visitorReward[which.max(vec_visitorReward)] - vec_visitorReward[arm] ))
}
