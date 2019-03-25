#'Return regret of chosen arm
#'
#'Return for a given vector the difference between
#'the highest value of reward and the reward obtained by the chosen arm.
#'Can be equal of 0
#'
#'@param arm  Integer value
#'@param vec_visitor_reward Numeric vector
#'
#'@return Numeric value
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rnorm(100, 30, .05)
#'K2 <- rnorm(100, 21, .05)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#regret of arm 1 for the fist item
#'RegretValue(1,visitor_reward[1,])
#'#'#regret of arm 1 for the fist item
#'RegretValue(2,visitor_reward[1,])
#'@export
RegretValue <- function(arm, vec_visitor_reward) {
  return (as.numeric(vec_visitor_reward[which.max(vec_visitor_reward)] - vec_visitor_reward[arm]))
}
