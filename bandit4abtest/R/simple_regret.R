#'Return list of regret
#'
#'Return a list with obtained regret at each iterations
#'
#'@param choice  Integer list
#'@param visitor_reward dataframe of integer or numeric values
#'
#'@return List of numeric values
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rnorm(100, 30, .05)
#'K2 <- rnorm(100, 21, .05)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#Random choices
#'choice <- sample(c(1,2), 100, replace = TRUE)
#'SimpleRegret(choice=choice,visitor_reward=visitor_reward)
#'
#'@export
SimpleRegret <- function(choice, visitor_reward) {
  regret <- c()
  for (i in 1:nrow(visitor_reward)) {
    regret[i] <- RegretValue(as.integer(choice[i]), visitor_reward[i,])
  }
  return (regret)
}
