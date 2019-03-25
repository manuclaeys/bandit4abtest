#'DataControlContextReward
#'
#'Control if number of item in data reward and context data are equal
#'Print a message and stop if this condition is not respected.
#'Else return TRUE.
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param dt  Dataframe of integer numeric or factor values
#'
#'@return Logical value
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rnorm(100, 30, .05)
#'K2 <- rnorm(100, 35, .05)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(K1)
#'## Define a dataframe of context
#'c1 <- rnorm(50, 35, .05)
#'dt <- as.data.frame(c1)
#'## Control
#'DataControlContextReward(dt=dt,visitor_reward=visitor_reward)
#'c1 <- rnorm(100, 30, .05)
#'dt <- as.data.frame(c1)
#'DataControlContextReward(dt=dt,visitor_reward=visitor_reward)
#'
#'@export
DataControlContextReward <- function(dt, visitor_reward) {
  #Match size controle
  if (nrow(dt) != nrow(visitor_reward)) {
    stop("number of row in contextual data and rewards data are not equals")
    return (FALSE)
  }
  return (TRUE)
}
