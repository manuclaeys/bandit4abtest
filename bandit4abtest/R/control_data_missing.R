#'ControlDataMissing
#'
#'Control data for bandit.
#'Check in a dataframe if there is some missing values
#'Print a message if it's not respected.
#'Else return TRUE.
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'
#'@return Logical value
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rnorm(100, 30, .05)
#'K2 <- rnorm(100, 21, .05)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'## Control
#'ControlDataMissing(visitor_reward)
#'visitor_reward[1,1]= NA
#'## Control
#'ControlDataMissing(visitor_reward)
#'
#'@export
ControlDataMissing <- function(visitor_reward) {
  #no missing data
  if (sum(colSums(is.na(visitor_reward))) > 0) {
    stop("missing data in arm results database")
    return (FALSE)
  }
  return (TRUE)
}
