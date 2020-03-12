#'BanditRewardControl
#'
#'Control data for bandit algorithm.
#'See also \code{\link{ControlDataMissing}} and \code{\link{DataControlK}}
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
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
#'BanditRewardControl(visitor_reward,K=2)
#'
#'@export
BanditRewardControl <- function(visitor_reward, K = ncol(visitor_reward)) {
  DataControlK(visitor_reward = visitor_reward, K = K)
  ControlDataMissing(visitor_reward = visitor_reward)
}
