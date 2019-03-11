#'uniform_bandit_object_evaluation
#'
#'Run a uniform allocation using visitorReward values with \code{\link{uniform_bandit}} function.
#'Stop if something is wrong.
#'After execution of uniform_bandit, calculates the cumulative regret
#'associated with the choices made.
#'Review the cumulative regret according iterations and an uniform_bandit object.
#'See also \code{\link{uniform_bandit}}, \code{\link{cumulativeRegret}}
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitorReward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item uniform_bandit_alloc: uniform_bandit object ,
#'  \item cum_reg_uniform_bandit_alloc: List numeric.
#'  }
#'
#'@examples
#'## Generates 1000 numbers from 2 binomial distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitorReward <- as.data.frame(cbind(K1,K2) )
#'#Run uniform bandit allocation with policy evaluation
#'uniform_bandit_object_evaluation(visitorReward)
#'
#'@export
#uniform_bandit object evaluation
uniform_bandit_object_evaluation <- function(visitorReward=visitorReward,K=ncol(visitorReward)){
  uniform_bandit_alloc  <- uniform_bandit(visitorReward,K=K)
  cum_reg_uniform_bandit_alloc  <- cumulativeRegret(uniform_bandit_alloc$choice,visitorReward)
  return(list('uniform_bandit_alloc'=uniform_bandit_alloc ,'cum_reg_uniform_bandit_alloc'=cum_reg_uniform_bandit_alloc))
}
