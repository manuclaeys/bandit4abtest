#'UcbBanditObjectEvaluation
#'
#'Run the UCB algorithm using visitor_reward values with \code{\link{UCB}} function.
#'Stop if something is wrong.
#'After execution of UCB, calculates the cumulative regret
#'associated with the choices made.
#'Review the cumulative regret according iterations and an ucb object.
#'See also \code{\link{UCB}}, \code{\link{CumulativeRegret}}
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item ucb_alloc: ucb object ,
#'  \item cum_reg_ucb_alloc: List numeric.
#'  }
#'
#'@examples
#'## Generates 1000 numbers from 2 binomial distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#Run UCB algorithm with policy evaluation
#'UcbBanditObjectEvaluation(visitor_reward,alpha = 1)
#'
#'@export
#UCB object evaluation
UcbBanditObjectEvaluation <- function(visitor_reward=visitor_reward ,K=ncol(visitor_reward), alpha) {
  ucb_alloc <- UCB(visitor_reward, alpha = alpha, K=K)
  cum_reg_ucb_alloc <- CumulativeRegret(ucb_alloc$choice, visitor_reward)
  return (list('ucb_alloc'=ucb_alloc ,'cum_reg_ucb_alloc'=cum_reg_ucb_alloc))
}
