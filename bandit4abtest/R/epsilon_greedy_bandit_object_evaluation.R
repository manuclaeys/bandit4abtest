#'epsilonGreedy_bandit_object_evaluation
#'
#'Run the epsilonGreedy algorithm using visitorReward values with \code{\link{epsilonGreedy}} function.
#'Stop if something is wrong.
#'After execution of epsilonGreedy, calculates the cumulative regret
#'associated with the choices made.
#'Review the cumulative regret according iterations and an epsilonGreedy object.
#'See also \code{\link{epsilonGreedy}}, \code{\link{cumulativeRegret}}
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitorReward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param epsilon Numeric value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item epsilonGreedy_alloc: epsilonGreedy object ,
#'  \item cum_reg_epsilonGreedy_alloc: List numeric.
#'  }
#'
#'@examples
#'## Generates 1000 numbers from 2 binomial distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitorReward <- as.data.frame(cbind(K1,K2) )
#'#Run epsilonGreedy algorithm with policy evaluation
#'epsilonGreedy_bandit_object_evaluation(visitorReward,epsilon = 0.25)
#'
#'@export
#epsilonGreedy object evaluation
epsilonGreedy_bandit_object_evaluation <- function(visitorReward=visitorReward,K=ncol(visitorReward),epsilon=0.25){
  epsilonGreedy_alloc  <- epsilonGreedy(visitorReward,epsilon=epsilon,K=K)
  cum_reg_epsilonGreedy_alloc  <- cumulativeRegret(epsilonGreedy_alloc$choice,visitorReward)
  return(list('epsilonGreedy_alloc'=epsilonGreedy_alloc ,'cum_reg_epsilonGreedy_alloc'=cum_reg_epsilonGreedy_alloc))
}
