#'uniform_bandit_object_evaluation
#'
#'Run a uniform allocation using visitor_reward values with \code{\link{UniformBandit}} function.
#'Stop if something is wrong.
#'After execution of UniformBandit, calculates the cumulative regret
#'associated with the choices made.
#'Review the cumulative regret according iterations and an UniformBandit object.
#'See also \code{\link{UniformBandit}}, \code{\link{CumulativeRegret}}
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item uniform_bandit_alloc: UniformBandit object ,
#'  \item cum_reg_uniform_bandit_alloc: List numeric.
#'  }
#'
#'@examples
#'## Generates 1000 numbers from 2 binomial distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#Run uniform bandit allocation with policy evaluation
#'uniform_bandit_object_evaluation(visitor_reward)
#'uniform_bandit_object_evaluation(visitor_reward,average = TRUE,IsRewardAreBoolean = TRUE)
#'
#'@export
#UniformBandit object evaluation
uniform_bandit_object_evaluation <- function(visitor_reward=visitor_reward, K=ncol(visitor_reward),average = FALSE,IsRewardAreBoolean = FALSE,dt.reward=NA,explanatory_variable=colnames(dt.reward)) {
  uniform_bandit_alloc <- UniformBandit(visitor_reward, K=K)

  if(average == FALSE) cum_reg_uniform_bandit_alloc <- cumulativeRegret(uniform_bandit_alloc$choice, visitor_reward)
  if(average == TRUE) cum_reg_uniform_bandit_alloc <- cumulativeRegretAverage(uniform_bandit_alloc$choice,
                                                                          visitor_reward = visitor_reward,
                                                                          dt=dt.reward,
                                                                          IsRewardAreBoolean=IsRewardAreBoolean,
                                                                          explanatory_variable=explanatory_variable)

  return (list('uniform_bandit_alloc'=uniform_bandit_alloc ,'cum_reg_uniform_bandit_alloc'=cum_reg_uniform_bandit_alloc))
}
