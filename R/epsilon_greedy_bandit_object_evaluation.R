#'EpsilonGreedyBanditObjectEvaluation
#'
#'Run the EpsilonGreedy algorithm using visitor_reward values with \code{\link{EpsilonGreedy}} function.
#'Stop if something is wrong.
#'After execution of EpsilonGreedy, calculates the cumulative regret
#'associated with the choices made.
#'Review the cumulative regret according iterations and an EpsilonGreedy object.
#'See also \code{\link{EpsilonGreedy}}, \code{\link{CumulativeRegret}}
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param epsilon Numeric value (optional)
#'@param average Boolean value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item EpsilonGreedy_alloc: EpsilonGreedy object ,
#'  \item cum_reg_EpsilonGreedy_alloc: List numeric.
#'  }
#'
#'@examples
#'## Generates 1000 numbers from 2 binomial distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#Run EpsilonGreedy algorithm with policy evaluation
#'EpsilonGreedyBanditObjectEvaluation(visitor_reward,epsilon = 0.25)
#'
#'@export
#EpsilonGreedy object evaluation
EpsilonGreedyBanditObjectEvaluation <- function(visitor_reward=visitor_reward, K=ncol(visitor_reward), epsilon=0.25,average = FALSE,IsRewardAreBoolean = TRUE,dt=NA,explanatory_variable=colnames(dt) ) {
  epsilon_greedy_alloc <- EpsilonGreedy(visitor_reward, epsilon=epsilon, K=K)

  if(average == FALSE) cum_reg_epsilon_greedy_alloc  <- cumulativeRegret(epsilon_greedy_alloc$choice,visitor_reward)
  if(average == TRUE) cum_reg_epsilon_greedy_alloc  <- cumulativeRegretAverage(epsilon_greedy_alloc$choice,
                                                                                       visitor_reward = visitor_reward,
                                                                                       dt=dt,
                                                                                       IsRewardAreBoolean=IsRewardAreBoolean,
                                                                                       explanatory_variable=explanatory_variable)




  return(list('epsilon_greedy_alloc'=epsilon_greedy_alloc ,'cum_reg_epsilon_greedy_alloc'=cum_reg_epsilon_greedy_alloc))
}
