#'ThompsonSamplingBanditObjectEvaluation
#'
#'Run the Thompson Sampling algorithm using visitor_reward values with \code{\link{ThompsonSampling}} function.
#'Stop if something is wrong.
#'After execution of Thompson Sampling, calculates the cumulative regret
#'associated with the choices maded.
#'Review the cumulative regret according iterations and an thompson sampling object.
#'See also \code{\link{ThompsonSampling}}, \code{\link{CumulativeRegret}}
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'@param beta Numeric value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item ThompsonSampling_alloc: ThompsonSampling object ,
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
#'#Run Thompson Sampling algorithm with policy evaluation
#'ThompsonSamplingBanditObjectEvaluation(visitor_reward,alpha = 1, beta = 1 )
#'ThompsonSamplingBanditObjectEvaluation(visitor_reward= visitor_reward,alpha = 1, beta = 1 ,average = TRUE,IsRewardAreBoolean = TRUE)
#'ThompsonSamplingBanditObjectEvaluation(visitor_reward= visitor_reward,alpha = 1, beta = 1 ,average = TRUE,IsRewardAreBoolean = TRUE,dt=dt)
#'@export
#thompson sampling object evaluation
ThompsonSamplingBanditObjectEvaluation <- function(visitor_reward=visitor_reward, K=ncol(visitor_reward), alpha=1, beta=1,average = FALSE,IsRewardAreBoolean = TRUE,dt=NA,explanatory_variable=colnames(dt)) {
  ThompsonSampling_bandit_alloc <- ThompsonSampling(visitor_reward, alpha = alpha, beta = beta, K = K)


  if(average == FALSE) cum_reg_ThompsonSampling_bandit_alloc <- cumulativeRegret(ThompsonSampling_bandit_alloc$choice, visitor_reward)
  if(average == TRUE) cum_reg_ThompsonSampling_bandit_alloc <- cumulativeRegretAverage(ThompsonSampling_bandit_alloc$choice,
                                                                             visitor_reward = visitor_reward,
                                                                             dt=dt,
                                                                             IsRewardAreBoolean=IsRewardAreBoolean,
                                                                             explanatory_variable=explanatory_variable)




  return (list('ThompsonSampling_alloc'=ThompsonSampling_bandit_alloc ,'cum_reg_ThompsonSampling_alloc'=cum_reg_ThompsonSampling_bandit_alloc))
}
