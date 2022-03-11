#'uniform_bandit_object_evaluationRejectionSampling
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
uniform_bandit_object_evaluationRejectionSampling <- function(visitor_reward=visitor_reward, K=ncol(visitor_reward),average = FALSE,IsRewardAreBoolean = FALSE,dt.reward=NA,explanatory_variable=colnames(dt.reward)) {
  uniform_bandit_allocRejectionSampling <- UniformBanditRejectionSampling(visitor_reward, K=K)

  if(average == FALSE) {
    cum_rew_uniform_bandit_allocRejectionSampling <- reward_cumulative(choice=uniform_bandit_allocRejectionSampling$choice,
                                                                       visitor_reward=visitor_reward)
    plot(cum_rew_uniform_bandit_allocRejectionSampling , type = 'l',ylab = "Cumulative Reward",xlab="Number of items")
    message(paste("average : "),max(cum_rew_uniform_bandit_allocRejectionSampling )/length(cum_rew_uniform_bandit_allocRejectionSampling ), sep = " " )

  }



  # if(average == TRUE) #TODO

  return (list('uniform_bandit_rejectionSampling_alloc'=uniform_bandit_allocRejectionSampling ,'cum_rew_uniform_bandit_allocRejectionSampling'=cum_rew_uniform_bandit_allocRejectionSampling))
}
