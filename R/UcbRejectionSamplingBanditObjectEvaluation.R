#'UcbRejectionSamplingBanditObjectEvaluation
#'
#'Run the UCB algorithm with rejection sampling method using visitor_reward values with \code{\link{UCB}} function.
#'Exclude any choices which not corresponds to real exepriments in dataset
#'Stop if something is wrong.
#'After execution of UCB, calculates the cumulative reward
#'associated with the choices made.
#'Review the cumulative reward according iterations and an ucb rejection sampling object.
#'See also \code{\link{UCB_rejection_sampling}}, \code{\link{UCB_rejection_sampling}}
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'@param average Boolean values to define the cumulative regret evaluation (simple:FALSE, average:TRUE)
#'
#'@return
#' \itemize{ List of element:
#'  \item ucb_alloc: ucb object ,
#'  \item cum_reg_ucb_alloc: List numeric.
#'  }
#'
#'@examples
#'## Generates 10000 numbers from 2 binomial  distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#remove data
#'temp_list <- sample(1:nrow(visitor_reward), 500, replace = FALSE, prob = NULL)
#'visitor_reward$K1[temp_list] <- NA
#'visitor_reward$K2[-temp_list] <- NA
#'#run ucb on missing data
#'UcbRejectionSamplingBanditObjectEvaluation(visitor_reward,alpha = 1)
#'@export
#UCB object evaluation
UcbRejectionSamplingBanditObjectEvaluation <- function(visitor_reward=visitor_reward ,K=ncol(visitor_reward), alpha=1) {
  ucb_rejection_sampling_bandit_alloc <- UCB_rejection_sampling(visitor_reward, alpha = alpha, K=K)
  cum_rew_ucb_rejection_sampling_alloc <- reward_cumulative(choice=ucb_rejection_sampling_bandit_alloc$choice,
                                                            visitor_reward=visitor_reward)

  plot(cum_rew_ucb_rejection_sampling_alloc, type = 'l',ylab = "Ucb Rejection Sampling")
  message(paste("average : "),max(cum_rew_ucb_rejection_sampling_alloc)/length(cum_rew_ucb_rejection_sampling_alloc), sep = " " )
  return (list('ucb_rejection_sampling_bandit_alloc'=ucb_rejection_sampling_bandit_alloc ,'cum_rew_ucb_rejection_sampling_alloc'=cum_rew_ucb_rejection_sampling_alloc))
}
