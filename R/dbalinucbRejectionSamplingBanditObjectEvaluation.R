#'dbalinucbRejectionSamplingBanditObjectEvaluation
#'
#'Run a \code{\link{DBALINUCB_rejection_sampling}} using visitor_reward and dt values.
#'Control data.
#'Stop if something is wrong.
#'Exclud any choices which not corresponds to real exepriments in dataset.
#'After execution of DBALINUCB_rejection_sampling, calculates the cumulative regret
#'associated with the choices made.
#'Review the cumulative regret according iterations and an DBALINUCB_rejection_sampling object.
#'See also  \code{\link{DBALINUCB_rejection_sampling}}
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param dt Dataframe of integer or numeric values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param alpha numerical value (optional)
#'@param K Integer value (optional)
#'@param average Boolean value to evaluate the policy
#'
#'@return
#' \itemize{ List of element:
#'  \item dbalinucb_rejection_sampling_alloc: dbalinucb bandit object ,
#'  \item cum_reg_dbalinucb_bandit_alloc: List numeric.
#'  }
#'
#'@examples
#'size.tot = 9000
#'@import partykit
#'@export
#dbalinucb_rejection_sampling_bandit object evaluation
dbalinucbRejectionSamplingBanditObjectEvaluation <- function(dt, visitor_reward, alpha=1, K=ncol(visitor_reward), listSerie, listKCentroids , learn_size = as.integer(nrow(dt)*0.3), IsRewardAreBoolean = FALSE , listCategorial=0 , listInteger=0) {



  dbalinucb_rejection_sampling_bandit_alloc <- DBALINUCB_rejection_sampling(dt, visitor_reward, alpha, K, listSerie, listKCentroids , learn_size, IsRewardAreBoolean  , listCategorial , listInteger)
  cum_rew_dbalinucb_rejection_sampling_alloc <- reward_cumulative(choice=dbalinucb_rejection_sampling_bandit_alloc$choice,
                                                                    visitor_reward=visitor_reward[(dbalinucb_rejection_sampling_bandit_alloc$first_train_element+1):nrow(visitor_reward),])

  plot(cum_rew_dbalinucb_rejection_sampling_alloc, type = 'l',ylab = "Cumulative Reward",xlab="Number of items")
  message(paste("average : "),max(cum_rew_dbalinucb_rejection_sampling_alloc)/length(cum_rew_dbalinucb_rejection_sampling_alloc), sep = " " )

  return (list('dbalinucb_rejection_sampling_bandit_alloc'=dbalinucb_rejection_sampling_bandit_alloc ,'cum_rew_dbalinucb_rejection_sampling_alloc'=cum_rew_dbalinucb_rejection_sampling_alloc))
}
