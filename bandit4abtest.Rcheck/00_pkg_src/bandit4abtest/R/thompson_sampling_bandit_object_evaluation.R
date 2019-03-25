#'thompson_sampling_bandit_object_evaluation
#'
#'Run the Thompson Sampling algorithm using visitorReward values with \code{\link{thompson_sampling}} function.
#'Stop if something is wrong.
#'After execution of Thompson Sampling, calculates the cumulative regret
#'associated with the choices maded.
#'Review the cumulative regret according iterations and an thompson sampling object.
#'See also \code{\link{thompson_sampling}}, \code{\link{cumulativeRegret}}
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitorReward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'@param beta Numeric value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item thompson_sampling_alloc: thompson_sampling object ,
#'  \item cum_reg_ucb_alloc: List numeric.
#'  }
#'
#'@examples
#'## Generates 1000 numbers from 2 binomial distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitorReward <- as.data.frame(cbind(K1,K2) )
#'#Run Thompson Sampling algorithm with policy evaluation
#'thompson_sampling_bandit_object_evaluation(visitorReward,alpha = 1, beta = 1 )
#'
#'@export
#thompson sampling object evaluation
thompson_sampling_bandit_object_evaluation <- function(visitorReward=visitorReward,K=ncol(visitorReward),alpha=1,beta=1){
  thompson_sampling_alloc  <- thompson_sampling(visitorReward,alpha = alpha, beta=beta,K=K)
  cum_reg_thompson_sampling_alloc  <- cumulativeRegret(thompson_sampling_alloc$choice,visitorReward)
  return(list('thompson_sampling_alloc'=thompson_sampling_alloc ,'cum_reg_thompson_sampling_alloc'=cum_reg_thompson_sampling_alloc))
}
