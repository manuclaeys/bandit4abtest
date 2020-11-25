#'LinucbRejectionSamplingBanditObjectEvaluation
#'
#'Run the LINUCB algorithm with rejection sampling method using visitor_reward values with \code{\link{LINUCB}} function.
#'Exclude any choices which not corresponds to real exepriments in dataset
#'Stop if something is wrong.
#'After execution of LINUCB, calculates the cumulative reward
#'associated with the choices made.
#'Review the cumulative reward according iterations and an linucb rejection sampling object.
#'See also \code{\link{LINUCB_rejection_sampling}}, \code{\link{LINUCB_rejection_sampling}}
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param dt Dataframe of integer or numeric values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'@param IsRewardAreBoolean (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item linucb_alloc: linucb object ,
#'  \item cum_reg_ucb_alloc: List numeric.
#'  }
#'
#'@examples
#'# Generates 10000 numbers from 2 binomial  distributions
#'set.seed(4434)                         # this makes the example exactly reproducible
#'size.tot = 1000
#'x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
#'x2 = runif(size.tot, min=0, max=10)
#'x3 = runif(size.tot, min=0, max=10)
#'x4 = runif(size.tot, min=0, max=10)
#'dt = cbind(x1,x2,x3,x4)
#'#arm reward
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = crossprod(t(dt),arm_1)
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = crossprod(t(dt),arm_2)
#'dt <- as.data.frame(dt)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'colnames(visitor_reward) = c("K1","K2")
#'#remove data
#'temp_list <- sample(1:nrow(visitor_reward), as.integer(nrow(visitor_reward)/2), replace = FALSE, prob = NULL)
#'visitor_reward$K1[temp_list] <- NA
#'visitor_reward$K2[-temp_list] <- NA
#'#run linucb on missing data
#'#'LinucbRejectionSamplingBanditObjectEvaluation(dt,visitor_reward,alpha = 1,IsRewardAreBoolean = FALSE)
#'@export
#Linucb object evaluation
LinucbRejectionSamplingBanditObjectEvaluation <- function(dt,visitor_reward=visitor_reward ,K=ncol(visitor_reward), alpha=1,IsRewardAreBoolean = FALSE) {

    linucb_rejection_sampling_bandit_alloc <- LINUCB_rejection_sampling(dt,visitor_reward, alpha = alpha, K=K,IsRewardAreBoolean = IsRewardAreBoolean)
    cum_rew_linucb_rejection_sampling_alloc <- reward_cumulative(choice=linucb_rejection_sampling_bandit_alloc$choice,
                                                            visitor_reward=visitor_reward)
    plot(cum_rew_linucb_rejection_sampling_alloc, type = 'l',ylab = "Cumulative Reward",xlab="Number of items")
    message(paste("average : "),max(cum_rew_linucb_rejection_sampling_alloc)/length(cum_rew_linucb_rejection_sampling_alloc), sep = " " )

  return (list('linucb_rejection_sampling_bandit_alloc'=linucb_rejection_sampling_bandit_alloc ,'cum_rew_linucb_rejection_sampling_alloc'=cum_rew_linucb_rejection_sampling_alloc))
}
