#'ctreeucbRejectionSamplingBanditObjectEvaluation
#'
#'Run a \code{\link{ctreeucb_rejection_sampling}} using visitor_reward and dt values.
#'Control data.
#'Stop if something is wrong.
#'Exclud any choices which not corresponds to real exepriments in dataset.
#'After execution of ctreeucb_bandit, calculates the cumulative regret
#'associated with the choices made.
#'Review the cumulative regret according iterations and an ctreeucb_bandit object.
#'See also \code{\link{ctreeucb}}, \code{\link{ctreeucb_rejection_sampling}}
#'Require \code{\link{ctree}} \code{\link{partykit}} library
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
#'  \item ctreeucb_bandit_alloc: ctreeucb bandit object ,
#'  \item cum_reg_ctreeucb_bandit_alloc: List numeric.
#'  }
#'
#'@examples
#'##### Pairewise #####
#'set.seed(1234)
#'size.tot <- 10000
#'x <- seq(0, 5, 0.01)
#'x1<- sample(x, size.tot, replace = TRUE, prob = NULL)
#'arm_1 <-  as.vector(c(2,-1,1.5,0))
#'K1 <- (x1 < 1 ) * arm_1[4]  +
#'  (x1 >= 1 & x1 < 2 ) * arm_1[1]  +
#'  (x1 >= 2 & x1 < 3) * arm_1[2]  +
#'  (x1 >= 3 & x1 < 4) * arm_1[3]  +
#'  (x1 >= 4) * arm_1[4]
#'plot(x1, K1)
#'
#'arm_2 <-  as.vector(c(1.5,-0.5,1.25,0))
#'K2 <- (x1 < 1 ) * arm_2[4]  +
#'  (x1 >= 1 & x1 < 2 ) * arm_2[1]  +
#'  (x1 >= 2 & x1 < 3) * arm_2[2]  +
#'  (x1 >= 3 & x1 < 4) * arm_2[3]  +
#'  (x1 >= 4) * arm_2[4]
#'plot(x1, K2)

#'#covariate without interest
#'x2<- sample(x, size.tot, replace = TRUE, prob = NULL)
#'#Results for each variation
#'visitor_reward <-  data.frame(K1,K2 )
#'#remove data
#'temp_list <- sample(1:nrow(visitor_reward), as.integer(size.tot/2) , replace = FALSE, prob = NULL)
#'visitor_reward$K1[temp_list] <- NA
#'visitor_reward$K2[-temp_list] <- NA
#'summary(visitor_reward)
#'dt <- as.data.frame(cbind(x1,x2))
#'controle_param = ctreeucb_parameters_control_default(dt=dt, visitor_reward=visitor_reward,learn_size=1500,  alpha=1, ctree_control_val= partykit::ctree_control(teststat = "quadratic"))
#'ctreeucb_rejection_sampling_bandit = ctreeucbRejectionSamplingBanditObjectEvaluation(dt=dt,visitor_reward,ctree_parameters_control = controle_param )
#'#take data for online ab test for other algorithm
#'first <- ctreeucb_rejection_sampling_bandit$ctreeucb_rejection_sampling_bandit_alloc$first_train_element
#'last <- nrow(visitor_reward)
#'dt.abtest <- dt[first:last,]
#'visitor_reward.abtest <- visitor_reward[first:last,]
#'#compare with linucb bandit
#'linucb_rejection_sampling_bandit <- LinucbRejectionSamplingBanditObjectEvaluation(dt.abtest,visitor_reward.abtest)
#'@import partykit
#'@export
#ctreeucb_rejection_sampling_bandit object evaluation
ctreeucbRejectionSamplingBanditObjectEvaluation <- function(dt, visitor_reward, K=ncol(visitor_reward), ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward)) {
  ctreeucb_rejection_sampling_bandit_alloc <- ctreeucb_rejection_sampling(dt,visitor_reward,K, ctree_parameters_control)
  cum_rew_ctreeucb_rejection_sampling_alloc <- reward_cumulative(choice=ctreeucb_rejection_sampling_bandit_alloc$choice,
                                                                 visitor_reward=visitor_reward[ctreeucb_rejection_sampling_bandit_alloc$first_train_element:nrow(visitor_reward),])

  plot(cum_rew_ctreeucb_rejection_sampling_alloc, type = 'l',ylab = "Cumulative Reward",xlab="Number of items")
  message(paste("average : "),max(cum_rew_ctreeucb_rejection_sampling_alloc)/length(cum_rew_ctreeucb_rejection_sampling_alloc), sep = " " )

  return (list('ctreeucb_rejection_sampling_bandit_alloc'=ctreeucb_rejection_sampling_bandit_alloc ,'cum_rew_ctreeucb_rejection_sampling_alloc'=cum_rew_ctreeucb_rejection_sampling_alloc))
}
