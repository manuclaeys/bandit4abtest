#'ctreeucbBanditObjectEvaluation
#'
#'Run a \code{\link{ctreeucb}} using visitor_reward and dt values.
#'Control data.
#'Stop if something is wrong.
#'After execution of ctreeucb_bandit, calculates the cumulative regret
#'associated with the choices made.
#'Review the cumulative regret according iterations and an ctreeucb_bandit object.
#'See also \code{\link{ctreeucb}}, \code{\link{CumulativeRegret}}
#'Require \code{\link{ctree}} \code{\link{partykit}} library
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param dt Dataframe of integer or numeric values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param alpha numerical value (optional)
#'@param K Integer value (optional)
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
#'summary(visitor_reward)
#'dt <- as.data.frame(cbind(x1,x2))
#'controle_param = ctreeucb_parameters_control_default(dt=dt, visitor_reward=visitor_reward,learn_size=1500,  alpha=1, ctree_control_val= partykit::ctree_control(teststat = "quadratic"))
#'ctreeucb_bandit = ctreeucbBanditObjectEvaluation(dt=dt,visitor_reward,ctree_parameters_control = controle_param )

#'#take data for online ab test for other algorithm
#'first <-  ctreeucb_bandit$ctreeucb_bandit_alloc$first_train_element
#'last <- nrow(visitor_reward)
#'dt.abtest <- dt[first:last,]
#'visitor_reward.abtest <- visitor_reward[first:last,]
#'#compare with linucb bandit
#'linucb_bandit <- LinucbBanditObjectEvaluation(dt.abtest,visitor_reward.abtest)
#'@import partykit
#'@export
#ctreeucb_bandit object evaluation
ctreeucbBanditObjectEvaluation <- function(dt, visitor_reward, K=ncol(visitor_reward), ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward)) {
  ctreeucb_bandit_alloc <- ctreeucb(dt,visitor_reward,K, ctree_parameters_control)
  cum_reg_ctreeucb_bandit_alloc <- CumulativeRegret(ctreeucb_bandit_alloc$choice,visitor_reward[ctreeucb_bandit_alloc$first_train_element:nrow(visitor_reward),])
  return (list('ctreeucb_bandit_alloc'=ctreeucb_bandit_alloc ,'cum_reg_ctreeucb_bandit_alloc'=cum_reg_ctreeucb_bandit_alloc))
}
