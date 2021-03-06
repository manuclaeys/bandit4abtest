#'dbactreeucbRejectionSamplingBanditObjectEvaluation
#'
#'Run a \code{\link{dbactreeucb_rejection_sampling}} using visitor_reward and dt values.
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
#'size.tot = 9000
#'set.seed(4649)                          # this makes the example exactly reproducible
#'x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
#'x2 = runif(size.tot, min=0, max=10)
#'x3 = runif(size.tot, min=0, max=10)
#'x4 = runif(size.tot, min=0, max=10)
#'dt = cbind(x1,x2,x3,x4)
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = crossprod(t(dt),arm_1)
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = crossprod(t(dt),arm_2)
#'arm_3 <-  as.vector(c(-1,-5,1,10))
#'K3 = crossprod(t(dt),arm_3)
#'visitor_reward <-  data.frame(K1,K2,K3)
#'dt <- as.data.frame(dt)
#'size.tot = 9000
#'# Time series
#'alpha_list <- c(1,2,3)
#'beta_list <- c(0.5,0.1,-0.2)
#'theta_list <- c(0.8,0.2,0.5)
#'y <- as.data.frame(c(1))
#'colnames(y) = "ID"
#'temp=1
#'for (j in 1:3000){
#'  for (i in 1:length(alpha_list)){
#'    n = sample(1:100,1)
#'    t <- 1:n
#'    ts <- alpha_list[i] + beta_list[i] * t + arima.sim(list(ma = theta_list[i]), n = length(t))
#'    y[temp, "time_series"][[1]] <- list(ts)
#'    y[temp, "cluster"][[1]] <- i
#'    y$ID[temp] = temp
#'    temp = temp +1
#'  }
#'}
#'y <- y[sample(nrow(y)),]



#'dt <-  as.data.frame(cbind(x1,x2,x3,x4,y$time_series))
#'colnames(dt) <- c("x1","x2","x3","x4","time_series")

#'for(i in 1:nrow(dt)) {
#'  if(y$cluster[i] == 1) visitor_reward$K1[i] = 10
#'  if(y$cluster[i] == 2) visitor_reward$K2[i] = 20
#'  if(y$cluster[i] == 3) visitor_reward$K3[i] = 30
#'}

#'dt$cluster <- NULL
#'dt$x1 <- as.numeric(dt$x1)
#'dt$x2 <- as.numeric(dt$x2)
#'dt$x3 <- as.numeric(dt$x3)
#'dt$x4 <- as.numeric(dt$x4)
#'K=ncol(visitor_reward)
#'ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward)
#'listSerie = c("time_series")
#'listKCentroids=c(3)
#'dbactreeucbRejectionSamplingBanditObjectEvaluation(dt,visitor_reward,K, listSerie, listKCentroids , ctree_parameters_control)
#'@import partykit
#'@export
#dbactreeucb_rejection_sampling_bandit object evaluation
dbactreeucbRejectionSamplingBanditObjectEvaluation <- function(dt,visitor_reward,K=ncol(visitor_reward), listSerie, listKCentroids , ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward)) {
  dbactreeucb_rejection_sampling_bandit_alloc <- dbactreeucb_rejection_sampling(dt,visitor_reward,K, listSerie, listKCentroids , ctree_parameters_control)
  cum_rew_dbactreeucb_rejection_sampling_alloc <- reward_cumulative(choice=dbactreeucb_rejection_sampling_bandit_alloc$choice,
                                                                 visitor_reward=visitor_reward[dbactreeucb_rejection_sampling_bandit_alloc$first_train_element:nrow(visitor_reward),])

  plot(cum_rew_dbactreeucb_rejection_sampling_alloc, type = 'l',ylab = "Cumulative Reward",xlab="Number of items")
  message(paste("average : "),max(cum_rew_dbactreeucb_rejection_sampling_alloc)/length(cum_rew_dbactreeucb_rejection_sampling_alloc), sep = " " )

  return (list('dbactreeucb_rejection_sampling_bandit_alloc'=dbactreeucb_rejection_sampling_bandit_alloc ,'cum_rew_dbactreeucb_rejection_sampling_alloc'=cum_rew_dbactreeucb_rejection_sampling_alloc))
}
