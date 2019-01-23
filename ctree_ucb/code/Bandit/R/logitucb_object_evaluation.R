
source("Bandit/R/linucb_parameters_control_default.R")
source("Bandit/R/LIN_LOGIT.R")
source("Bandit/R/regret.R")

logitucb_object_evaluation <- function(dt=dt,visitorReward=visitorReward,linucb_parameters_control=linucb_parameters_control_default(dt,visitorReward),update_val= 100){

  logitucb_res <- LINUCB_logit(dt,visitorReward,linucb_parameters_control,update_val)

  cum_reg_logitucb <- cumulativeRegret(logitucb_res$choice,visitorReward)


  return(list('logitucb_res'=logitucb_res,'cum_reg_logitucb'=cum_reg_logitucb))
}
