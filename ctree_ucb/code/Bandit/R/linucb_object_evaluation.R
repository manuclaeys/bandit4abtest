
source("Bandit/R/linucb_parameters_control_default.R")
source("Bandit/R/LINUCB.R")
source("Bandit/R/regret.R")

linucb_object_evaluation <- function(dt=dt,visitorReward=visitorReward,linucb_parameters_control=linucb_parameters_control_default(dt,visitorReward)){

  linucb_res <- LINUCB(dt,visitorReward,linucb_parameters_control)

   cum_reg_linucb <- cumulativeRegret(linucb_res$choice,visitorReward)


  return(list('linucb_res'=linucb_res,'cum_reg_linucb'=cum_reg_linucb))
}
