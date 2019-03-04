#set to local files
(WD <- getwd())
#WD <- "/home/manue/Documents/manue/Manipulation/datascience-emmanuelle/programme_R/bandit/xp ctree_ucb/programme général/Bandit"
if (!is.null(WD)) setwd(WD)
source("Bandit/R/linucb_parameters_control_default.R")
source("Bandit/R/KERNEL_UCB.R")
source("Bandit/R/regret.R")

kernel_object_evaluation <- function(dt=dt,visitorReward=visitorReward,linucb_parameters_control=linucb_parameters_control_default(dt,visitorReward),update_val= 100,alpha=1){

  kernelucb_res <- KERNEL_UCB(dt,visitorReward,linucb_parameters_control,update_val,alpha)

  cum_reg_kernelucb <- cumulativeRegret(kernelucb_res$choice,visitorReward)


  return(list('kernelucb_res'=kernelucb_res,'cum_reg_kernelucb'=cum_reg_kernelucb))
}
