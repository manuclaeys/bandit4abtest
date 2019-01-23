
source("Bandit/R/UCB_functions.R")
source("Bandit/R/regret.R")

ucb_bandit_object_evaluation <- function(visitorReward=visitorReward,alpha){

  ucb_alloc  <- UCB(visitorReward,alpha = alpha)

  cum_reg_ucb_alloc  <- cumulativeRegret(ucb_alloc$choice,visitorReward)


  return(list('ucb_alloc'=ucb_alloc ,'cum_reg_ucb_alloc'=cum_reg_ucb_alloc))
}
