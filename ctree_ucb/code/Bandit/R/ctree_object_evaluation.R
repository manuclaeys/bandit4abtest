source("Bandit/R/ctree_parameters_control_default.R")
source("Bandit/R/CTREE.R")

ctree_object_evaluation <- function(dt = dt, visitorReward = visitorReward, ctree_parameters_control = ctree_parameters_control_default(dt, visitorReward)) {

  ctree_res <- CTREE(dt, visitorReward, ctree_parameters_control)

  source("Bandit/R/regret.R")
  cum_reg_ctree <- cumulativeRegret(ctree_res$choice, visitorReward[c((ctree_res$first_train_element):nrow(visitorReward)), ])

  return(list('ctree_res' = ctree_res,'cum_reg_ctree' = cum_reg_ctree))
}
