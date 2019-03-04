library(partykit)
ctree_parameters_control_default <- function(
  dt,
  visitorReward,
  learn_size = as.integer(nrow(dt)*0.10),
  is_reward_are_boolean = FALSE,
  arm_for_learn = names(visitorReward)[1],
  explanatory_variable =  names(dt),
  alpha=1,
  ctree_control_val=ctree_control()
  ){
    return(list(learn_size = learn_size ,is_reward_are_boolean = is_reward_are_boolean , arm_for_learn= arm_for_learn,
                explanatory_variable =explanatory_variable,
                alpha=alpha,
                ctree_control_val=ctree_control_val
                ))
  }
