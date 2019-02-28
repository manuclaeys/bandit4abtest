
change_data_type_for_ctree <- function(dt, is_reward_boolean, visitorReward) {
  # Change the type of reward data if it is consider as boolean values
  #
  # Args:
  #   dt: the dataframes, or reward value
  #   is_reward_boolean: tell if the dataframes, or reward, is of boolean type
  #   visitorReward: dataframe of reward
  #
  # Returns:
  #   
  #
  # Error handling
  if (is_reward_boolean) {
    for (i in 1:ncol(visitorReward))
      visitorReward[, i] <- as.factor(visitorReward[, i])
  }
  for (i in 1:ncol(dt)) {
    if ((typeof(dt[, i]) == "character") | (typeof(dt[, i]) == "logical"))
      dt[,i] <- as.factor(dt[,i])
  }
  return(list(dt = dt,visitorReward=visitorReward ))
}



