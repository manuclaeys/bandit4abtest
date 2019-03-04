# Change the type of reward data if it is consider as boolean values
#
# Args:
#   dt: the dataframes, or reward value
#   isRewardBoolean: tell if the dataframes, or reward, is of boolean type
#   visitorReward: dataframe of reward
#
# Returns:
#   
#
# Error handling

ChangeDataTypeForCtree <- function(dt, isRewardBoolean, visitorReward) {
  if (isRewardBoolean) {
    for (i in 1:ncol(visitorReward))
      visitorReward[, i] <- as.factor(visitorReward[, i])
  }
  for (i in 1:ncol(dt)) {
    if ((typeof(dt[, i]) == "character") | (typeof(dt[, i]) == "logical"))
      dt[, i] <- as.factor(dt[, i])
  }
  return(list(dt = dt, visitorReward = visitorReward))
}



