# Perform various checks on the variables received as arguments
#
# Args:
#   dt: contextual data
#   visitorReward: reward data
#   isRewardBoolean: tell if the reward is of boolean type
#
# Returns:
#   Return a message and a status (true or false depending on the result of the controls)
#

CtreeDataControl <- function(dt, visitorReward, isRewardBoolean) {
  #  contextual data size controle
  if (nrow(dt) == 0) {
    message = "empty contextual data"
    control = FALSE
    return(list(message = message, control = control))
  }

  #  reward data size controle
  if (nrow(visitorReward) == 0) {
    message = "empty reward data"
    control = FALSE
    return(list(message = message, control = control))
  }

  #  Match size controle
  if (nrow(dt) != nrow(visitorReward)) {
    message = " number of row in contextual data and rewards data are not equals"
    control = FALSE
    return(list(message = message, control = control))
  }

  #  Type controle
  listRewardPossible <- c("double", "integer")
  for (i in 1:ncol(visitorReward)) {
      if (length(intersect(typeof(visitorReward[, i]), listRewardPossible))) {
        message = paste("The", i, "colomn of reward data is not a double of integer data", sep = " ")
        control = FALSE
        return(list(message = message, control = control))
    }
  }

  #  Reward data type identicals control
  typeOfReward <- sapply(visitorReward, typeof)
  if (length(unique(typeOfReward)) > 1) {
    message = "reward data type are not identicals "
    control = FALSE
    return(list(message = message, control = control))
  }

  #  Check if reward are a boolean value (option)
  if (isRewardBoolean) {
    for (i in 1:ncol(visitorReward)) {
      if (typeof(visitorReward[, i]) != "integer") {
        message = paste("The", i, "colomn of reward data is not a integer/double data so it can be define as a boolean data", sep = " ")
        control = FALSE
        return(list(message = message, control = control))
      }
    }
  }
  return(list(message = "OK", control = TRUE))
}
