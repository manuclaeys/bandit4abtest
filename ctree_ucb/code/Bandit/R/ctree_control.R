
ctree_data_control <- function(dt, visitorReward, is_reward_boolean) {
  #contextual data size controle
  if (nrow(dt) == 0) {
    message = "empty contextual data"
    control = FALSE
    return(list(message = message, control =  control))
  }

  #reward data size controle
  if (nrow(dt) == 0) {
    message = "empty reward data"
    control = FALSE
    return(list(message = message, control = control))
  }

  #Match size controle
  if (nrow(dt) != nrow(visitorReward)) {
    message = " number of row in contextual data and rewards data are not equals"
    control = FALSE
    return(list(message = message, control = control))
  }

  #Type controle
  list_reward_possible <- c("double", "integer")
  for (i in 1:ncol(visitorReward)) {
      if (length(intersect(typeof(visitorReward[,i]), list_reward_possible))) {
        message = paste("The", i, "colomn of reward data is not a double of integer data", sep = " ")
        control = FALSE
        return(list(message = message, control = control))
    }
  }

  #Reward data type identicals control
  type_of_reward <- sapply(visitorReward, typeof)
  if (length(unique(type_of_reward)) > 1) {
    message = "reward data type are not identicals "
    control = FALSE
    return(list(message = message, control = control))
  }

  #Check if reward are a boolean value (option)
  if (is_reward_boolean) {
    for (i in 1:ncol(visitorReward)) {
      if (typeof(visitorReward[,i]) != "integer" ) {
        message = paste("The", i, "colomn of reward data is not a integer/double data so it can be define as a boolean data", sep = " ")
        control = FALSE
        return(list(message = message,control = control))
      }
    }
  }
  return(list(message = "OK", control = TRUE))
}
