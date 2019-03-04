LinUCBParametersControlDefault <- function(dt, visitorReward, explanatory_variable = names(dt), alpha = 1) {
  return(list(alpha = alpha, explanatory_variable = explanatory_variable))
}