source("Bandit/R/linucb_parameters_control_default.R")
source("Bandit/R/LINUCB.R")
source("Bandit/R/regret.R")

LinUCBObjectEvaluation <- function(dt = dt, visitorReward = visitorReward, LinUCBParametersControl = LinUCBParametersControlDefault(dt, visitorReward)) {
  linUCBRes <- LINUCB(dt, visitorReward, LinUCBParametersControl)
  cumRegLinUCB <- CumulativeRegret(linUCBRes$choice, visitorReward)
  return(list('linUCBRes'=linUCBRes,'cumRegLinUCB'=cumRegLinUCB))
}
