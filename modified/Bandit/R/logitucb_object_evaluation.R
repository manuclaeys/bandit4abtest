source("Bandit/R/linucb_parameters_control_default.R")
source("Bandit/R/LIN_LOGIT.R")
source("Bandit/R/regret.R")

LogitUCBObjectEvaluation <- function(dt = dt, 
                                     visitorReward = visitorReward,
                                     linUCBParametersControl = LinUCBParametersControlDefault(dt, visitorReward),
                                     updateVal = 100) {
  logitUCBRes <- LINUCB_logit(dt, visitorReward, linUCBParametersControl, updateVal)
  cumRegLogitUCB <- CumulativeRegret(logitucb_res$choice, visitorReward)
  return(list('logitUCBRes' = logitUCBRes, 'cumRegLogitUCB' = cumRegLogitUCB))
}
