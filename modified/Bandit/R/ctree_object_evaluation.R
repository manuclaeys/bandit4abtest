source("Bandit/R/ctree_parameters_control_default.R")
source("Bandit/R/CTREE.R")

CtreeObjectEvaluation <- function(dt = dt, visitorReward = visitorReward, ctreeParametersControl = ctreeParametersControlDefault(dt, visitorReward)) {

  ctreeRes <- CTREE(dt, visitorReward, ctreeParametersControl)

  source("Bandit/R/regret.R")
  cumRegCtree <- cumulativeRegret(ctreeRes$choice, visitorReward[c((ctreeRes$firstTrainElement):nrow(visitorReward)), ])

  return(list('ctreeRes' = ctreeRes,'cumRegCtree' = cumRegCtree))
}
