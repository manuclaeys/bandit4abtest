source("Bandit/R/UCB_functions.R")
source("Bandit/R/regret.R")

UCBBanditObjectEvaluation <- function(visitorReward = visitorReward, alpha) {
  ucbAlloc <- UCB(visitorReward, alpha = alpha)
  cumRegUcbAlloc <- CumulativeRegret(ucbAlloc$choice, visitorReward)
  return(list('ucbAlloc' = ucbAlloc ,'cumRegUcbAlloc' = cumRegUcbAlloc))
}
