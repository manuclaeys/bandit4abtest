#  set to local files
(WD <- getwd())
#WD <- "/home/manue/Documents/manue/Manipulation/datascience-emmanuelle/programme_R/bandit/xp ctree_ucb/programme général/Bandit"
if (!is.null(WD)) setwd(WD)
source("Bandit/R/linucb_parameters_control_default.R")
source("Bandit/R/KERNEL_UCB.R")
source("Bandit/R/regret.R")

KernelObjectEvaluation <- function(dt = dt,
                                   visitorReward = visitorReward,
                                   linUCBParametersControl = LinUCBParametersControlDefault(dt, visitorReward), 
                                   updateVal = 100, 
                                   alpha = 1) {

  kernelUCBRes <- KernelUCB(dt, visitorReward, linUCBParametersControl, updateVal, alpha)
  cumRegKernelUCB <- CumulativeRegret(kernelUCBRes$choice, visitorReward)
  return(list('kernelUCBRes' = kernelUCBRes, 'cumRegKernelUCB' = cumRegKernelUCB))
}
