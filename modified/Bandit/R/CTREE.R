#return time elaps
library(tictoc)

source("Bandit/R/ctree_parameters_control_default.R")

CTREE <- function(dt, visitorReward, ctreeParametersControl = CtreeParametersControlDefault(dt, visitorReward)) {
  set.seed(4321)
  #  set to file directory
  (WD <- getwd())
  if (!is.null(WD)) setwd(WD)
  ### control ###
  #  data controle : see ctree_control
  source("Bandit/R/ctree_control.R")
  control <- ctree_data_control(dt, visitorReward, ctreeParametersControl$isRewardAreBoolean) #  data controle : need a context and reward data of same size
    if (control$control == FALSE) return(paste("Error,", control$message, sep = ""))


  #Change the type of data
  source("Bandit/R/change_data_type_for_ctree.R")
  temp <- ChangeDataTypeForCtree(dt, ctreeParametersControl$isRewardAreBoolean, visitorReward)
  dt <- temp$dt

  #if reward is boolean, data will be modify temporary
  temp.visitorReward <- temp$visitorReward

  ### learning  ###
  #  Generate formula and tree
  source("Bandit/R/ctree_formula_generate.R")
  ctreeTree <- CtreeFormulaGenerate(dt = dt, 
                                     visitorReward = temp.visitorReward,
                                     ctreeControlVal = ctreeParametersControl$ctreeControlVal,
                                     armForLearn = ctreeParametersControl$armForLearn,
                                     explanatoryVariable = ctreeParametersControl$explanatoryVariable,
                                     learnSize = ctreeParametersControl$learnSize)
  #  return to regular data
  visitorReward <- visitorReward

  #  remove the learn set
  #  update handle one covariate

  if (length(ctreeParametersControl$explanatoryVariable) == 1) {
    dt <- as.data.frame(dt[c((ctreeParametersControl$learnSize + 1):nrow(dt)), ])
    colnames(dt) <- c("x")
  } else {
    dt <- dt[c((ctreeParametersControl$learnSize + 1):nrow(dt)), ]
  }

  visitorReward <- visitorReward[c((ctreeParametersControl$learnSize+1):nrow(visitorReward)), ]

  ###  Training  ###

  #  define cluster for each item
  dt$groups <- predict(ctreeTree, newdata=dt, type="node")
  dt$choice <- 0
  dt$regret <- NA
  dt <- cbind(dt, visitorReward)
  groups <- dt$groups

  #  object for stores choices
  choices <- c(rep(0, nrow(visitorReward)))

  CtreeObject <- list()
  #  start ucb for set upper than learn set
  #  get the number of arm
  K = ncol(visitorReward)

  #  source("R/UCB_qui_marche.R")
  source("Bandit/R/UCB_functions.R")
  source("Bandit/R/regret.R")

  tic()

  #  for each groups play a private strategy of ucb
  for (i in levels(as.factor(dt$groups))) {
    message(paste("subset", i, sep = " "))
    #  Subset visitors from this segment
    visitorRewardForCtree <- subset.data.frame(dt, dt$groups == i)
    visitorRewardForCtree <-  visitorRewardForCtree[, (ncol(visitorRewardForCtree) - K + 1):ncol(visitorRewardForCtree)]

    #  UCB results
    ucbTmpRes <- UCB(visitorReward = visitorRewardForCtree, K = K, alpha = ctreeParametersControl$alpha)

    #  update choice vector
    dt[dt$groups == i, ]$choice <- ucbTmpRes$choice
    #  dt[dt$groups==i,]$regret <- SimpleRegret(ucbTmpRes$choice,visitorRewardForCtree )

    ###  past regret on all item database
    # simpleRegret <- SimpleRegret(ucbTmpRes$choice,visitorRewardForCtree )
    # all_regret_CTREE[all_regret_CTREE$segment==i,]$regret <- simpleRegret

    #Save results
    CtreeObject <- c(CtreeObject, list(i, ucbTmpRes))
    rm(ucbTmpRes)
  }

  time <- toc()

  # return  data , models, groups and results
  return(list('dataReward' = visitorReward, 
              'dataContext' = dt, 
              'groups' = groups,
              'ctreeUCB' = CtreeObject,
              'firstTrainElement' = (ctreeParametersControl$learnSize + 1),
              'time' = (time$toc - time$tic),
              'choice' = dt$choice,
              'tree' = ctreeTree))
}