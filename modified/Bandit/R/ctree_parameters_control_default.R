library(partykit)
ctree_parameters_control_default <- function(dt,
                                             visitorReward,
                                             learnSize = as.integer(nrow(dt) * 0.10),
                                             isRewardAreBoolean = FALSE,
                                             armForLearn = names(visitorReward)[1],
                                             explanatoryVariable = names(dt),
                                             alpha = 1,
                                             ctreeControlVal = ctree_control()) {
    return(list(learnSize = learnSize,
                isRewardAreBoolean = isRewardAreBoolean,
                armForLearn = armForLearn,
                explanatoryVariable = explanatoryVariable,
                alpha = alpha,
                ctreeControlVal = ctreeControlVal))
  }
