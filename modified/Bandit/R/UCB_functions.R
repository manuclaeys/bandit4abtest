#  GenerateMaxtixS for keep results
GenerateMatrixS <- function(K) {
  S <- matrix(rep(0, 2 * K), nrow = 2, ncol = K)
  colnames(S) <- paste('bandit', 1:K)
  return(S)
}

#  play arm
PlayArm <- function(iter, arm, S, visitorReward) {
  #  mean
  S[1, arm] <- ((S[1, arm] * S[2, arm] + visitorReward[iter, arm]) / (S[2, arm] + 1))
  #  play
  S[2, arm] = S[2, arm] + 1
  return(S)
}

#  return new mean with new reward
Alloc <- function(arm, iter, visitorReward, S) {
  #  S[2,i] #number of try
  #  S[1,i] #mean at t-1
  return((S[1, arm] * S[2, arm] + visitorReward[iter, arm]) / (S[2, arm] + 1))
}

ConditionForUCB <- function(S, iter, alpha = 1, K) {
  choice <- c()
  for (arm in 1:K) choice[arm] <- S[1, arm] + alpha * sqrt((2 * log(iter)) / S[2, arm])
  return(which.max(choice))
}

ProbaMaxForUCB <- function(S, it, alpha = 1, K) {
  choice <- c()
  for (j in 1:K) choice[j] <- S[1, j] + alpha * sqrt((2 * log(it)) / S[2, j])
  return(max(choice))
}

ChangeType <- function(visitorReward) {
  for (i in 1:ncol(visitorReward)) visitorReward[, i] <- as.double(as.character(visitorReward[, i]))
    return(visitorReward)
}

#  UCB
UCB <- function(visitorReward, K = ncol(visitorReward), alpha = 1) {
  #  control
  source("Bandit/R/UCB_control.R")
  UCBControl(visitorReward = visitorReward, K = K)
  #  change type
  #  visitorReward <- ChangeType(visitorReward)

  #  return time elaps
  library(tictoc)
  #  data formating
  visitorReward <- as.matrix(visitorReward)
  #  keep list of choice
  choice <- c()
  proba <- c()
  S <- GenerateMatrixS(K)

  tic()

  if (K >= nrow(visitorReward)) {
    warning(" more arm than visitors !")
    for (j in 1:nrow(visitorReward)) {
      S <- PlayArm(j, j, S, visitorReward)  #  handle case where there is more arm than visitors
      proba[j] <- ProbaMaxForUCB(S, j, alpha = alpha, K)
    }
    choice[1:nrow(visitorReward)] <- c(1:nrow(visitorReward))
    if (K > nrow(visitorReward)) {
      S[, c(j:K)] <- 0
    }
    return(list('S' = S,'choice' = choice))
  } else {
    ###  initialisation
    for (j in 1:K) {
      S <- PlayArm(iter = j, arm = j, S = S, visitorReward)  #  check if an arm have already been played
      #  message(S)
      choice[1:K] <- c(1:K)
      proba[j] <- ProbaMaxForUCB(S, j, alpha, K)
    }
    for (i in (K + 1):nrow(visitorReward)) {
      #  message(S)
      choice[i] <- ConditionForUCB(S, iter = i, alpha = alpha, K)
      #  message(choice[i])
      S <- PlayArm(iter = i, arm = choice[i], S, visitorReward)
      proba[i] <- ProbaMaxForUCB(S, i, alpha, K)
    }
    time <- toc()
    #  coef estimate
    thHat = S[1, ]
    #real coef
    th = colMeans(visitorReward)
    message("thHat")
    message(thHat)
    message("th real")
    message(th)
    return(list('S' = S, 'choice' = choice, 'proba' = proba, 'time' = (time$toc - time$tic), 'theta_hat' = thHat, 'theta' = th))
  }
}










