#  GenerateMaxtixS for keep results
GenerateMaxtixS <- function(K) {
  S <- matrix(rep(0, 2 * K), nrow = 2, ncol = K)
  colnames(S) <- paste('bandit', 1:K)
  return(S)
}

#  PlayArm
PlayArm <- function(i, j, S) {
  S[1, i] <- alloc(i, j, visitorReward, S)
  S[2, i] =  S[2, i] + 1
  return(S)
}

#  return new mean with new reward
Alloc <- function(i, j, visitorReward, S) {
  #  S[2, i]  #  number of try
  #  S[1, i]  #  mean at t-1
  return((S[1, i] * S[2, i] + visitorReward[j, i]) / (S[2, i] + 1))
}

ChangeType <- function(visitorReward) {
  for (i in 1:ncol(visitorReward)) visitorReward[, i] <- as.double(as.character(visitorReward[, i]))
  return(visitorReward)
}

#  random
RandomBandit <- function(visitorReward, K = ncol(visitorReward)) {
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
  S <- GenerateMaxtixS(K)
  tic()
  if (K > nrow(visitorReward)) {
    warning("more arm than visitors !")
    for (j in 1:nrow(visitorReward)) {
      S <- play_arm(j, j, S)  #  handle case where there is more arm than visitors
    }
    choice[1:nrow(visitorReward)] <- c(1:nrow(visitorReward))
    return(list('S' = S, 'choice' = choice))
  } else {
    for (i in 1:nrow(visitorReward)) {
      choice[i] <- ((i - 1)%%K + 1)
      S <- play_arm(choice[i], i, S)
    }
    time <- toc()
    return(list('S' = S, 'choice' = choice, 'time' = (time$toc - time$tic)))
  }
}










