#  initialisation
K <- 5                #  number of arms (? changer)
Time <- 1             #  number of try for estimate mean in exploration
iteration <- 10000    #  number of iteration

PlayArm <- function(i, j, S, visitorReward) {
  S[1, i] <- Alloc(i, j, S, visitorReward)
  S[2, i] =  S[2, i] + 1
  return(S)
}

Alloc <- function(i, j, S, visitorReward) {
  #  S[2, i] #number of try
  #  S[1, i] #mean at t-1
  return((S[1, i] * S[2, i] + visitorReward[j, i]) / (S[2, i] + 1))
}

ConditionForUCB <- function(S, it, alpha=1) {
  choice <- c()
  for (j in 1:K) choice[j] <- S[1, j] + alpha * sqrt((2 * log(it)) / S[2, j])
  return(which.max(choice))
}

UCBWithoutProba <- function(S, alpha = 1, visitorReward, K) {
  #  keep list of choice
  choice <- c()
  #  proba <- c()

  if (K >= nrow(visitorReward)) {
    for (j in 1:nrow(visitorReward)) {
      S <- PlayArm(j, j, S, visitorReward)  #  handle case where there is more arm than visitors
      assign("S", S, envir = .GlobalEnv)
    }
    choice[1:nrow(visitorReward)] <- c(1:nrow(visitorReward))
    #proba[1:nrow(visitorReward)] <- max(prob_winner(sim_post(S[1,],S[2,])))
    return(list('S' = S, 'choice' = choice))
  } else {
    ###  initialisation
    for (j in 1:K) { 
      S <- PlayArm(j, j, S, visitorReward)  #  check if an arm have already been played
      assign("S", S, envir = .GlobalEnv)
      choice[1:K] <- c(1:K)
      #  proba[1:K] <- max(prob_winner(sim_post(S[1,],S[2,])))
    }
    for (i in (K + 1):nrow(visitorReward)) {
      choice[i] <- ConditionForUCB(S, i, alpha)
      #  proba[i] <- max(prob_winner(sim_post(S[1,],S[2,])))
      S <- PlayArm(choice[i], i, S, visitorReward)
      assign("S", S, envir = .GlobalEnv)
    }
    return(list('S' = S,'choice' = choice))
  }
}

CumulativeRegret <- function(choice, visitorReward) {
  regret <- c()
  for (i in 1:nrow(visitorReward)) {
    if (i == 1) regret <- c(RegretValue(as.integer(choice[i]), visitorReward[i, ]))
    if (i > 1) regret <- c(regret, tail(regret, n = 1) + RegretValue(as.integer(choice[i]), visitorReward[i, ]))
  }
  #  plot(regret, type='l', ylim=c(0, nrow(visitorReward))) #Not very clear
  plot(regret, type = 'l')
  return(regret)
}

GenerateMatrixS <- function(K) {
  S <- matrix(rep(0, 2 * K), nrow = 2, ncol = K)
  colnames(S) <- paste('bandit', 1:K)
  return(S)
}

RegretValue <- function(val_choice, vec_visitorReward) {
  return(vec_visitorReward[which.max(vec_visitorReward)] - vec_visitorReward[val_choice])
}

UCBQuiMarche <- function(visitorReward, K = ncol(visitorReward)) {
  S <- GenerateMatrixS(K)
  #  assign("S",S, envir = .GlobalEnv)
  assign("K", K, envir = .GlobalEnv)
  S <- UCBWithoutProba(S, visitorReward = visitorReward, K = K)
  return(S)
}

GenerateMaxtixVisitorReward <- function(K, Time, iteration) {
  n <- K * Time * iteration
  visitorReward <- matrix(0, nrow = n, ncol = K)
  #  Simulation
  #  p.win <- simProba(1)
  p.win <- c(0.1, 0.2, 0.2, 0.1, 0.8)
  sum(p.win)
  # for(i in 1:K) visitorReward[, i] <- rbinom(n, 1, p.win[i][1])
  for(i in 1:K) visitorReward[, i] <- p.win[i]
  summary(visitorReward)
  return(visitorReward)
}

#  visitorReward <- GenerateMaxtixVisitorReward(K,Time,iteration)
#  S <- UCBQuiMarche(visitorReward,K)
#  regret <- CumulativeRegret(S$choice,visitorReward)
#  plot(regret,type='l',xlab = "item")
#  summary(visitorReward)