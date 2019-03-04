###  Regret
CumulativeRegret <- function(choice, visitorReward) {
  regret <- vector()
  visitorReward <- as.matrix(visitorReward)
  regret <- SimpleRegret(as.vector(choice), visitorReward)
  # plot(regret, type = 'l', ylim = c(0, nrow(visitorReward)))  #  Not very clear
  plot(cumsum(regret), type = 'l')
  return(cumsum(regret))
}

RegretValue <- function(valChoice, vecVisitorReward) {
  return(vecVisitorReward[which.max(vecVisitorReward)] - vecVisitorReward[valChoice])
}

SimpleRegret <- function(choice, visitorReward) {
  regret <- c()
  for (i in 1:nrow(visitorReward)) {
    regret[i] <- regretValue(as.integer(choice[i]), visitorReward[i, ])
  }
  #  plot(regret, type = 'l', ylim = c(0, nrow(visitorReward)))  #  Not very clear
  #  plot(regret, type = 'l')
  return(regret)
}