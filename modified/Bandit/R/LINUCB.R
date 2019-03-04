source("Bandit/R/linucb_parameters_control_default.R")

LINUCB <- function(dt, visitorReward, linUCBParametersControl = LinUCBParametersControlDefault(dt, visitorReward)) {
  #  set to file directory
  (WD <- getwd())
  if (!is.null(WD)) setwd(WD)
  #  K arm
  K = ncol(visitorReward)
  #  control data
  source("Bandit/R/LINUCB_control.R")
  LINUCBControl(dt = dt, visitorReward = visitorReward, K = K)
  #  return time elaps
  library(tictoc)
  #  data formating
  visitorReward <- as.matrix(visitorReward)
  #  parameter
  alph = linUCBParametersControl$alpha
  #  Context matrix
  D <- as.matrix(dt)
  n <- nrow(dt)
  nF <- ncol(D)
  #  Keep the past choice for regression
  choices = list(rep.int(0, n))
  rewards = list(rep.int(0, n))
  proba = list(rep.int(0, n))
  #  parameters to modelize
  thHat = array(0, c(K, nF))
  colnames(thHat) <- colnames(dt)
  rownames(thHat) <- colnames(visitorReward)
  #  regression variable
  b <- matrix(0, K, nF)
  A <- array(0, c(nF, nF, K))
  #  tempory variable
  p = list(rep.int(0, K))
  #  time keeper
  tic()
  #initialization
  for (j in 1:K) {
    A[, , j] = diag(nF)
  }
  for(i in 1:n) {
    x_i = D[i, ]
    for(j in 1:K) {
      A_inv      = solve(A[, , j])
      thHat[j, ] = A_inv %*% b[j, ]
      ta         = t(x_i) %*% A_inv %*% x_i
      a_upper_ci = alph * sqrt(ta)             #  upper part of variance interval
      a_mean     = thHat[j, ] %*% x_i          #  current estimate of mean
      p[j]       = a_mean + a_upper_ci         #  top CI
    }
    #  choose the highest,
    choices[i] = which.max(p)
    #  save probability
    proba[i] = max(unlist(p))
    #  see what kind of result we get
    rewards[i] = visitorReward[i,as.integer(choices[i])]
    #  update the input vector
    A[, , as.integer(choices[i])] = A[, , as.integer(choices[i])] + x_i %*% t(x_i)
    b[as.integer(choices[i]), ] = b[as.integer(choices[i]), ] + x_i * as.numeric(rewards[i])
  }
  time <- toc()

  source('Bandit/R/linucb_return_real_theta.R')
  #  return real theta from a rigide regression
  th <- ReturnRealTheta(dt = dt, visitorReward = visitorReward)
  #  return data, models, groups and results
  return(list('dataReward' = visitorReward, 'dataContext' = dt, 'proba' = proba, 'theta_hat' = thHat, 'theta' = th, 'choice' = choices, 'time' = (time$toc - time$tic)))

}
