UpdateModel <- function(choice, M, updateVal) {
  #  cumpute the regression models
  #  subset past data
  ATemp <- subset(M, arm == choice)
  #  remove arm number for regression
  ATemp$arm <- NULL
  if (updateVal > 0) ATemp <- ATemp[(nrow(ATemp) - updateVal + 1):nrow(ATemp), ]
  #  construct the model
  model <- glm( reward ~., family = binomial, data = ATemp)
  #  keep the coeffient
  thHat = model$coefficients[-1]
  return(list('model' = model, 'thHat' = thHat))
}

#  return TRUE if we need to re cumpute the model
ControlUpdateModulo <- function(choice, M, updateVal) {
  #  subset past data
  ATemp <- subset(M, arm == choice)
  if (nrow(ATemp) %% updateVal == 0) return(TRUE)
  return(FALSE)
}

source("Bandit/R/linucb_parameters_control_default.R")

LINUCBLogit <- function(dt, visitorReward, LinUCBParametersControl = LinUCBParametersControlDefault(dt, visitorReward), updateVal = 100) {
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
  #  alph = LinUCBParametersControl$alpha

  #  Context matrix
  D <- as.matrix(dt)
  colnames(D) <- colnames(D)
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
  #  past observation
  M <- as.data.frame(D)
  M$arm <- as.numeric(0)
  M$reward <- as.numeric(0)

  itArm <- as.vector(rep.int(0,K))

  #  tempory variable
  p = list(rep.int(0, K))
  #  list of models
  listKModel <- list()

  #  time keeper
  tic()

  #  initialization
  for (i in 1:K) {
    #  play i arm on time
    choices[i] = i
    #  update data stored
    itArm[i] = 1
    #  A[observation, , observation, K] give features observed at i time for arm K
    M[i, 'arm'] = i
    M[i, 'reward'] = visitorReward[i, i]
    # create models
    tempModel <- UpdateModel(choices[i], M, updateVal = 0)
    listKModel[[i]] <- tempModel$model
    thHat[i, ] = tempModel$thHat
  }
  #  first round robin
  if (updateVal > 0) {
    for (i in (K + 1):(K * updateVal + K)) {
      #  play i arm on time
      choices[i] = ((i - 1) %% K + 1)
      #  A[observation, , observation, K] give features observed at i time for arm K
      M[i, 'arm'] = ((i - 1) %% K + 1)
      M[i, 'reward'] = visitorReward[i, ((i - 1) %% K + 1)]
    }
    #  update model at the end of the fist round robin
    for (i in 1:K) {
      tempModel <- UpdateModel(i, M, updateVal = updateVal)
      listKModel[[i]] <- tempModel$model
      thHat[i, ] = tempModel$thHat
    }
  }
  for (i in (K * updateVal + K + 1):n) {
    x_i = D[i, ]
    for (j in 1:K) {
        #  predict with confident bound
        proba <- predict.glm(listKModel[[j]], newdata = as.data.frame(t(x_i)), type = "link", se.fit = TRUE)
        critval <- 1.95  ##  approx 95% CI
        a_upper_ci <- proba$fit + (critval * proba$se.fit)
        a_upper_ci_inv <- listKModel[[j]]$family$linkinv(a_upper_ci)
        p[j] = a_upper_ci_inv  #  top CI
    }
    #  choose the highest,
    choices[i] = which.max(p)
    #  message(i)
    #  save probability
    proba[i] = max(unlist(p))
    #  see what kind of result we get
    rewards[i] = visitorReward[i, as.integer(choices[i])]
    #  update the input vector
    M[i, c(1:ncol(dt))] <- dt[i, ]
    M$arm[i] <- choices[i]
    M$reward[i] <- visitorReward[i, as.integer(choices[i])]
    # update the played model each modulo iteration %10
    if(updateVal == 0) {
      tempModel <- UpdateModel(as.integer(choices[i]), M , updateVal)
      listKModel[[as.integer(choices[i])]] <- tempModel$model
      thHat[as.integer(choices[i]), ] = tempModel$thHat
    } else {
      if (ControlUpdateModulo(as.integer(choices[i]), M, updateVal) == TRUE) {
        message(paste("update", as.integer(choices[i]), sep = " "))
        tempModel <- UpdateModel(as.integer(choices[i]), M, updateVal)
        listKModel[[as.integer(choices[i])]] <- tempModel$model
        thHat[as.integer(choices[i]), ] = tempModel$thHat
      }
    }
  }
  time <- toc()
  source('Bandit/R/linucb_logit_return_real_theta.R')
  #  return real theta from a rigide regression
  #  th <- return_real_theta(dt=dt,visitorReward=visitorReward)
  th = 0
  choices <- unlist(choices)
  #return data , models, groups and results
  return(list('dataReward' = visitorReward,'dataContext' = dt,'proba' = proba,'thetaHat' = thHat, 'theta'= th, 'choice' = choices, 'time' = (time$toc - time$tic)))
}




