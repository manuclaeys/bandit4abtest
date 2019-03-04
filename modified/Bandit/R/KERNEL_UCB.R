#  Source : https://cran.r-project.org/web/packages/KERE/KERE.pdf
#  https://socialsciences.mcmaster.ca/jfox/Books/Companion/appendix/Appendix-Nonparametric-Regression.pdf
#

UpdateModel <- function(choice, M, updateVal) {
  #  cumpute the regression models
  #  subset past data
  ATmp <- subset(M, arm == choice)
  #  remove arm number for regression
  ATmp$arm <- NULL
  ATmp <- ATmp[(nrow(ATmp) - updateVal + 1):nrow(ATmp), ]
  #  construct the model
  model <- krr(ATmp[, -ncol(M)], ATmp$reward)
  #  keep the coeffient
  #  thHat  = model$gamma
  return(list('model' = model))
}

#  return TRUE if we need to re cumpute the model
ControlUpdateModulo <- function(choice, M, updateVal) {
  #  subset past data
  ATmp <- subset(M, arm == choice)
  if (nrow(ATmp) %% updateVal == 0) return(TRUE)
  return(FALSE)
}

ReturnVariance <- function(choices, proba, rewards, arm) {
  #  subset past data
  valEstim <- proba[choices == 1]
  valReal <- rewards[choices == 1]
  #  serror estimate
  error = mapply('-', val_real, val_estim)
  return('var' = var(error, na.rm = TRUE))
}

source("Bandit/R/linucb_parameters_control_default.R")

KernelUCB  <- function(dt, 
                       visitorReward,
                       LinUCBParametersControl = LinUCBParametersControlDefault(dt, visitorReward),
                       updateVal = 100,
                       K = ncol(visitorReward),
                       delta = 1,
                       lambda = 1,
                       alpha = 1) {

  #  Parameter
  #eta = alpha*sqrt((log(2*nrow(visitorReward)*K/delta)/(2*lambda)))

  library(listdtr)
  #  set to file directory
  (WD <- getwd())
  if (!is.null(WD)) setwd(WD)

  #  K arm
  K = ncol(visitorReward)

  #  control data
  source("Bandit/R/LINUCB_control.R")
  LINUCB_control(dt=dt,visitorReward = visitorReward, K= K)


  #  return time elaps
  library(tictoc)
  #  data formating
  visitorReward <- as.matrix(visitorReward)
  
  #  parameter
  #alph   = linucb_parameters_control$alpha

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
  # thHat = array(0, c(K,nF))
  # colnames(thHat) <- colnames(dt)
  # rownames(thHat) <- colnames(visitorReward)

  #  regression variable
  #  past observation
  M <- as.data.frame(D)
  M$arm <- as.numeric(0)
  M$reward <- as.numeric(0)

  itArm <- as.vector(rep.int(0, K))

  #  tempory variable
  p = list(rep.int(0, K))

  #  list of models
  listKModel <- list()

  #  time keeper
  tic()

  #initialization
  for (i in 1:K) {
    #  play i arm on time
    choices[i] =  i

    #  update data stored
    itArm[i] = 1

    #A[observation,,observation,K] give features observed at i time for arm K
    M[i, 'arm'] = i
    M[i, 'reward'] = visitorReward[i, i]

    #  create models
    tempModel <- updateModel(choices[i], M, 0)
    listKModel[[i]] <- tempModel$model
    #  thHat[i,] = tempModel$thHat

    proba[i] = 1 / K
    rewards[i] = visitorReward[i, as.integer(choices[i])]
  }

  #  first round robin
  if (updateVal > 0) {
    for (i in (K + 1):(K * updateVal + K)) {
      #  play i arm on time
      choices[i] =  ((i - 1)%%K + 1)
      #A[observation,,observation,K] give features observed at i time for arm K
      M[i, 'arm'] = ((i - 1)%%K + 1)
      M[i, 'reward'] = visitorReward[i, ((i - 1)%%K + 1)]
      proba[i] = 1 / K
      rewards[i] = visitorReward[i, as.integer(choices[i])]
    }
    #  update model at the end of the fist round robin
    for (i in 1:K) {
      tempModel <- updateModel(i, M, updateVal)
      listKModel[[i]] <- tempModel$model
      #  thHat[i,] = tempModel$thHat
    }
  }

  for (i in (K * updateVal + K + 1):n) {
    xI = D[i, ]
    for(j in 1:K) {
      #  predict with confident bound
      probaTmp <- predict(listKModel[[j]], xnew = as.data.frame(t(xI)))
      #  dirty solution to improve
      if(is.nan(probaTmp) == TRUE) probaTmp <- predict(listKModel[[j]], xnew = as.data.frame(t(xI)))
      if(is.nan(probaTmp) == TRUE) probaTmp <- as.numeric(p[j])
      #  aUpperBound = eta*sqrt(return_variance(choices ,proba, rewards , j))
      aUpperBound = alpha* sqrt((2 * log(i)) / as.integer(itArm[j]))
      #  message(aUpperBound)
      #  message(itArm[j])
      aUpperCi <- probaTmp + aUpperBound
      p[j] = aUpperCi  #  top CI
    }

    #  choose the highest,
    choices[i] =  which.max(p)
    #  message(p)
    #  message(choices[i])

    itArm[as.integer(choices[i])] = itArm[as.integer(choices[i])] + 1


    #  save probability
    proba[i] = max(unlist(p))

    #  see what kind of result we get
    rewards[i] = visitorReward[i, as.integer(choices[i])]

    #  update the input vector
    M[i, c(1:ncol(dt))] <- dt[i, ]
    M$arm[i] <- choices[i]
    M$reward[i] <- visitorReward[i, as.integer(choices[i])]

    #  update the played model each modulo iteration %10
    if (updateVal == 0 ) {
      tempModel <- updateModel(as.integer(choices[i]), M, updateVal)
      listKModel[[as.integer(choices[i])]] <- tempModel$model
      #   thHat[as.integer(choices[i]),] = tempModel$thHat
    } else {
      if (ControlUpdateModulo(as.integer(choices[i]), M, updateVal) == TRUE) {
        message(paste("update", as.integer(choices[i]), sep = " "))
        tempModel <- updateModel(as.integer(choices[i]), M, updateVal)
        listKModel[[as.integer(choices[i])]] <- tempModel$model
        #  thHat[as.integer(choices[i]), ] = tempModel$thHat
      }
    }
  }

  time <- toc()

  source('Bandit/R/linucb_logit_return_real_theta.R')

  #  return real theta from a rigide regression
  #  th <- return_real_theta(dt=dt,visitorReward=visitorReward)

  choices <- unlist(choices)

  #return data, models, groups and results
  return(list('dataReward' = visitorReward, 'dataContext' = dt, 'proba' = proba, 'choice' = choices, 'time' = (time$toc - time$tic)))
}




