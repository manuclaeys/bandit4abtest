#Source : https://cran.r-project.org/web/packages/KERE/KERE.pdf
#https://socialsciences.mcmaster.ca/jfox/Books/Companion/appendix/Appendix-Nonparametric-Regression.pdf
#



update_model <- function(  choice , M ,update_val){

  #cumpute the regression models

  #subset past data
  A_temp <- subset(M,arm==choice )

  #remove arm number for regression
  A_temp$arm <- NULL

  A_temp <- A_temp[(nrow(A_temp)-update_val+1):nrow(A_temp),]

  #construct the model
  model <- krr(A_temp[,-ncol(M)], A_temp$reward)

  #keep the coeffient
  #th_hat  = model$gamma


  return(list('model'=model))
}

#return TRUE if we need to re cumpute the model
control_update_modulo <- function(choice, M, update_val){

  #subset past data
  A_temp <- subset(M,arm==choice )
  if(nrow(A_temp) %% update_val == 0) return(TRUE)

  return(FALSE)
}



return_variance <- function(choices ,proba, rewards , arm ){

  #subset past data
  val_estim <- proba[choices == 1]

  val_real <- rewards[choices == 1]

  #serror estimate
  error = mapply('-',val_real,val_estim)

  return('var'= var(error,na.rm = TRUE))
}



source("Bandit/R/linucb_parameters_control_default.R")

KERNEL_UCB  <- function(dt,visitorReward,linucb_parameters_control=linucb_parameters_control_default(dt,visitorReward),update_val= 100,K=ncol(visitorReward),delta=1,lambda=1,alpha=1){

  #Parameter
  #eta = alpha*sqrt((log(2*nrow(visitorReward)*K/delta)/(2*lambda)))


  library(listdtr)
  #set to file directory
  (WD <- getwd())
  if (!is.null(WD)) setwd(WD)

  #K arm
  K = ncol(visitorReward)

  #control data
  source("Bandit/R/LINUCB_control.R")
  LINUCB_control(dt=dt,visitorReward = visitorReward, K= K)


  #return time elaps
  library(tictoc)

  #data formating
  visitorReward <- as.matrix(visitorReward)

  #parameter
  #alph   = linucb_parameters_control$alpha

  #Context matrix
  D <- as.matrix(dt)
  colnames(D) <- colnames(D)
  n <- nrow(dt)
  n_f <- ncol(D)

  #Keep the past choice for regression
  choices= list(rep.int(0,n))
  rewards= list(rep.int(0,n))
  proba = list(rep.int(0,n))

  #parameters to modelize
 # th_hat = array(0, c(K,n_f))
 # colnames(th_hat) <- colnames(dt)
 # rownames(th_hat) <- colnames(visitorReward)

  #regression variable
  #past observation
  M <- as.data.frame(D)
  M$arm <- as.numeric(0)
  M$reward <- as.numeric(0)

  it_arm <- as.vector(rep.int(0,K))

  #tempory variable
  p      = list(rep.int(0,K))

  #list of models
  list_K_model <- list()

  #time keeper
  tic()

  #initialization
  for (i in 1:K){
    # play i arm on time
    choices[i] =  i

    #update data stored
    it_arm[i] = 1

    #A[observation,,observation,K] give features observed at i time for arm K
    M[i,'arm'] = i
    M[i,'reward'] = visitorReward[i,i]

    #creat models
    temp_model <- update_model(  choices[i], M,0)
    list_K_model[[i]] <-  temp_model$model
  #  th_hat[i,] = temp_model$th_hat

    proba[i] = 1/K
    rewards[i] = visitorReward[i,as.integer(choices[i])]
  }

  #first round robin
  if(update_val>0){


    for (i in (K+1):(K*update_val+K)){
      # play i arm on time
      choices[i] =  ((i-1)%%K + 1)

      #A[observation,,observation,K] give features observed at i time for arm K
      M[i,'arm'] = ((i-1)%%K + 1)
      M[i,'reward'] = visitorReward[i, ((i-1)%%K + 1)]

      proba[i] = 1/K
      rewards[i] = visitorReward[i,as.integer(choices[i])]
    }


    #update model at the end of the fist round robin
    for(i in 1:K){
      temp_model <- update_model(  i, M ,update_val)
      list_K_model[[i]] <-  temp_model$model
  #    th_hat[i,] = temp_model$th_hat
    }



  }




  for(i in (K*update_val+K+1):n){
    x_i = D[i,]
    for(j in 1:K){



      #predict with confident bound
      proba_temp <- predict(list_K_model[[j]], xnew  = as.data.frame(t(x_i)))

      #dirty solution to improve
      if( is.nan(proba_temp) == TRUE) proba_temp <- predict(list_K_model[[j]], xnew  = as.data.frame(t(x_i)))
      if( is.nan(proba_temp) == TRUE) proba_temp <- as.numeric(p[j])


  #    a_upper_bound = eta*sqrt(return_variance(choices ,proba, rewards , j))
      a_upper_bound =  alpha* sqrt( (2*log(i))/as.integer(it_arm[j] ))
  #    message(a_upper_bound)
  #    message(it_arm[j] )

      a_upper_ci  <- proba_temp  + a_upper_bound


      p[j]       = a_upper_ci      # top CI
    }

    # choose the highest,
    choices[i] =  which.max(p)
 #   message(p)
 #   message(choices[i])

    it_arm[as.integer(choices[i])] = it_arm[as.integer(choices[i])] + 1


    #save probability
    proba[i] = max(unlist(p))

    # see what kind of result we get
    rewards[i] = visitorReward[i,as.integer(choices[i])]

    # update the input vector
    M[i,c(1:ncol(dt))] <- dt[i,]
    M$arm[i] <- choices[i]
    M$reward[i] <- visitorReward[i,as.integer(choices[i])]

    #update the played model each modulo iteration %10
    if(update_val == 0 ){
      temp_model <- update_model(  as.integer(choices[i]), M ,update_val)
      list_K_model[[as.integer(choices[i])]] <-  temp_model$model
   #   th_hat[as.integer(choices[i]),] = temp_model$th_hat

    }else{
      if(control_update_modulo(as.integer(choices[i]), M, update_val)==TRUE){
        message(paste("update", as.integer(choices[i]),sep=" ") )
        temp_model <- update_model(  as.integer(choices[i]), M ,update_val)
        list_K_model[[as.integer(choices[i])]] <-  temp_model$model
      #  th_hat[as.integer(choices[i]),] = temp_model$th_hat
      }
    }

  }

  time <- toc()

  source('Bandit/R/linucb_logit_return_real_theta.R')

  #return real theta from a rigide regression
  #th <- return_real_theta(dt=dt,visitorReward=visitorReward)


  choices <- unlist(choices)

  #return  data , models, groups and results
  return(list('data_reward'=visitorReward,'data_context'=dt,'proba' = proba,'choice'=choices,'time'=(time$toc - time$tic)))

}




