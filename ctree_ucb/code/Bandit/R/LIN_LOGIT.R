
update_model <- function(  choice , M , update_val){

  #cumpute the regression models

  #subset past data
  A_temp <- subset(M,arm==choice )

  #remove arm number for regression
  A_temp$arm <- NULL

 if(update_val>0) A_temp <- A_temp[(nrow(A_temp)-update_val+1):nrow(A_temp),]



  #construct the model
  model <- glm( reward ~., family=binomial, data=A_temp)

  #keep the coeffient
  th_hat  = model$coefficients[-1]

  return(list('model'=model, 'th_hat'= th_hat))
}



#return TRUE if we need to re cumpute the model
control_update_modulo <- function(choice, M, update_val){

  #subset past data
  A_temp <- subset(M,arm==choice )
  if(nrow(A_temp) %% update_val == 0) return(TRUE)

  return(FALSE)
  }



source("Bandit/R/linucb_parameters_control_default.R")

LINUCB_logit <- function(dt,visitorReward,linucb_parameters_control=linucb_parameters_control_default(dt,visitorReward),update_val= 100){

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
  th_hat = array(0, c(K,n_f))
  colnames(th_hat) <- colnames(dt)
  rownames(th_hat) <- colnames(visitorReward)

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
    temp_model <- update_model(  choices[i], M , update_val = 0)
    list_K_model[[i]] <-  temp_model$model
    th_hat[i,] = temp_model$th_hat

  }

  #first round robin
  if(update_val>0){


    for (i in (K+1):(K*update_val+K)){
      # play i arm on time
      choices[i] =  ((i-1)%%K + 1)

      #A[observation,,observation,K] give features observed at i time for arm K
      M[i,'arm'] = ((i-1)%%K + 1)
      M[i,'reward'] = visitorReward[i, ((i-1)%%K + 1)]

    }


    #update model at the end of the fist round robin
    for(i in 1:K){
      temp_model <- update_model(  i, M , update_val = update_val)
      list_K_model[[i]] <-  temp_model$model
      th_hat[i,] = temp_model$th_hat
    }



  }




  for(i in (K*update_val+K+1):n){
    x_i = D[i,]
    for(j in 1:K){



        #predict with confident bound
        proba <- predict.glm(list_K_model[[j]], newdata = as.data.frame(t(x_i)), type="link",se.fit = TRUE)

        critval <- 1.95 ## approx 95% CI

        a_upper_ci  <- proba$fit + (critval * proba$se.fit)
        a_upper_ci_inv <- list_K_model[[j]]$family$linkinv(a_upper_ci )

        p[j]       = a_upper_ci_inv        # top CI
    }

    # choose the highest,
    choices[i] =  which.max(p)
  #  message(i)

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
      temp_model <- update_model(  as.integer(choices[i]), M , update_val)
      list_K_model[[as.integer(choices[i])]] <-  temp_model$model
      th_hat[as.integer(choices[i]),] = temp_model$th_hat

      }else{
          if(control_update_modulo(as.integer(choices[i]), M, update_val)==TRUE){
          message(paste("update", as.integer(choices[i]),sep=" ") )
          temp_model <- update_model(  as.integer(choices[i]), M ,update_val)
          list_K_model[[as.integer(choices[i])]] <-  temp_model$model
          th_hat[as.integer(choices[i]),] = temp_model$th_hat
          }
      }

  }

  time <- toc()

  source('Bandit/R/linucb_logit_return_real_theta.R')

  #return real theta from a rigide regression
#  th <- return_real_theta(dt=dt,visitorReward=visitorReward)
    th = 0

  choices <- unlist(choices)

  #return  data , models, groups and results
  return(list('data_reward'=visitorReward,'data_context'=dt,'proba' = proba,'theta_hat'=th_hat,'theta'=th,'choice'=choices,'time'=(time$toc - time$tic)))

}




