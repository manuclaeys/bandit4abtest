#'kernelucb algorithm
#'
#'Control data in visitor_reward with \code{\link{BanditRewardControl}}
#'Stop if something is wrong.
#' \itemize{ At each iteration
#'  \item Calculates the arm expectedbilities according to a kernel regression of context in dt dataframe
#'  \item Choose the arm with the maximum upper bound (with alpha parameter)
#'  \item Receives a reward in visitor_reward for the arm and associated iteration
#'  \item Updates the results
#'  }
#'Returns the calculation time.
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param dt Dataframe of integer or numeric values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'@param update_val limit the number of items used for cumpute the regression
#'
#'@return
#' \itemize{ List of element:
#'  \item choice: choices of kernelucb,
#'  \item expected: expected reward of the chosen arms,
#'  \item time: time of cumputation
#'  }
#'
#'@examples
#'size.tot = 1000
#'set.seed(4649)                          # this makes the example exactly reproducible
#'x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
#'x2 = runif(size.tot, min=0, max=10)
#'x3 = runif(size.tot, min=0, max=10)
#'x4 = runif(size.tot, min=0, max=10)
#'dt = cbind(x1,x2,x3,x4)
#'#arm reward
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = crossprod(t(dt),arm_1)
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = crossprod(t(dt),arm_2)
#'arm_3 <-  as.vector(c(-1,-5,1,10))
#'K3 = crossprod(t(dt),arm_3)
#'visitor_reward <-  data.frame(K1,K2,K3)
#'dt <- as.data.frame(dt)
#'kernelucb(dt,visitor_reward,update_val= 100,K=ncol(visitorReward),delta=1,lambda=1,alpha=1)
#'@import tictoc
#'@export
#kernelucb
kernelucb  <- function(dt,visitor_reward,update_val= 100,K=ncol(visitor_reward),delta=1,lambda=1,alpha=1){

  #Parameter
  #eta = alpha*sqrt((log(2*nrow(visitor_reward)*K/delta)/(2*lambda)))

  #set to file directory
  (WD <- getwd())
  if (!is.null(WD)) setwd(WD)

  #K arm
  K = ncol(visitor_reward)

  #control data
  #control data
  DataControlK(visitor_reward, K = K)
  DataControlContextReward(dt, visitor_reward)

  #data formating
  visitor_reward <- as.matrix(visitor_reward)

  #Context matrix
  D <- as.matrix(dt)
  colnames(D) <- colnames(D)
  n <- nrow(dt)
  n_f <- ncol(D)

  #Keep the past choice for regression
  choices= list(rep.int(0,n))
  rewards= list(rep.int(0,n))
  expected = list(rep.int(0,n))

  #parameters to modelize
  # th_hat = array(0, c(K,n_f))
  # colnames(th_hat) <- colnames(dt)
  # rownames(th_hat) <- colnames(visitor_reward)

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
    M[i,'reward'] = visitor_reward[i,i]

    #creat models
    temp_model <- update_model_for_KernelUCB(  choices[i], M,0)
    list_K_model[[i]] <-  temp_model$model
    #  th_hat[i,] = temp_model$th_hat

    expected[i] = 1/K
    rewards[i] = visitor_reward[i,as.integer(choices[i])]
  }

  #first round robin
  if(update_val>0){


    for (i in (K+1):(K*update_val+K)){
      # play i arm on time
      choices[i] =  ((i-1)%%K + 1)

      #A[observation,,observation,K] give features observed at i time for arm K
      M[i,'arm'] = ((i-1)%%K + 1)
      M[i,'reward'] = visitor_reward[i, ((i-1)%%K + 1)]

      expected[i] = 1/K
      rewards[i] = visitor_reward[i,as.integer(choices[i])]
    }


    #update model at the end of the fist round robin
    for(i in 1:K){
      temp_model <- update_model_for_KernelUCB(  i, M ,update_val)
      list_K_model[[i]] <-  temp_model$model
      #    th_hat[i,] = temp_model$th_hat
    }



  }




  for(i in (K*update_val+K+1):n){
    x_i = D[i,]
    for(j in 1:K){



      #predict with confident bound
      expected_temp <- predict(list_K_model[[j]], xnew  = as.data.frame(t(x_i)))

      #dirty solution to improve
      if( is.nan(expected_temp) == TRUE) expected_temp <- predict(list_K_model[[j]], xnew  = as.data.frame(t(x_i)))
      if( is.nan(expected_temp) == TRUE) expected_temp <- as.numeric(p[j])


      #    a_upper_bound = eta*sqrt(return_variance(choices ,expected, rewards , j))
      a_upper_bound =  alpha* sqrt( (2*log(i))/as.integer(it_arm[j] ))
      #    message(a_upper_bound)
      #    message(it_arm[j] )

      a_upper_ci  <- expected_temp  + a_upper_bound


      p[j]       = a_upper_ci      # top CI
    }

    # choose the highest,
    choices[i] =  which.max(p)
    #   message(p)
    #   message(choices[i])

    it_arm[as.integer(choices[i])] = it_arm[as.integer(choices[i])] + 1


    #save expectedbility
    expected[i] = max(unlist(p))

    # see what kind of result we get
    rewards[i] = visitor_reward[i,as.integer(choices[i])]

    # update the input vector
    M[i,c(1:ncol(dt))] <- dt[i,]
    M$arm[i] <- choices[i]
    M$reward[i] <- visitor_reward[i,as.integer(choices[i])]

    #update the played model each modulo iteration %10
    if(update_val == 0 ){
      temp_model <- update_model_for_KernelUCB(  as.integer(choices[i]), M ,update_val)
      list_K_model[[as.integer(choices[i])]] <-  temp_model$model
      #   th_hat[as.integer(choices[i]),] = temp_model$th_hat

    }else{
      if(control_update_modulo(as.integer(choices[i]), M, update_val)==TRUE){
        message(paste("update", as.integer(choices[i]),sep=" ") )
        temp_model <- update_model_for_KernelUCB(  as.integer(choices[i]), M ,update_val)
        list_K_model[[as.integer(choices[i])]] <-  temp_model$model
        #  th_hat[as.integer(choices[i]),] = temp_model$th_hat
      }
    }

  }

  time <- toc()


  #return real theta from a rigide regression
  #th <- return_real_theta(dt=dt,visitor_reward=visitor_reward)


  choices <- unlist(choices)

  #return  data , models, groups and results
  return(list('expected' = expected,'choice'=choices,'time'=(time$toc - time$tic)))

}




