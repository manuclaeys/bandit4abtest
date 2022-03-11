#'Uniform Rejection Sampling algorithm
#'
#'Control data in visitor_reward with \code{\link{BanditRewardControl}}
#'Stop if something is wrong.
#'Generate a matrix to save the results (S).
#' \itemize{ At each iteration
#'  \item Choose alternatively an arm
#'  \item Receives a reward in visitor_reward for the arm and associated iteration
#'  \item Updates the results matrix S.
#'  }
#'Returns the calculation time.
#'Return the estimated and number of choices for each arm.
#'See also  \code{\link{GenerateMatrixS}} and \code{\link{PlayArm}}.
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item S:numerical matrix of results ,
#'  \item choice: choices of UCB,
#'  \item time: time of cumputation,
#'  }
#'
#'@examples
#'## TODO
#'@import tictoc
#'@export
#UniformBandit Rejection Sampling
UniformBanditRejectionSampling <- function(visitorReward, K=ncol(visitor_reward)) {

  #data formating
  visitorReward <- as.matrix(visitorReward)

  #keep list of choice
  #keep list of choice
  choice <- c()
  proba <- c()
  rewards <- c()
  S <- GenerateMatrixS(K)

  tic()

  #tempo variables
  temp_i=1
  i=1

  ###initialisation
  while(temp_i<(K+1)){

    ####Rejection sampling
    ### le bon choix
    if(is.na(visitorReward[i,temp_i])==FALSE){

      choice[i] =  temp_i
      ###Si choix réel
      # see what kind of result we get
      rewards[temp_i] = visitorReward[i,temp_i]

      # update the input vector
      S <- PlayArm(iter=i,arm=temp_i,S,visitorReward)
      proba[temp_i] <-  0
      temp_i = temp_i +1
    }else{
      choice[i] = NA
    }


    i=i+1
  }



  for(i in i:nrow(visitorReward)){
    #    message(S)
    choice[i] <- as.integer(which.min(S[2,]))

    ####Rejection sampling
    ### le bon choix
    if(is.na(visitorReward[i,as.integer(choice[i])])==FALSE){

      ###Si choix réel
      # see what kind of result we get
      rewards[temp_i] = visitorReward[i,as.integer(choice[i])]

      # update the input vector
      S <- PlayArm(iter=i,arm=choice[i],S,visitorReward)
      proba[i] <-  0
      temp_i = temp_i +1
    }else{
      choice[i]= NA
    }

  }

  time <- toc()

  options(scipen=999)

  #coef estimate
  th_hat=S[1,]

  #real coef
  th = colMeans(visitorReward,na.rm =TRUE)


  message("th_hat")
  message(th_hat)
  message("th real")
  message(th)

  message(paste('number of used items', sum(S[2,1],S[2,2])),', number of excluded items :',(nrow(visitorReward) - sum(S[2,1],S[2,2])), sep= " " )

  return (list('S'=S,'choice'= choice,'proba' = proba,'time'=(time$toc - time$tic),'theta_hat'=th_hat,'theta'=th))

}
