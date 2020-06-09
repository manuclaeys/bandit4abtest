#'UCB_rejection_sampling
#'
#'UCB algorithme with rejection sampling method.
#'Exclud any choices which not corresponds to real exepriments in dataset
#'Stop if something is wrong.
#'Generate a matrix to save the results (S).
#' \itemize{ At each iteration,
#'  \item Calculates the arm probabilities,
#'  \item Choose the arm with the maximum upper bound (with alpha parameter)
#'  \item Receives a reward in visitor_reward for the arm and associated iteration
#'  \item Updates the results matrix S.
#'  }
#'Returns the calculation time.
#'Review the estimated, actual averages and number of choices for each arm.
#'See also \code{\link{ConditionForUCB}}, \code{\link{GenerateMatrixS}},
#'\code{\link{ProbaMaxForUCB}} and \code{\link{PlayArm}}.
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item S:numerical matrix of results ,
#'  \item choice: choices of UCB,
#'  \item proba: probability of the chosen arms,
#'  \item time: time of cumputation,
#'  \item theta_hat: mean estimated of each arm
#'  \item theta: real mean of each arm
#'  }
#'
#'@examples
#'## Generates 10000 numbers from 2 binomial  distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#remove data
#'temp_list <- sample(1:nrow(visitor_reward), 500, replace = FALSE, prob = NULL)
#'visitor_reward$K1[temp_list] <- NA
#'visitor_reward$K2[-temp_list] <- NA
#'#run ucb on missing data
#'ucb_alloc  <- UCB_rejection_sampling(visitor_reward,alpha = 10)
#'@import tictoc
#'@export
UCB_rejection_sampling <- function(visitorReward, K=ncol(visitorReward) , alpha = 1){


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

      choice[temp_i] =  temp_i
      ###Si choix réel
      # see what kind of result we get
      rewards[temp_i] = visitorReward[i,temp_i]

      # update the input vector
      S <- PlayArm(iter=i,arm=temp_i,S,visitorReward)
      proba[temp_i] <-  max(ProbaMaxForUCB(S=S, iter=temp_i, alpha=alpha, K))
      temp_i = temp_i +1
    }


    i=i+1
  }



  for(i in i:nrow(visitorReward)){
    #    message(S)
    choice[i] <- ConditionForUCB(S,iter=temp_i,alpha=alpha,K)

    ####Rejection sampling
    ### le bon choix
    if(is.na(visitorReward[i,as.integer(choice[i])])==FALSE){

      ###Si choix réel
      # see what kind of result we get
      rewards[temp_i] = visitorReward[i,as.integer(choice[i])]

      # update the input vector
      S <- PlayArm(iter=i,arm=choice[i],S,visitorReward)
      proba[i] <-  max(ProbaMaxForUCB(S=S, iter=i, alpha=alpha, K))
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

