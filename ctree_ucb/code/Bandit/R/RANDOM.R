
#generate_Maxtix_S for keep results
generate_Maxtix_S <- function(K){
  S <- matrix( rep(0,2*K),nrow = 2, ncol = K )
  colnames(S) <- paste('bandit', 1:K)
  return(S)
}

#play arm
play_arm <- function(i,j,S){
  S[1,i] <- alloc(i,j,visitorReward,S)
  S[2,i] =  S[2,i] + 1
  return(S)
}

#return new mean with new reward
alloc <- function(i,j,visitorReward,S){
  #  S[2,i] #number of try
  #  S[1,i] #mean at t-1
  return( (  S[1,i] *  S[2,i] + visitorReward[j,i] ) /  ( S[2,i] + 1 ) )

}




change_type <- function(visitorReward){
  for(i in 1:ncol(visitorReward)) visitorReward[,i] <- as.double(as.character(visitorReward[,i]))

  return(visitorReward)
}

#random
random_bandit <- function(visitorReward, K=ncol(visitorReward) ){

  #control
  source("Bandit/R/UCB_control.R")
  UCB_control(visitorReward = visitorReward, K= K)

  #change type
  # visitorReward <- change_type(visitorReward)

  #return time elaps
  library(tictoc)

  #data formating
  visitorReward <- as.matrix(visitorReward)


  #keep list of choice
  choice <- c()
  S <- generate_Maxtix_S(K)

  tic()

  if(K > nrow(visitorReward)){
    warning(" more arm than visitors !")

    for(j in 1:nrow(visitorReward)){
      S <- play_arm(j,j,S)        #handle case where there is more arm than visitors

    }
    choice[1:nrow(visitorReward)] <- c(1:nrow(visitorReward))
    return(list('S'=S,'choice'= choice))

  }else{


    for(i in 1:nrow(visitorReward)){
      choice[i] <- ((i-1)%%K + 1)
      S <- play_arm(choice[i],i,S)

    }

    time <- toc()


    return(list('S'=S,'choice'= choice,'time'=(time$toc - time$tic)))

  }
}










