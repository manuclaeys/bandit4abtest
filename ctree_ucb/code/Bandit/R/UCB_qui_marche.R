play_arm <- function(i,j,S,visitorReward){
  S[1,i] <- alloc(i,j,S,visitorReward)
  S[2,i] =  S[2,i] + 1
  return(S)
}




alloc <- function(i,j,S,visitorReward){
  #  S[2,i] #number of try
  #  S[1,i] #mean at t-1
  return( (  S[1,i] *  S[2,i] + visitorReward[j,i] ) /  ( S[2,i] + 1 ) )

}



condition_For_UCB <- function(S,it,alpha=1){
  choice <- c()
  for(j in 1:K) choice[j] <- S[1,j] + alpha* sqrt( (2*log(it))/S[2,j] )
  return(which.max(choice))
}


UCB_without_proba <- function(S,alpha=1,visitorReward,K){
  #keep list of choice
  choice <- c()
  #proba <- c()

  if(K >= nrow(visitorReward)){
    for(j in 1:nrow(visitorReward)){
      S <- play_arm(j,j,S,visitorReward)        #handle case where there is more arm than visitors
      assign("S",S, envir = .GlobalEnv)

    }
    choice[1:nrow(visitorReward)] <- c(1:nrow(visitorReward))
    #proba[1:nrow(visitorReward)] <- max(prob_winner(sim_post(S[1,],S[2,])))
    return(list('S'=S,'choice'= choice))



  }else{

    ###initialisation
    for(j in 1:K){ S <- play_arm(j,j,S,visitorReward)         #check if an arm have already been played
    assign("S",S, envir = .GlobalEnv)
    choice[1:K] <- c(1:K)
    #  proba[1:K] <- max(prob_winner(sim_post(S[1,],S[2,])))
    }

    for(i in (K+1):nrow(visitorReward)){
      choice[i] <- condition_For_UCB(S,i,alpha)
      #  proba[i] <- max(prob_winner(sim_post(S[1,],S[2,])))

      S <- play_arm(choice[i],i,S,visitorReward)
      assign("S",S, envir = .GlobalEnv)
    }

    return(list('S'=S,'choice'= choice))

  }

}

cumulativeRegret <- function(choice,visitorReward){

  regret <- c()

  for(i in 1:nrow(visitorReward)){
    if(i==1) regret <- c(regretValue(as.integer(choice[i]),visitorReward[i,]))
    if(i>1) regret <-  c(regret ,  tail(regret, n=1) + regretValue(as.integer(choice[i]),visitorReward[i,]) )
  }
  #plot(regret,type='l',ylim=c(0, nrow(visitorReward))) #Not very clear
  plot(regret,type='l')
  return(regret)
}


generate_Maxtix_S <- function(K){
  S <- matrix( rep(0,2*K),nrow = 2, ncol = K )
  colnames(S) <- paste('bandit', 1:K)
  return(S)
}



regretValue <- function(val_choice,vec_visitorReward){
  return( vec_visitorReward[which.max(vec_visitorReward)] - vec_visitorReward[val_choice] )

}


UCB_qui_marche <- function(visitorReward,K=ncol(visitorReward)){

S <- generate_Maxtix_S(K)
#assign("S",S, envir = .GlobalEnv)



assign("K", K, envir = .GlobalEnv)
S <- UCB_without_proba(S,visitorReward = visitorReward,K=K)
return(S)

}


#initialisation
K <- 5                             #number of arms (? changer)
Time <- 1                        #number of try for estimate mean in exploration
iteration <- 10000                #number of iteration


generate_Maxtix_Visitor_Reward  <- function(K,Time,iteration){
  n <- K*Time*iteration
  visitorReward <-   matrix( 0 ,nrow = n, ncol = K )
  #Simulation
  #p.win <- simProba(1)
  p.win <- c(0.1,0.2,0.2,0.1,0.8)
  sum(p.win)
  # for(i in 1:K)  visitorReward[,i] <- rbinom(n,1,p.win[i][1])
  for(i in 1:K)  visitorReward[,i] <- p.win[i]
  summary(visitorReward)
  return(visitorReward)
}


#visitorReward <- generate_Maxtix_Visitor_Reward(K,Time,iteration)
#S <- UCB_qui_marche(visitorReward,K)
#regret <- cumulativeRegret(S$choice,visitorReward)
#plot(regret,type='l',xlab = "item")
#summary(visitorReward)






