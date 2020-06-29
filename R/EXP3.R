EXP3 <- function(visitor_reward, K=ncol(visitor_reward), gamma=0.05){

  #control
  BanditRewardControl(visitor_reward = visitor_reward, K = K)
  
  #data formating
  visitor_reward <- as.matrix(visitor_reward)
  
  
  weight <- rep(1, times=K)
  proba <- rep(0, times=K)
  choice <- c()
  reward <- c()
  estimated_reward <- c()
  S <- GenerateMatrixS(K)
  
  tic()
  

  ###initialisation

  for (h in 1:K) {
    
    weight_sum <- sum(weight)
    S <- PlayArm(iter=h, arm=h, S=S, visitor_reward)      
    proba[h] <- (1 - gamma) * (weight[h]/weight_sum) + (gamma/K)
    reward[h] <- visitor_reward[h,h]
   # print(proba)
   
    estimated_reward[h] <- reward[h]/proba[h]
    #update weight
    weight[h] <- weight[h]*exp(gamma*estimated_reward[h]/K)
    
    #print(S)
    choice[1:K] <- c(1:K)
    
  }

  
  for (i in (K+1):nrow(visitor_reward)) {
  
    for (j in 1:K){
      weight_sum <- sum(weight)
      proba[j] <- (1 - gamma) * (weight[j]/weight_sum) + (gamma/K)
     # print(proba)
    }


  choice[i] <- ConditionForEXP3(S, prob = proba)
  reward[i] <- visitor_reward[i,choice[i]]
  S <- PlayArm(iter=i, arm=choice[i], S, visitor_reward)
  
  estimated_reward[i] <- reward[i]/proba[choice[i]]

  #update weight
  weight[choice[i]] <- weight[choice[i]]*exp(gamma*estimated_reward[i]/K)
  
  }
  
  time <- toc()
  
  #coef estimate
  th_hat=S[1,]
  
  #real coef
  th = colMeans(visitor_reward)
  
  cat("th_hat", fill=2)
  cat(th_hat, fill=TRUE)
  cat("th real", fill=2)
  cat(th, fill=TRUE)
  
  return(list('S'=S,'proba'=proba, 'time'=(time$toc - time$tic),'choice'= choice,'theta_hat'=th_hat,'theta'=th, 'weight'=weight))
  
}


