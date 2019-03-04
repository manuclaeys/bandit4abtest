#return real theta from a rigide regression
return_real_theta <- function(dt,visitorReward){
  
  K <- ncol(visitorReward)
  n_f <- ncol(dt) 
  #creat th object
  th = array(0, c(K,n_f))
  colnames(th) <- colnames(dt)
  rownames(th) <- colnames(visitorReward)
  
  #tempory variable
  temp <- dt
  for(i in 1:K){
    temp$prediction <- visitorReward[,i]
    
    logitMod <- glm( prediction~., family=binomial, data=temp) # build linear regression model on full data
    #intercept is not save
    th[i,] <- logitMod$coefficients[-1]
    
    temp$prediction <- NULL
  }
  
  return('th'=th)
  
}