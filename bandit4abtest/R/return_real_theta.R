#'return_real_theta
#'
#'Return real theta from a rigide regression of context to arm's reward.
#'Return coefficients of regression (except intercept)
#'See also , \code{\link{lm}}.
#'
#'@param dt Dataframe of context
#'@param visitorReward Dataframe of integer or numeric values
#'
#'@return theta_hat: mean estimated of each arm
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
#'visitorReward <-  data.frame(K1,K2,K3)
#'dt <- as.data.frame(dt)
#'return_real_theta(dt,visitorReward)
#'@export
#
return_real_theta <- function(dt,visitorReward,option="linear"){

  K <- ncol(visitorReward)
  n_f <- ncol(dt)
  #creat th object
  th = array(0, c(K,n_f))
  colnames(th) <- colnames(dt)
  rownames(th) <- colnames(visitorReward)

  #tempory variable
  temp <- dt
  if(option=="linear"){
  for(i in 1:K){
    temp$prediction <- visitorReward[,i]

    linearMod <- lm(prediction ~. , data = temp)  # build linear regression model on full data
    #intercept is not save
    th[i,] <- linearMod$coefficients[-1]

    temp$prediction <- NULL
  }
  }

  if(option=="logit"){
    for(i in 1:K){
      temp$prediction <- visitorReward[,i]

      logitMod <- glm(prediction ~. , family=binomial(link='logit'), data = temp)  # build linear regression model on full data
      #intercept is not save
      th[i,] <- logitMod$coefficients[-1]

      temp$prediction <- NULL
    }
  }



  return('th'=th)

}
