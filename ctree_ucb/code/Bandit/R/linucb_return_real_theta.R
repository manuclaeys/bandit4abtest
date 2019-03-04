#  Return the real theta from a rigide regression
#
# Args:
#   dt: contextual data
#   visitorReward: reward data
#
# Return:
#   Return the real theta from a rigide regression
#
 
ReturnRealTheta <- function(dt, visitorReward) {
  K <- ncol(visitorReward)
  nF <- ncol(dt)
  #  create th object
  th = array(0, c(K, nF))
  colnames(th) <- colnames(dt)
  rownames(th) <- colnames(visitorReward)
  #  tempory variable
  temp <- dt
  for (i in 1:K) {
    temp$prediction <- visitorReward[, i]
    linearMod <- lm(prediction ~., data = temp)  #  build linear regression model on full data
    #  intercept is not save
    th[i, ] <- linearMod$coefficients[-1]
    temp$prediction <- NULL
  }
  return('th' = th)
}