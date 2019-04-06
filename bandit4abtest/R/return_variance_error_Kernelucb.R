#'return_variance_error_Kernelucb
#'
#'Cumpute variance of a kernelized regression models with \code{\link{krr}} function (library listdtr)
#'based on selected data.
#'@param choice an integer value associated with the selected arm
#'@param expected vector of numeric values expected according to a selected arm
#'@param rewards  vector of numeric values obtained according to a selected arm
#'
#'@return variance of error
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
#'#covariates
#'dt <-as.data.frame(dt)
#'#prepare data for regression
#'M <- dt
#'M$arm <- 1
#'M$reward <- K1
#'#cumpute the model
#'ker_model <- update_model_for_KernelUCB(choice=1 , M=M ,update_val=100)
#'#evaluate the model
#'xnew <- as.data.frame(dt)
#'predictions <- predict(ker_model$model, xnew)
#'return_variance_error_Kernelucb(choice=M$arm ,expected=predictions, rewards=K1)
#'@export
#return_variance_error_Kernelucb
return_variance_error_Kernelucb <- function(choice ,expected, rewards){

  #subset past data
  val_estim <- expected[choice == choice]

  val_real <- rewards[choice == choice]

  #serror estimate
  error = mapply('-',val_real,val_estim)

  return('var'= var(error,na.rm = TRUE))
}

