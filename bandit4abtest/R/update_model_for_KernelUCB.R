#'update_model_for_KernelUCB
#'
#'Cumpute a kernelized regression models with \code{\link{krr}} function (library listdtr)
#'on selected data.
#'This function require a lot of time so an uptate value can be use for limit the number of used data.
#'@param M Dataframe of integer or numeric values
#'@param choice an integer value associated with the selected arm
#'@param update_value first item used for cumpute the regression;
#'
#'@return a new model
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
#'ynew <- predict(ker_model$model, xnew)
#'ytrue <- K1
#'mean((ynew - K1) ^ 2)  # MSE
#'plot(density((ynew - K1)))
#'@export
# update_model_for_KernelUCB
update_model_for_KernelUCB  <- function(choice , M ,update_val){

  #subset past data
  A_temp <- subset(M,arm==choice )

  #remove arm number for regression
  A_temp$arm <- NULL
  A_temp <- A_temp[(nrow(A_temp)-update_val+1):nrow(A_temp),]

  #construct the model
  model <- krr(A_temp[,-ncol(M)], A_temp$reward)

  return(list('model'=model))
}
