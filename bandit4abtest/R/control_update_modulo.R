#'control_update_modulo
#'
#'Check if the model has to be re-compute (number of item equals to update_val have been tested)
#'Return TRUE if we need to re cumpute the model
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
#arm reward
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = crossprod(t(dt),arm_1)
#covariates
#'dt <-as.data.frame(dt)
#prepare data for regression
#'M <- dt
#'M$reward <- K1
#'M$arm <- 0
#'M$arm[1:99] <- 1
#'control_update_modulo(1, M, 100)
#'M$arm[100] <- 1
#'control_update_modulo(1, M, 100)
#'@export
#control_update_modulo
control_update_modulo <- function(choice, M, update_val){

  #subset past data
  A_temp <- subset(M,arm==choice )
  if(nrow(A_temp) %% update_val == 0) return(TRUE)

  return(FALSE)
}
