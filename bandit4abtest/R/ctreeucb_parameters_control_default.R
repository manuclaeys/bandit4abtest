#'ctreeucb_parameters_control_default
#'
#'Various parameters that control aspects of the ctreeUCB fit. See also \code{\link{ctree_control}} and \code{\link{ctree}}
#'
#'@param dt  Dataframe of integer numeric or factor values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param is_reward_are_boolean logical value (optional) (optional),
#'@param learn_size number of items dedicated to the learnset (step 1) (optional),
#'@param arm_for_learn arm dedicated to the learnset (step 1) (optional),
#'@param explanatory_variable =  list of covariates (optional),
#'@param alpha Numeric value (optional)
#'@param ctree_control_val Various parameters that control aspects of the ‘ctree’ fit (optional),
#'
#'@return list of parameters
#'
#'@examples
#'c1 <- rep("Test",100)
#'dt <- as.data.frame(c1)
#'K1 <- sample(c(0,1),replace=TRUE,size= 100)
#'visitor_reward <- as.data.frame(K1)
#'ctreeucb_parameters_control_default(dt=dt, visitor_reward=visitor_reward)
#'ctreeucb_parameters_control_default(dt=dt, visitor_reward=visitor_reward,is_reward_are_boolean=TRUE,learn_size=200, explanatory_variable="c1", alpha=1, ctree_control_val= ctree_control(teststat = "quadratic"))
#'@export
ctreeucb_parameters_control_default <- function(
  dt,
  visitor_reward,
  is_reward_are_boolean = FALSE,
  learn_size = as.integer(nrow(dt)*0.10),
  arm_for_learn = names(visitor_reward)[1],
  explanatory_variable =  names(dt),
  alpha=1,
  ctree_control_val=ctree_control()
){

  return(list(learn_size = learn_size ,is_reward_are_boolean = is_reward_are_boolean , arm_for_learn= arm_for_learn,
              explanatory_variable =explanatory_variable,
              alpha=alpha,
              ctree_control_val=ctree_control_val
  ))
}
