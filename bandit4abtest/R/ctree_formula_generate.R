#'ctree_formula_generate
#'
#'Generate a conditional inference tree with \code{\link{ctree}} function.
#'Apply a aecursive partitioning for continuous, censored, ordered, nominal and multivariate response variables in a conditional inference framework.
#'
#'@param dt  Dataframe of integer numeric or factor values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param is_reward_are_boolean logical value (optional)
#'@param learn_size number of items dedicated to the learnset (step 1) (optional),
#'@param arm_for_learn arm dedicated to the learnset (step 1) (optional),
#'@param explanatory_variable = list of covariates (optional),
#'@param ctree_control_val	= Various parameters that control aspects of the ‘ctree’ fit (optional),
#'
#'@return An object of class \code{\link{party}}
#'
#'@examples
## Generates 1000 numbers from 2 uniform distributions
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
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = crossprod(t(dt),arm_2)
#'arm_3 <-  as.vector(c(-1,-5,1,10))
#'K3 = crossprod(t(dt),arm_3)
#'visitor_reward <-  data.frame(K1,K2,K3)
#'dt <- as.data.frame(dt)
#'ctreeucb_control_val=ctreeucb_parameters_control_default(dt=dt,  visitor_reward= visitor_reward)
#'ctree_formula_generate(dt, visitor_reward,arm_for_learn='K1',explanatory_variable=c('x1','x2','x3','x4'),learn_size=100,ctree_control_val=ctreeucb_control_val$ctree_control_val)
#'ctree_formula_generate(dt, visitor_reward,arm_for_learn='K2',explanatory_variable=c('x1','x2','x3','x4'),learn_size=100,ctree_control_val=ctreeucb_control_val$ctree_control_val)
#'ctree_formula_generate(dt, visitor_reward,arm_for_learn='K3',explanatory_variable=c('x1','x2','x3','x4'),learn_size=100,ctree_control_val=ctreeucb_control_val$ctree_control_val)
#'@import partykit
#'@export
ctree_formula_generate <- function(dt, visitor_reward,arm_for_learn,explanatory_variable,learn_size,ctree_control_val){

 # set.seed(1234)
  dt <- dt[1:learn_size,]
   visitor_reward <-  visitor_reward[1:learn_size,]

  #missing values
  #dt <- dt[!is.na( visitor_reward[,arm_for_learn]),]
  #visitor_reward <-  visitor_reward[!is.na( visitor_reward[,arm_for_learn]),]

  #extract element of the list
  elt <- c(explanatory_variable[1])
  if(length(explanatory_variable)>1){
    for(i in 2:length(explanatory_variable)) elt<- paste(elt, explanatory_variable[i], sep = " + ")
  }


  #update corrct bug where we had only one covariate
  dt <- as.data.frame(dt)
  if(length(explanatory_variable)==1){
    colnames(dt) <- c("x")
    elt <- c("x")
  }

  #add one variation restult to the data
  dt$arm_for_learn <-  visitor_reward[,arm_for_learn]

  Formula <-  paste("arm_for_learn "," ~ " , elt, sep = "")

  Formula <- as.formula(Formula)

  reg_tree <- ctree(formula = Formula , dt,control = ctree_control_val, na.action = na.exclude)

  plot(reg_tree)

  return(reg_tree)
}
