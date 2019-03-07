#'change_data_type_for_ctree
#'
#'Check if a reward is defined as logical value: change reward type as factor.
#'Check if a a colonm of covariates is caractere or logical: change type as factor.
#'
#'@param dt  Dataframe of integer numeric or factor values
#'@param is_reward_are_boolean logical value (optional)
#'@param visitorReward Dataframe of integer or numeric values
#'
#'@return List with the updated dataframe dt and the updated visitorReward
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'c1 <- rep("Test",100)
#'dt <- as.data.frame(c1)
#' typeof(change_data_type_for_ctree(dt=dt,visitorReward=dt)$visitorReward[,1])
#'K1 <- sample(c(0,1),replace=TRUE,size= 100)
#'visitorReward <- as.data.frame(K1)
#'typeof(change_data_type_for_ctree(dt=dt,visitorReward=dt,is_reward_are_boolean=FALSE)$visitorReward[,1])
#'@export
change_data_type_for_ctree <- function(dt,is_reward_are_boolean=FALSE,visitorReward){

  #change type of reward data if it's consider as boolean values
  if(is_reward_are_boolean==TRUE){
    for(i in 1:ncol(visitorReward)) visitorReward[,i] <- as.factor(visitorReward[,i] )
  }

  for(i in 1:ncol(dt)){
    if(((typeof(dt[,i])=="character") | (typeof(dt[,i])=="logical"))==TRUE) dt[,i] <- as.factor(dt[,i])
  }

  return(list(dt = dt,visitorReward=visitorReward ))
}
