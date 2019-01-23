
change_data_type_for_ctree <- function(dt,is_reward_are_boolean,visitorReward){

  #change type of reward data if it's consider as boolean values
  if(is_reward_are_boolean==TRUE){
    for(i in 1:ncol(visitorReward)) visitorReward[,i] <- as.factor(visitorReward[,i] )
  }

  for(i in 1:ncol(dt)){
    if(((typeof(dt[,i])=="character") | (typeof(dt[,i])=="logical"))==TRUE) dt[,i] <- as.factor(dt[,i])
  }

  return(list(dt = dt,visitorReward=visitorReward ))
}



