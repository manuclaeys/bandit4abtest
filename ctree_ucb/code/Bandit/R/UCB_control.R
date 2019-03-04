UCB_control_K <- function( visitorReward, K){
  #arm must be superior to 2
  try(if(K<2) stop("arm must be superior or equal to 2"))

  #each arm get a result
  try(if(ncol(visitorReward) != K) stop("each arm need a result"))

  return(TRUE)
}

UCB_control_data <- function(visitorReward, K ){

  #no missing data
  try(if(sum(colSums(is.na(visitorReward))) > 0) stop("missing data in arm results database"))
}


UCB_control <- function(visitorReward, K ){
  UCB_control_K(visitorReward = visitorReward, K= K)
  UCB_control_data(visitorReward = visitorReward, K=K)
}
