#'data_control_context_reward
#'
#'Control if number of item in data reward and context data are equal
#'Print a message and stop if this condition is not respected.
#'Else return TRUE.
#'
#'@param visitorReward Dataframe of integer or numeric values
#'@param dt  Dataframe of integer numeric or factor values
#'
#'@return Logical value
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rnorm(100, 30, .05)
#'K2 <- rnorm(100, 35, .05)
#'## Define a dataframe of rewards
#'visitorReward <- as.data.frame(K1)
#'## Define a dataframe of context
#'c1 <- rnorm(50, 35, .05)
#'dt <- as.data.frame(c1)
#'## Control
#'data_control_context_reward(dt=dt,visitorReward=visitorReward)
#'c1 <- rnorm(100, 30, .05)
#'dt <- as.data.frame(c1)
#'data_control_context_reward(dt=dt,visitorReward=visitorReward)
#'
#'@export
data_control_context_reward <- function(dt,visitorReward){
#Match size controle
if(nrow(dt)!=nrow(visitorReward) ){
  stop("number of row in contextual data and rewards data are not equals")
  return(FALSE)
}


  return(TRUE)
}
