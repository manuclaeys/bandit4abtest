#'Return list of regret
#'
#'Return a list with obtained regret at each iterations
#'
#'@param choice  Integer list
#'@param visitorReward dataframe of integer or numeric values
#'
#'@return List of numeric values
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rnorm(100, 30, .05)
#'K2 <- rnorm(100, 21, .05)
#'## Define a dataframe of rewards
#'visitorReward <- as.data.frame(cbind(K1,K2) )
#'#Random choices
#'choice <- sample(c(1,2), 100, replace = TRUE)
#'simpleRegret(choice=choice,visitorReward=visitorReward)
#'
#'@export
simpleRegret <- function(choice,visitorReward){
  regret <- c()
  for(i in 1:nrow(visitorReward)){
    regret[i] <-   regretValue(as.integer(choice[i]),visitorReward[i,])
  }
  return(regret)
}
