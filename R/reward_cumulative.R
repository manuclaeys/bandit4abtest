#'Return list of cumulative reward
#'
#'Return a list with cumulative rewards at each iterations
#'
#'@param choice  Integer list
#'@param visitor_reward dataframe of integer or numeric values
#'
#'@return List of numeric values
#'
#'@examples
#'##### Pairewise #####
#'set.seed(1234)
#'size.tot <- 10000
#'x <- seq(0, 5, 0.01)
#'x1<- sample(x, size.tot, replace = TRUE, prob = NULL)
#'arm_1 <-  as.vector(c(2,-1,1.5,0))
#'K1 <- (x1 < 1 ) * arm_1[4]  +
#'  (x1 >= 1 & x1 < 2 ) * arm_1[1]  +
#'  (x1 >= 2 & x1 < 3) * arm_1[2]  +
#'  (x1 >= 3 & x1 < 4) * arm_1[3]  +
#'  (x1 >= 4) * arm_1[4]
#'plot(x1, K1)
#'
#'arm_2 <-  as.vector(c(1.5,-0.5,1.25,0))
#'K2 <- (x1 < 1 ) * arm_2[4]  +
#'  (x1 >= 1 & x1 < 2 ) * arm_2[1]  +
#'  (x1 >= 2 & x1 < 3) * arm_2[2]  +
#'  (x1 >= 3 & x1 < 4) * arm_2[3]  +
#'  (x1 >= 4) * arm_2[4]
#'plot(x1, K2)

#'#covariate without interest
#'x2<- sample(x, size.tot, replace = TRUE, prob = NULL)
#'#Results for each variation
#'visitor_reward <-  data.frame(K1,K2 )
#'summary(visitor_reward)
#'dt <- as.data.frame(cbind(x1,x2))
#'#Random choices
#'choice <- sample(c(1,2), size.tot, replace = TRUE)
#'reward <- reward_cumulative(choice=choice,visitor_reward=visitor_reward)
#'plot(1:size.tot,  cumsum(reward))
#'@export
reward_cumulative <- function(choice, visitor_reward){

  reward.evolutive <- c()
 for(i in 1:nrow(visitor_reward)) reward.evolutive[i] <- visitor_reward[i,choice[i]]

  return(cumsum(reward.evolutive))
}



