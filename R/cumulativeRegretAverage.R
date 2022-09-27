#'Return list of cumulative regret based on average
#'
#'Plot the cumulative regret overt the time. Diff bewteen arm with maximal average and chosen arms
#'Return a list with cumulative regret at each iterations
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
#'cumulativeRegretAverage(choice, visitor_reward)
#'#cumulativeRegretAverage(choice, visitor_reward,dt=dt,explanatory_variable=c("x1","x2"))

#'@import graphics
#'@export
cumulativeRegretAverage <- function(choice, visitor_reward,dt=0,IsRewardAreBoolean=FALSE,explanatory_variable=colnames(dt),message_tree = FALSE) {

  ####Non contextual policy
  if(length(dt)==1){
    max_average_regret = max(colMeans(visitor_reward))
    regret <- c()
    for (i in 1:nrow(visitor_reward)) {
      regret[i] <- max_average_regret - mean(visitor_reward[1:i, as.integer(choice[i])])
      if(regret[i] < 0)  regret[i] = 0
    }
  }

  ###Contextual policy
  if(length(dt)>1){
    ####Model construction
   library(partykit)

  #Change the type of data
  temp <-changeDataTypeForCtreeUCB(dt=dt,visitor_reward=visitor_reward,is_reward_are_boolean=IsRewardAreBoolean)
  dt <- temp$dt

  #if reward is boolean, data will be modify temporary
  temp.visitor_reward <- temp$visitor_reward

  ctree_models <- c()

  for(i in 1:ncol(visitor_reward)){
  ### learning  ###
  #Generate formula and tree

    if( message_tree == TRUE){
      ctree_models[[i]] <-  ctree_formula_generate(dt = dt,
                                                   visitor_reward = temp.visitor_reward,
                                                   ctree_control_val = ctree_control(teststat = c("quadratic")),
                                                   arm_for_learn = colnames(visitor_reward[i]),
                                                   explanatory_variable= explanatory_variable,
                                                   learn_size = nrow(dt),print = TRUE)

    }else{

      ctree_models[[i]] <-  ctree_formula_generate(dt = dt,
                                                   visitor_reward = temp.visitor_reward,
                                                   ctree_control_val = ctree_control(teststat = c("quadratic"), alpha = 0.1),
                                                   arm_for_learn = colnames(visitor_reward[i]),
                                                   explanatory_variable= explanatory_variable,
                                                   learn_size = nrow(dt),print = FALSE)



    }
  }


  regret <- averageRegret(choice= as.vector(choice), visitor_reward,dt,ctree_models,isRewardAreBoolean=IsRewardAreBoolean)
  }

  plot(cumsum(regret), type='l', xlab="Time T", ylab="Cumulative regret")
  return (cumsum(regret))
}
