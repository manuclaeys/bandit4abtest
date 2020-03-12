#'Return list of regret
#'
#'Return a list with obtained regret at each iterations
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
#'
#'#Best policy
#'ctree_models <- c()
#'  for(i in 1:ncol(visitor_reward)){
#'### learning  ###
#'#Generate formula and tree
#'ctree_models[[i]] <-  ctree_formula_generate(dt = dt,
#'                                             visitor_reward = visitor_reward,
#'                                             ctree_control_val = ctree_control(teststat = c("quadratic")),
#'                                             arm_for_learn = colnames(visitor_reward[i]),
#'                                             explanatory_variable= c("x1","x2"),
#'                                             learn_size = nrow(dt))
#' }
#'regret <- averageRegret(choice=choice,visitor_reward=visitor_reward,dt,ctree_models)
#'plot(1:size.tot,  cumsum(regret))
#'@export
averageRegret <- function(choice, visitor_reward,dt,ctree_models,isRewardAreBoolean=FALSE) {



  #To keep regret
  regret <- c()

  res_j <- c()
  for(j in 1:ncol(visitor_reward)) res_j [[j]] <-  predict(ctree_models[[j]],dt)

  if(isRewardAreBoolean==TRUE){
    for(j in 1:ncol(visitor_reward)) res_j [[j]] <- predict(ctree_models[[j]],dt, type="prob")
  }

  res_j <- as.data.frame(res_j)
  res_j$max <- 0


  for(i in 1:nrow(visitor_reward))  res_j$max[i] <- max(res_j[i,1:ncol(visitor_reward)])

 # for (i in 1:nrow(visitor_reward)) {
  #     regret[i] <- res_j$max[i] - visitor_reward[i, as.integer(choice[i])]
  #   if(regret[i] < 0)  regret[i] = 0
  #}

   for (i in 1:nrow(visitor_reward)) {
       regret[i] <- res_j$max[i] -  res_j[i,as.integer(choice[i])]
     if(regret[i] < 0)  regret[i] = 0
  }

  return (regret)
}
