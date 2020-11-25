#'ctreeucb_rejection_sampling
#'
#' ctreeucb_rejection_sampling automatically create homogeneous groups by a conditional inference method (see  \code{\link{ctree}}) in a collection and processing step before the A/B test (step 1).
#' These groups are created according to the objective of the test using information from previous items (obtained rewards, items characteristics, temporal information, \ldots).
#' This information comes from the items that have already been subjected to the original variation (A),
#' implemented before the test. In the A/B test period (step 2), the method defines as many non-contextual bandits  (see  \code{\link{UCB}}) with rejection sampling method as there are groups.
#' Exclud any choices which not corresponds to real exepriments in dataset.
#' Each bandit aims to find the optimal variation associated to its group.
#' So, a new item is firstly classed into a group and then the associated bandit chooses the variation to which the item must be affected.
#'
#'@param dt  Dataframe of integer numeric or factor values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param is_reward_are_boolean logical value (optional)
#'@param learn_size number of items dedicated to the learnset (step 1) (optional),
#'@param arm_for_learn arm dedicated to the learnset (step 1) (optional),
#'@param explanatory_variable = list of covariates (optional),
#'@param ctree_control_val	= Various parameters that control aspects of the ‘ctree’ fit (optional),
#'
#'@return
#' \itemize{ List of element:
#'  \item choice: choices of UCB,
#'  \item proba: probability of the chosen arms,
#'  \item time: time of cumputation,
#'  \item theta_hat: coefficients estimated of each arm
#'  \item theta: real coefficients of each arm
#'  }
#
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
#'visitor_reward <-  data.frame(K1,K2)
#'dt <- as.data.frame(dt)
#'size.tot = 1000
#'#remove data
#'temp_list <- sample(1:nrow(visitor_reward), as.integer(size.tot/2) , replace = FALSE, prob = NULL)
#'visitor_reward$K1[temp_list] <- NA
#'visitor_reward$K2[-temp_list] <- NA
#'#run ucb on missing data
#'ctreeucb_rejection_sampling(dt,visitor_reward)
#'@import tictoc
#'
#'@export
#ctreeucb_rejection_sampling agorithm
ctreeucb_rejection_sampling <- function(dt,visitor_reward,K=ncol(visitor_reward), ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward)){

  # set.seed(4321)

  ### control ###

  #data controle
  DataControlK(visitor_reward, K = K)
  #DataControlContextReward(dt, visitor_reward)

  #Change the type of data
  temp <-changeDataTypeForCtreeUCB(dt=dt,visitor_reward=visitor_reward,is_reward_are_boolean=ctree_parameters_control$is_reward_are_boolean)
  dt <- temp$dt

  #if reward is boolean, data will be modify temporary
  temp.visitor_reward <- temp$visitor_reward

  ### learning  ###
  #Generate formula and tree
  ctree_tree <- ctree_formula_generate(dt = dt,
                                       visitor_reward = temp.visitor_reward,
                                       ctree_control_val = ctree_parameters_control$ctree_control_val,
                                       arm_for_learn = ctree_parameters_control$arm_for_learn,
                                       explanatory_variable= ctree_parameters_control$explanatory_variable,
                                       learn_size = ctree_parameters_control$learn_size,
                                       print=TRUE)
  #return to regular data
  visitor_reward <- visitor_reward

  #remove the learn set
  #update handle one covariate

  if(length(ctree_parameters_control$explanatory_variable)==1){
    dt <- as.data.frame(dt[c((ctree_parameters_control$learn_size+1):nrow(dt)),])
    colnames(dt) <- c("x")
  }else{

    dt <- dt[c((ctree_parameters_control$learn_size+1):nrow(dt)),]
  }

  visitor_reward <- visitor_reward[c((ctree_parameters_control$learn_size+1):nrow(visitor_reward)),]


  ### Training  ###

  #define cluster for each item
  dt$groups <- predict(ctree_tree, newdata=dt, type="node")
  dt$choice <- 0
  dt$regret <- NA
  dt <- cbind(dt,visitor_reward)
  groups <- dt$groups

  #object for stores choices
  choices <- c( rep(0,nrow(visitor_reward)))



  Ctree_object <- list()
  #start ucb for set upper than learn set
  #get the number of arm
  K= ncol(visitor_reward)

  tic()

  #for each groups play a private strategy of ucb
  for(i in levels(as.factor(dt$groups ))){

    message(paste("subset",i,sep=" "))
    #Subset visitors from this segment
    visitor_reward_for_ctree <- subset.data.frame(dt,dt$groups== i)
    visitor_reward_for_ctree <-  visitor_reward_for_ctree [,(ncol(visitor_reward_for_ctree) -K+1):ncol(visitor_reward_for_ctree )]

    #UCB results
    ucb_temp_res <- UCB_rejection_sampling(visitorReward=visitor_reward_for_ctree , K=K ,alpha =ctree_parameters_control$alpha)

    #update choice vector
    dt[dt$groups==i,]$choice  <-ucb_temp_res$choice

    #Save results
    Ctree_object <- c(Ctree_object,list(i,ucb_temp_res))
    rm(ucb_temp_res)



  }


  time <- toc()

  #return  data , models, groups and results
  return(list('data_reward'=visitor_reward,'data_context'=dt,'groups'=groups,'ctree_ucb'=Ctree_object,'first_train_element'=(ctree_parameters_control$learn_size+1) ,'time'=(time$toc - time$tic),'choice'=dt$choice,'tree'= ctree_tree))

}
