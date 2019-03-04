#return time elaps
library(tictoc)

source("Bandit/R/ctree_parameters_control_default.R")

CTREE <- function(dt,visitorReward,ctree_parameters_control=ctree_parameters_control_default(dt,visitorReward)){

  set.seed(4321)

  #set to file directory
  (WD <- getwd())
  if (!is.null(WD)) setwd(WD)

  ### control ###
  #data controle : see ctree_control
  source("Bandit/R/ctree_control.R")
  control <- ctree_data_control(dt,visitorReward,ctree_parameters_control$is_reward_are_boolean) #data controle : need a context and reward data of same size
    if(control$control == FALSE) return(paste("Error,",control$message,sep=""))


  #Change the type of data
  source("Bandit/R/change_data_type_for_ctree.R")
  temp <- change_data_type_for_ctree(dt,ctree_parameters_control$is_reward_are_boolean,visitorReward)
  dt <- temp$dt

  #if reward is boolean, data will be modify temporary
  temp.visitorReward <- temp$visitorReward

  ### learning  ###
  #Generate formula and tree
  source("Bandit/R/ctree_formula_generate.R")
  ctree_tree <- ctree_formula_generate(dt = dt,visitorReward = temp.visitorReward,
                                       ctree_control_val = ctree_parameters_control$ctree_control_val,
                                       arm_for_learn = ctree_parameters_control$arm_for_learn,explanatory_variable= ctree_parameters_control$explanatory_variable,learn_size = ctree_parameters_control$learn_size)
  #return to regular data
  visitorReward <- visitorReward

  #remove the learn set
  #update handle one covariate

  if(length(ctree_parameters_control$explanatory_variable)==1){
    dt <- as.data.frame(dt[c((ctree_parameters_control$learn_size+1):nrow(dt)),])
    colnames(dt) <- c("x")
  }else{

    dt <- dt[c((ctree_parameters_control$learn_size+1):nrow(dt)),]
  }

  visitorReward <- visitorReward[c((ctree_parameters_control$learn_size+1):nrow(visitorReward)),]


  ### Training  ###

  #define cluster for each item
  dt$groups <- predict(ctree_tree, newdata=dt, type="node")
  dt$choice <- 0
  dt$regret <- NA
  dt <- cbind(dt,visitorReward)
  groups <- dt$groups

  #object for stores choices
  choices <- c( rep(0,nrow(visitorReward)))



  Ctree_object <- list()
  #start ucb for set upper than learn set
    #get the number of arm
    K= ncol(visitorReward)

   # source("R/UCB_qui_marche.R")
    source("Bandit/R/UCB_functions.R")
    source("Bandit/R/regret.R")



    tic()

    #for each groups play a private strategy of ucb
    for(i in levels(as.factor(dt$groups ))){

      message(paste("subset",i,sep=" "))
      #Subset visitors from this segment
      visitorReward_for_ctree <- subset.data.frame(dt,dt$groups== i)
      visitorReward_for_ctree <-  visitorReward_for_ctree [,(ncol(visitorReward_for_ctree) -K+1):ncol(visitorReward_for_ctree )]

      #UCB results
      ucb_temp_res <- UCB(visitorReward=visitorReward_for_ctree , K=K ,alpha =ctree_parameters_control$alpha)



      #update choice vector
      dt[dt$groups==i,]$choice  <-ucb_temp_res$choice
    #  dt[dt$groups==i,]$regret <- SimpleRegret(ucb_temp_res$choice,visitorReward_for_ctree )

      ###past regret on all item database
     # simpleRegret <- SimpleRegret(ucb_temp_res$choice,visitorReward_for_ctree )
     # all_regret_CTREE[all_regret_CTREE$segment==i,]$regret <- simpleRegret



      #Save results
      Ctree_object <- c(Ctree_object,list(i,ucb_temp_res))
      rm(ucb_temp_res)



      }


    time <- toc()

    #return  data , models, groups and results
    return(list('data_reward'=visitorReward,'data_context'=dt,'groups'=groups,'ctree_ucb'=Ctree_object,'first_train_element'=(ctree_parameters_control$learn_size+1) ,'time'=(time$toc - time$tic),'choice'=dt$choice,'tree'= ctree_tree))

  }
