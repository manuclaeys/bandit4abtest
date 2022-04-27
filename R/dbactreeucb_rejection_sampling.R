#'dbactreeucb_rejection_sampling
#'
#' dbactreeucb_rejection_sampling automatically create homogeneous groups by a conditional inference method (see  \code{\link{ctree}}) in a collection and processing step before the A/B test (step 1).
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
#'@param ctree_control_val Various parameters that control aspects of the ‘ctree’ fit (optional),
#'@param listKCentroids List of Integer (Number of centroid)
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
#'size.tot = 9000
#'set.seed(4649)                          # this makes the example exactly reproducible
#'x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
#'x2 = runif(size.tot, min=0, max=10)
#'x3 = runif(size.tot, min=0, max=10)
#'x4 = runif(size.tot, min=0, max=10)
#'dt = cbind(x1,x2,x3,x4)
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = crossprod(t(dt),arm_1)
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = crossprod(t(dt),arm_2)
#'arm_3 <-  as.vector(c(-1,-5,1,10))
#'K3 = crossprod(t(dt),arm_3)
#'visitor_reward <-  data.frame(K1,K2,K3)
#'dt <- as.data.frame(dt)
#'size.tot = 9000
#'# Time series
#'alpha_list <- c(1,2,3)
#'beta_list <- c(0.5,0.1,-0.2)
#'theta_list <- c(0.8,0.2,0.5)
#'y <- as.data.frame(c(1))
#'colnames(y) = "ID"
#'temp=1
#'for (j in 1:3000){
#'  for (i in 1:length(alpha_list)){
#'    n = sample(1:100,1)
#'    t <- 1:n
#'    ts <- alpha_list[i] + beta_list[i] * t + arima.sim(list(ma = theta_list[i]), n = length(t))
#'    y[temp, "time_series"][[1]] <- list(ts)
#'    y[temp, "cluster"][[1]] <- i
#'    y$ID[temp] = temp
#'    temp = temp +1
#'  }
#'}
#'y <- y[sample(nrow(y)),]



#'dt <-  as.data.frame(cbind(x1,x2,x3,x4,y$time_series))
#'colnames(dt) <- c("x1","x2","x3","x4","time_series")

#'for(i in 1:nrow(dt)) {
#'  if(y$cluster[i] == 1) visitor_reward$K1[i] = 10
#'  if(y$cluster[i] == 2) visitor_reward$K2[i] = 20
#'  if(y$cluster[i] == 3) visitor_reward$K3[i] = 30
#'}

#'dt$cluster <- NULL
#'dt$x1 <- as.numeric(dt$x1)
#'dt$x2 <- as.numeric(dt$x2)
#'dt$x3 <- as.numeric(dt$x3)
#'dt$x4 <- as.numeric(dt$x4)
#'K=ncol(visitor_reward)
#'ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward)
#'listSerie = c("time_series")
#'listKCentroids=c(3)
#'dbactreeucb_rejection_sampling(dt,visitor_reward,K, listSerie, listKCentroids , ctree_parameters_control)
#'@import tictoc
#'
#'@export
#dbactreeucb_rejection_sampling agorithm
dbactreeucb_rejection_sampling <- function(dt,visitor_reward,K=ncol(visitor_reward), listSerie, listKCentroids , ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward)){

  # set.seed(4321)

  ### control ###

  #data controle
  DataControlK(visitor_reward, K = K)
  library(dplyr)


  if(ctree_parameters_control$is_reward_are_boolean){
    for(i in 1:K)  visitor_reward[,i] =  as.factor(visitor_reward[,i])
  }




  ### learning  ###
  #Learn Clustering
  clust_data = dt[1:ctree_parameters_control$learn_size & (is.na(visitor_reward[1:ctree_parameters_control$learn_size, ctree_parameters_control$arm_for_learn]))==FALSE ,  ]
  obj <- createClusters(listSerie = listSerie , dt = clust_data  ,
                        method = "DBA" ,
                        listKCentroids=listKCentroids ,
                        plotCentroids = TRUE ,
                        plotClusters = TRUE ,
                        maxIter = 100L )


  #Add clusters
  for(i in 1:length(listSerie)) dt[[paste("cluster",listSerie[i],sep = "")]] <- 0
  for(i in 1:length(listSerie)){

    dt[[paste("cluster",listSerie[i],sep = "")]][1:ctree_parameters_control$learn_size & (is.na(visitor_reward[1:ctree_parameters_control$learn_size, ctree_parameters_control$arm_for_learn]))==FALSE] <- obj$clust_obj[[i]]@cluster


    #Add cluster to explanatory variables
    ctree_parameters_control$explanatory_variable <- c( ctree_parameters_control$explanatory_variable,paste("cluster",listSerie[i],sep = ""))

  }

  for(i in 1:length(listSerie)) dt[[paste("cluster",listSerie[i],sep = "")]] <- as.factor(dt[[paste("cluster",listSerie[i],sep = "")]])

  temp_list =  ctree_parameters_control$explanatory_variable
  #Remove series for ctreeucb learning step (we keep the clusters)
  temp_list =  temp_list[ temp_list !=  listSerie]

  #Generate formula and tree
  ctree_tree <- ctree_formula_generate(dt = dt,
                                       visitor_reward = visitor_reward,
                                      # visitor_reward = temp.visitor_reward,
                                       ctree_control_val = ctree_parameters_control$ctree_control_val,
                                       arm_for_learn = ctree_parameters_control$arm_for_learn,
                                       explanatory_variable= temp_list,
                                       learn_size = ctree_parameters_control$learn_size,
                                       print=TRUE)


  #return to regular data
  visitor_reward <- visitor_reward

  #remove the learn set
  dt.old <- dt
  #update handle one covariate

  if(length(ctree_parameters_control$explanatory_variable)==1){
    dt <- as.data.frame(dt[c((ctree_parameters_control$learn_size+1):nrow(dt)),])
    colnames(dt) <- c("x")
  }else{

    dt <- dt[c((ctree_parameters_control$learn_size+1):nrow(dt)),]
  }

  visitor_reward <- visitor_reward[c((ctree_parameters_control$learn_size+1):nrow(visitor_reward)),]


  ### AB Test ###
  temp_i = 1
  for(i in listSerie){
    if( listKCentroids[temp_i] != length(levels(ctree_tree$data[[paste("cluster",listSerie[temp_i],sep = "")]]))){
      message(paste("A label was exclude (not in the tree) from", listSerie[temp_i], sep = " "))
      message(paste("nb clusters ",listKCentroids[temp_i],  sep = " "))
      message(paste("level  ",  levels(ctree_tree$data[[paste("cluster",listSerie[temp_i],sep = "")]]), sep = " "))
    }
    temp_i = temp_i+1
  }



  #define cluster for each item
  temp_i = 1
  for(i in listSerie){
    print(i)
    list_K_cluster = rep(0,listKCentroids[temp_i])
    for(j in 1: nrow(dt)){

      for(k in 1:listKCentroids[temp_i]){
        list_K_cluster[k]  = dtw2(unlist(dt[[i]][j]), unlist(obj$clust_obj[[temp_i]]@centroids[k]))$distance

        ### exclude level not in the tree ==> predic problem
        if(!k %in% levels(ctree_tree$data[[paste("cluster",listSerie[temp_i],sep = "")]])){
          list_K_cluster[k]  = 1/0

        }
      }
      dt[[paste("cluster",listSerie[temp_i],sep = "")]][j] <- as.factor( which.min(list_K_cluster))
    }
      temp_i = temp_i+1
  }





######



  dt$groups <- predict(ctree_tree, newdata=dt, type="node")
  dt$choice <- 0
  dt$regret <- NA

 if(ctree_parameters_control$is_reward_are_boolean){
   for(i in 1:K)  visitor_reward[,i] =  as.integer(as.character(visitor_reward[,i]))
 }

  dt <- cbind(dt,visitor_reward)
  groups <- dt$groups

  #object for stores choices
  choices <- c( rep(0,nrow(visitor_reward)))



  Ctree_object <- list()
  #start ucb for set upper than learn set
  #get the number of arm
  K= ncol(visitor_reward)

  library(tictoc)
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
