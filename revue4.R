#Modz sans shuffle
library(bandit4abtest)
library(partykit)

remove(list = ls())
set.seed(1234)


setwd("~/Documents/bandit_time_serie_git/cluster/dorcel")


#Importe data
library(jsonlite)
library(readr)

data.train  <- jsonlite::fromJSON("parcoursuserDatabaseFinalModz1", simplifyDataFrame = TRUE)
#visitorReward <- jsonlite::fromJSON("parcours_reward_modz.JSON", simplifyDataFrame = TRUE)
visitorReward <- read.csv2("~/Documents/Experimentation/dbactreeucb/ModzLeaveAtLeastOne.csv", header = TRUE, sep = ',',colClasses=c("fullVisitorId"="character"))
visitorReward$X = NULL


total <- merge(data.train ,visitorReward, by="fullVisitorId")


#global summary of data
summary(as.factor(total$A))
summary(as.factor(total$B))

#which covariate will be observe?
#here covariates are time series
listInteger   = c( "presence_time_serie" ,  "time_spend_time_serie" ,  "connexion_time_time_serie" )

#don't use time series with lenght smaller than 2
total <-total[total$size_time_serie>1 , ]
#how many transaction do we have now?
summary(as.factor(total$transactions))

#define time series as numerical time series
total$presence_time_serie <- lapply(total$presence_time_serie, as.numeric)
total$connexion_time_time_serie <- lapply(total$connexion_time_time_serie, as.numeric)
total$time_spend_time_serie <- lapply(total$time_spend_time_serie, as.numeric)

#remplace NA by 0 (encoding problem for 2 time series)
for(i in 1:nrow(total)) total$time_spend_time_serie[i] <- lapply(total$time_spend_time_serie[i] ,function(x) replace(x,is.na(x),0))



rm(list=ls()[! ls() %in% c("total")])

list_results_dba_ctree_ucb  = c()
list_results_dba_lin_ucb = c()
list_results_ctree_ucb = c()
list_results_lin_ucb = c()
list_results_random = c()

library(partykit)
total_old = total
learnset = total
testset = total
i=1

  learnset = learnset[sample(nrow(learnset)),]
  testset = testset[sample(nrow(testset)),]
  total = rbind(learnset,testset)
  config = 0.5

  #Config 70 - 30
  config = 0.3


  visitor_reward = total[,c("A","B")]
  dt = total[,c("presence_time_serie","time_spend_time_serie","connexion_time_time_serie")]

  K=ncol(visitor_reward)

  alpha = 1

  #Parametrage
  ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward,
                                                               is_reward_are_boolean = TRUE,
                                                               alpha = alpha,
                                                               arm_for_learn = names(visitor_reward)[1],
                                                               learn_size = as.integer(nrow(dt) * config))
  listSerie = c("presence_time_serie","time_spend_time_serie","connexion_time_time_serie")
  listKCentroids=c(5,10,5)


  resVal_dbactreeucb <- dbactreeucbRejectionSamplingBanditObjectEvaluation(dt,visitor_reward,K, listSerie, listKCentroids , ctree_parameters_control)

  #Performence metrics
  taux_reward_dba_ctree_ucb = max(resVal_dbactreeucb$cum_rew_dbactreeucb_rejection_sampling_alloc)/length(resVal_dbactreeucb$cum_rew_dbactreeucb_rejection_sampling_alloc)

  list_results_dba_ctree_ucb  = append(list_results_dba_ctree_ucb,taux_reward_dba_ctree_ucb)

  ###### Comparatif avec DBALinUCB

  IsRewardAreBoolean = TRUE
  listCategorial=0
  listInteger=0
  resVal_dbalinucb  <- dbalinucbRejectionSamplingBanditObjectEvaluation(dt=dt,
                                                                        visitor_reward=visitor_reward,
                                                                        alpha=alpha, K=ncol(visitor_reward),
                                                                        listSerie, listKCentroids ,
                                                                        learn_size = as.integer(nrow(dt) * config),
                                                                        IsRewardAreBoolean = IsRewardAreBoolean,
                                                                        listCategorial=listCategorial , listInteger=listInteger)


  #Performence metrics
  taux_reward_dba_lin_ucb = max(resVal_dbalinucb$cum_rew_dbalinucb_rejection_sampling_alloc)/length(resVal_dbalinucb$cum_rew_dbalinucb_rejection_sampling_alloc)
  list_results_dba_lin_ucb  = append(list_results_dba_lin_ucb,taux_reward_dba_lin_ucb)



  # Comparaison without time series clustering
  for(k in listSerie ){
    dt[[paste("mean_",k,sep = "")]] <- sapply(dt[,k],mean)
    dt[,k]= NULL
  }


  #ctreeucb
  ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward,
                                                               explanatory_variable = colnames(dt),
                                                               is_reward_are_boolean = TRUE,
                                                               alpha = 1,learn_size = as.integer(nrow(dt) * config))

  resVal_ctreeucb <- ctreeucbRejectionSamplingBanditObjectEvaluation(dt,visitor_reward,K, ctree_parameters_control)

  #Performence metrics
  taux_reward_ctree_ucb = max(resVal_ctreeucb$cum_rew_ctreeucb_rejection_sampling_alloc)/length(resVal_ctreeucb$cum_rew_ctreeucb_rejection_sampling_alloc)
  list_results_ctree_ucb  = append( list_results_ctree_ucb,taux_reward_ctree_ucb)


  #udate data
  dt2 = dt[resVal_ctreeucb$ctreeucb_rejection_sampling_bandit_alloc$first_train_element:nrow(dt),]
  visitor_reward2 = visitor_reward[resVal_ctreeucb$ctreeucb_rejection_sampling_bandit_alloc$first_train_element:nrow(dt),]
  #LinUCB
  resVal_linucb <- LinucbRejectionSamplingBanditObjectEvaluation(
    dt2,
    visitor_reward = visitor_reward2,
    K = ncol(visitor_reward),
    alpha = 1,
    IsRewardAreBoolean = FALSE
  )



  #Performence metrics
  taux_reward_lin_ucb = max(resVal_linucb$cum_rew_linucb_rejection_sampling_alloc)/length(resVal_linucb$cum_rew_linucb_rejection_sampling_alloc)
  list_results_lin_ucb   = append( list_results_lin_ucb ,taux_reward_lin_ucb)



  #Random
  resVal_random <- uniform_bandit_object_evaluationRejectionSampling(visitor_reward = visitor_reward2, average = FALSE  )


  #Performence metrics
  taux_reward_random  = max(  resVal_random$cum_rew_uniform_bandit_allocRejectionSampling)/length(resVal_random$cum_rew_uniform_bandit_allocRejectionSampling)
  list_results_random    = append( list_results_random  ,taux_reward_random )





mean(list_results_dba_ctree_ucb  )
mean(list_results_dba_lin_ucb)

mean(list_results_ctree_ucb)
mean(list_results_lin_ucb )
mean(list_results_random )

