#library(bandit4abtest)

remove(list = ls())
setwd("~/Documents/bandit_time_serie_git/cluster/dorcel")

#Importe data
library(jsonlite)

data.train  <- jsonlite::fromJSON("parcoursuserDatabaseFinalDorcel.JSON", simplifyDataFrame = TRUE)
visitorReward <- jsonlite::fromJSON("parcoursrewardFinalDorcel.JSON", simplifyDataFrame = TRUE)


#global summary of data
summary(as.factor(visitorReward$A))
summary(as.factor(visitorReward$B))

#which covariate will be observe?
#here covariates are time series
listInteger   = c( "presence_time_serie" ,  "time_spend_time_serie" ,  "connexion_time_time_serie" )

#don't use time series with lenght smaller than 2
data.train <-data.train[data.train$size_time_serie>1 , ]
#how many transaction do we have now?
summary(as.factor(data.train$transactions))

#define time series as numerical time series
data.train$presence_time_serie <- lapply(data.train$presence_time_serie, as.numeric)
data.train$connexion_time_time_serie <- lapply(data.train$connexion_time_time_serie, as.numeric)
data.train$time_spend_time_serie <- lapply(data.train$time_spend_time_serie, as.numeric)

#remplace NA by 0 (encoding problem for 2 time series)
for(i in 1:nrow(data.train)) data.train$time_spend_time_serie[i] <- lapply(data.train$time_spend_time_serie[i] ,function(x) replace(x,is.na(x),0))

total <- merge(data.train ,visitorReward, by="fullVisitorId")

rm(list=ls()[! ls() %in% c("total")])

visitor_reward = total[,c("A","B")]
dt = total[,c("presence_time_serie","time_spend_time_serie","connexion_time_time_serie")]

K=ncol(visitor_reward)

#Parametrage
ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward, alpha = 1,learn_size = as.integer(nrow(dt) * 0.3))
listSerie = c("presence_time_serie","time_spend_time_serie","connexion_time_time_serie")
listKCentroids=c(5,4,15)
resVal <- dbactreeucbRejectionSamplingBanditObjectEvaluation(dt,visitor_reward,K, listSerie, listKCentroids , ctree_parameters_control)




