#Install from github
#install.packages("devtools")
library(devtools)
#install_github("https://github.com/R-workshop-strasbourg/bandit4abtest/tree/master/bandit4abtest")
install_github("https://github.com/manuclaeys/bandit4abtest")

##Poster demo

library(bandit4abtest)

set.seed(4434)
V1 <- rbinom(5000, 1, 0.6)  # Generates 5000 numbers from a uniform distribution with mean 0.75
V2 <- rbinom(5000, 1, 0.7)
V3 <- rbinom(5000, 1, 0.5)
V4 <- rbinom(5000, 1, 0.3)
V5 <- rbinom(5000, 1, 0.9)

visitorReward <- as.data.frame( cbind(V1,V2,V3,V4,V5) )

############ Algorithm UCB ############

ucb_alloc  <- UCB(visitorReward,alpha = 1)
cum_reg_ucb_alloc  <- cumulativeRegretAverage(ucb_alloc$choice,visitorReward)


############ Epsilon greedy ######################

epsilonGreedy_alloc <- EpsilonGreedy(visitorReward,epsilon  = 0.05)
cum_reg_epsilonGreedy_alloc  <- cumulativeRegretAverage(epsilonGreedy_alloc$choice,visitorReward)



############ Thompson Sampling ############

thompson_sampling_alloc <- ThompsonSampling(visitorReward)
cum_reg_thompson_sampling_alloc <- cumulativeRegretAverage(thompson_sampling_alloc$choice,visitorReward)


############ Random ############

random_alloc <- UniformBandit(visitorReward)
cum_reg_random_alloc <- cumulativeRegretAverage(random_alloc$choice,visitorReward)



###PLOT WITH GGPLOT2 REGRET###

library(ggplot2)

comp_reg <- data.frame(cbind(cum_reg_ucb_alloc,
                             cum_reg_epsilonGreedy_alloc,
                             cum_reg_thompson_sampling_alloc,
                             cum_reg_random_alloc ))



ggplot(comp_reg, aes(c(1:nrow(comp_reg)), y = value, color = Algorithm)) +
  geom_line(linetype="dashed",aes(y = cum_reg_ucb_alloc, col = "UCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_epsilonGreedy_alloc, col = "Epsilon Greedy"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_thompson_sampling_alloc, col = "Thompson Sampling"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_random_alloc, col = "Random"),size = 0.5) +
  scale_colour_manual(values =  c("UCB"="brown","Epsilon Greedy"="orange","Thompson Sampling"="green","Random"="black"))+
  xlab("Time T") +
  ylab("Cumulative regret")




#################
rm(list = ls())
#Contextual with continus reward
size.tot = 10000
set.seed(4649)                          # this makes the example exactly reproducible
x1 = runif(size.tot, min=0, max=10)          # you have 2, largely uncorrelated predictors
x2 = runif(size.tot, min=0, max=10)

dt = cbind(x1,x2)
#arm reward
arm_1 <-  as.vector(c(0,0.5))
K1 = crossprod(t(dt),arm_1) + runif(size.tot, min=-1, max=1)    #  linear predictor
summary(K1)

arm_2 <-  as.vector(c(0.5,0))
K2 = crossprod(t(dt),arm_2) + runif(size.tot, min=-1, max=1)
summary(K2)

visitor_reward <-  data.frame(K1,K2)
dt <- as.data.frame(dt)


linucb_contextual_alloc <- LINUCB(dt,visitor_reward )
cum_reg_linucb_contextual_alloc <- cumulativeRegretAverage(linucb_contextual_alloc$choice,visitor_reward,dt = dt)

kernelucb_contextual_alloc <- kernelucb(dt, visitor_reward, update_val = 500)
cum_reg_kernelucb_contextual_alloc <- cumulativeRegretAverage(kernelucb_contextual_alloc$choice,visitor_reward,dt=dt)
max(cum_reg_kernelucb_contextual_alloc)

random_alloc <- UniformBandit(visitor_reward)
cum_reg_random_alloc <- cumulativeRegretAverage(random_alloc$choice,visitor_reward,dt=dt)

############ Algorithm UCB ############

ucb_alloc  <- UCB( visitor_reward,alpha = 1)
cum_reg_ucb_alloc  <- cumulativeRegretAverage(ucb_alloc$choice, visitor_reward,dt=dt)


############ Epsilon greedy ######################

epsilonGreedy_alloc <- EpsilonGreedy( visitor_reward,epsilon  = 0.05)
cum_reg_epsilonGreedy_alloc  <- cumulativeRegretAverage(epsilonGreedy_alloc$choice,visitor_reward,dt=dt)



###PLOT WITH GGPLOT2 REGRET###

library(ggplot2)

comp_reg <- data.frame(cbind(cum_reg_linucb_contextual_alloc,
                             cum_reg_kernelucb_contextual_alloc,
                             cum_reg_random_alloc,
                             cum_reg_ucb_alloc,
                             cum_reg_epsilonGreedy_alloc))



ggplot(comp_reg, aes(c(1:nrow(comp_reg)), y = value, color = Algorithm)) +
  geom_line(linetype="dashed",aes(y = cum_reg_ucb_alloc, col = "UCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_epsilonGreedy_alloc, col = "Epsilon Greedy"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_random_alloc, col = "Random"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_linucb_contextual_alloc, col = "LINUCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_kernelucb_contextual_alloc, col = "KernelUCB"),size = 0.5) +
  scale_colour_manual(values =  c("UCB"="brown","Epsilon Greedy"="orange","Thompson Sampling"="green","Random"="black", "Contextual TS"="dark green", "LINUCB"="blue","KernelUCB"= "purple"))+
  xlab("Time T") +
  ylab("Cumulative regret")














#################
rm(list = ls())
#Contextual with Binary reward
size.tot = 10000
set.seed(4649)                          # this makes the example exactly reproducible
x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
x2 = runif(size.tot, min=0, max=10)

dt = cbind(x1,x2)
#arm reward
arm_1 <-  as.vector(c(0.1,-0.3))
K1 = 1/(1+exp(- crossprod(t(dt),arm_1))) # inverse logit transform of linear predictor
summary(K1)
K1 = vapply(K1, function(x) rbinom(1, 1, x), as.integer(1L))
summary(K1)
arm_2 <-  as.vector(c(0.3,-0.4))
K2 = 1/(1+exp(- crossprod(t(dt),arm_2))) # inverse logit transform of linear predictor
summary(K2)
K2 = vapply(K2, function(x) rbinom(1, 1, x), as.integer(1L))
summary(K2)
visitor_reward <-  data.frame(K1,K2)
dt <- as.data.frame(dt)

summary(visitor_reward)

thompson_sampling_contextual_alloc <- TSLINUCB(dt=dt,visitor_reward)
cum_reg_thompson_sampling_contextual_alloc <- cumulativeRegretAverage(thompson_sampling_contextual_alloc$choice,visitor_reward,dt = dt)


linucb_contextual_alloc <- LINUCB(dt,visitor_reward )
cum_reg_linucb_contextual_alloc <- cumulativeRegretAverage(linucb_contextual_alloc$choice,visitor_reward,dt = dt)

kernelucb_contextual_alloc <- kernelucb(dt, visitor_reward, update_val = 500)
cum_reg_kernelucb_contextual_alloc <- cumulativeRegretAverage(kernelucb_contextual_alloc$choice,visitor_reward,dt=dt)
max(cum_reg_kernelucb_contextual_alloc)

random_alloc <- UniformBandit(visitor_reward)
cum_reg_random_alloc <- cumulativeRegretAverage(random_alloc$choice,visitor_reward,dt=dt)

############ Algorithm UCB ############

ucb_alloc  <- UCB( visitor_reward,alpha = 1)
cum_reg_ucb_alloc  <- cumulativeRegretAverage(ucb_alloc$choice, visitor_reward,dt=dt)


############ Epsilon greedy ######################

epsilonGreedy_alloc <- EpsilonGreedy( visitor_reward,epsilon  = 0.05)
cum_reg_epsilonGreedy_alloc  <- cumulativeRegretAverage(epsilonGreedy_alloc$choice,visitor_reward,dt=dt)



############ Thompson Sampling ############

thompson_sampling_alloc <- ThompsonSampling( visitor_reward)
cum_reg_thompson_sampling_alloc <- cumulativeRegretAverage(thompson_sampling_alloc$choice,visitor_reward,dt=dt)



###PLOT WITH GGPLOT2 REGRET###

library(ggplot2)

comp_reg <- data.frame(cbind(cum_reg_linucb_contextual_alloc,
                             cum_reg_kernelucb_contextual_alloc,
                             cum_reg_thompson_sampling_contextual_alloc,
                             cum_reg_random_alloc,
                             cum_reg_ucb_alloc,
                             cum_reg_epsilonGreedy_alloc,
                             cum_reg_thompson_sampling_alloc))



ggplot(comp_reg, aes(c(1:nrow(comp_reg)), y = value, color = Algorithm)) +
  geom_line(linetype="dashed",aes(y = cum_reg_ucb_alloc, col = "UCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_epsilonGreedy_alloc, col = "Epsilon Greedy"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_thompson_sampling_alloc, col = "Thompson Sampling"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_random_alloc, col = "Random"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_thompson_sampling_contextual_alloc, col = "Contextual TS"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_linucb_contextual_alloc, col = "LINUCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_kernelucb_contextual_alloc, col = "KernelUCB"),size = 0.5) +
  scale_colour_manual(values =  c("UCB"="brown","Epsilon Greedy"="orange","Thompson Sampling"="green","Random"="black", "Contextual TS"="dark green", "LINUCB"="blue","KernelUCB"= "purple"))+
  xlab("Time T") +
  ylab("Cumulative regret")





##### None linear #####

##### Pairewise #####
set.seed(1234)
size.tot <- 10000
x <- seq(0, 5, 0.01)
x1<- sample(x, size.tot, replace = TRUE, prob = NULL)
arm_1 <-  as.vector(c(2,-1,1.5,0))
K1 <- (x1 < 1 ) * arm_1[4]  +
  (x1 >= 1 & x1 < 2 ) * arm_1[1]  +
  (x1 >= 2 & x1 < 3) * arm_1[2]  +
  (x1 >= 3 & x1 < 4) * arm_1[3]  +
  (x1 >= 4) * arm_1[4]
plot(x1, K1)

arm_2 <-  as.vector(c(1.5,-0.5,1.25,0))
K2 <- (x1 < 1 ) * arm_2[4]  +
  (x1 >= 1 & x1 < 2 ) * arm_2[1]  +
  (x1 >= 2 & x1 < 3) * arm_2[2]  +
  (x1 >= 3 & x1 < 4) * arm_2[3]  +
  (x1 >= 4) * arm_2[4]
plot(x1, K2)
#covariate without interest
x2<- sample(x, size.tot, replace = TRUE, prob = NULL)
#Results for each variation
visitor_reward <-  data.frame(K1,K2 )
summary(visitor_reward)
dt <- as.data.frame(cbind(x1,x2))


controle_param = ctreeucb_parameters_control_default(dt=dt, visitor_reward=visitor_reward,learn_size=1500,  alpha=1, ctree_control_val= partykit::ctree_control(teststat = "quadratic"))

ctreeucb_alloc <- ctreeucb(dt, visitor_reward, ctree_parameters_control = controle_param )

#take data for online ab test for other algorithm
first <- ctreeucb_alloc$first_train_element
last <- nrow(visitor_reward)
dt.abtest <- dt[first:last,]
visitor_reward.abtest <- visitor_reward[first:last,]

cum_reg_ctreeucb_alloc <- cumulativeRegret(ctreeucb_alloc$choice,visitor_reward.abtest)



linucb_contextual_alloc <- LINUCB(dt.abtest,visitor_reward.abtest )
cum_reg_linucb_contextual_alloc <- cumulativeRegretAverage(linucb_contextual_alloc$choice,visitor_reward.abtest,dt = dt.abtest)

kernelucb_contextual_alloc <- kernelucb(dt.abtest, visitor_reward.abtest, update_val = 500)
cum_reg_kernelucb_contextual_alloc <- cumulativeRegretAverage(kernelucb_contextual_alloc$choice,visitor_reward.abtest,dt=dt.abtest)
max(cum_reg_kernelucb_contextual_alloc)

random_alloc <- UniformBandit(visitor_reward.abtest)
cum_reg_random_alloc <- cumulativeRegret(random_alloc$choice,visitor_reward.abtest)

############ Algorithm UCB ############
#source("/home/manue/Documents/manue/Manipulation/datascience-emmanuelle/programme_R/bandit/xp ctree_ucb/xp_revue code public/Bandit/R/ucb_bandit_object_evaluation.R")

ucb_alloc  <- UCB( visitor_reward.abtest,alpha = 1)
cum_reg_ucb_alloc  <- cumulativeRegret(ucb_alloc$choice, visitor_reward.abtest)


############ Epsilon greedy ######################

epsilonGreedy_alloc <- EpsilonGreedy( visitor_reward.abtest,epsilon  = 0.05)
cum_reg_epsilonGreedy_alloc  <- cumulativeRegret(epsilonGreedy_alloc$choice,visitor_reward.abtest)




###PLOT WITH GGPLOT2 REGRET###

library(ggplot2)

comp_reg <- data.frame(cbind(cum_reg_linucb_contextual_alloc,
                             cum_reg_kernelucb_contextual_alloc,
                             cum_reg_random_alloc,
                             cum_reg_ucb_alloc,
                             cum_reg_epsilonGreedy_alloc,
                             cum_reg_ctreeucb_alloc))



ggplot(comp_reg, aes(c(1:nrow(comp_reg)), y = value, color = Algorithm)) +
  geom_line(linetype="dashed",aes(y = cum_reg_ucb_alloc, col = "UCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_epsilonGreedy_alloc, col = "Epsilon Greedy"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_random_alloc, col = "Random"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_linucb_contextual_alloc, col = "LINUCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_kernelucb_contextual_alloc, col = "KernelUCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = cum_reg_ctreeucb_alloc, col = "CtreeUCB"),size = 0.5) +
  scale_colour_manual(values =  c("UCB"="brown","Epsilon Greedy"="orange","Random"="black","LINUCB"="blue","KernelUCB"= "purple" ,"CtreeUCB"="red"))+
  xlab("Time T") +
  ylab("Cumulative regret")





#### Real case

set.seed(1234)
####Data generate
temp <- paste(getwd(),"/data/abtest3.rda",sep="")
load(temp)
temp <- data3

##Formating
temp$value <-as.numeric( as.character(temp$value) )
temp$langID <- as.factor(temp$langID )
temp <- temp[!is.na(temp$variationID.y),]

###remplace NA in value
temp$value[is.na(temp$value)] <- 0
temp <- unique(temp)

list_col <- c("variationID.y", "visit.y","device","userAgent" ,"name" ,"value")
dt <- temp[,list_col]
summary(as.factor(dt$variationID))

dt <- unique(dt)


##Add test
dt$A <- NA
dt$B <- NA

for(i in 1:nrow(dt)){
  if(dt$variationID[i]== 0){ dt$A[i] = dt$value[i]
  }else{
    dt$B[i] = dt$value[i]
  }
}


##### Regret based on conditional means ###

library(partykit)
#Regression
ctree_models <- c()
#learn on A
ctree_models[[1]] <-  ctree_formula_generate(dt = dt,
                                             visitor_reward = dt[,c("A","B")],
                                             ctree_control_val = ctree_control(teststat = c("quadratic")),
                                             arm_for_learn = "A",
                                             explanatory_variable=  c("visit.y","device","userAgent" ,"name" ),
                                             learn_size = nrow(dt),
                                             print=TRUE)


#learn on B
ctree_models[[2]] <-  ctree_formula_generate(dt = dt,
                                             visitor_reward = dt[,c("A","B")],
                                             ctree_control_val = ctree_control(teststat = c("quadratic")),
                                             arm_for_learn = "B",
                                             explanatory_variable=  c("visit.y","device","userAgent" ,"name" ),
                                             learn_size = nrow(dt),
                                             print=TRUE)

#### Syntetique data ####
dt$A.pred <- NA
dt$B.pred <- NA

dt$A.pred <- predict(ctree_models[[1]],dt)
dt$B.pred <- predict(ctree_models[[2]],dt)




#####################################################################
######################################################################
###################### A/B Test ########################
rm(list=ls()[! ls() %in% c("dt")])
df <- dt


####Configuration
#Conf_30/70
config <- "30_70"

#Conf_100/100
config <- "100_100"



listCategorial =c("name","device","userAgent")
listInteger  = c("visit.y")

#Results for each variation
visitorReward <- df[,c("A.pred","B.pred")]

#Items caracteristics
dt <- df[, c(listCategorial,listInteger)]


set.seed(1234)


if(config  == "100_100" ){ learn_size = nrow(dt)*1
#### replication ###
rep_data <- 2
dt <- dt[rep(1:nrow(dt),each=rep_data),]
visitorReward <- visitorReward[rep(1:nrow(visitorReward),each=rep_data),]
}
if(config  ==  "30_70"  ) learn_size = nrow(dt)*0.30




dt.old <- dt




####CTREEUCBPARAMETER
## - the size of the learning set is already calculated according to the selected configuration (learn_size)
#  - mincriterion parameter refers to 1 -risk error accepted  (0.99,0.95,0.90)
#  - alpha refers to the dynamic allocation parameter (U.C.B)
#  - arm_for_learn is the original varitation (names(visitorReward)[1] or names(visitorReward)[2])
#  testtype and teststat is refer to type of test to build the tree (see the paper for more details)
# and are not supposed to be modified#


ctreeucb_parameters_control <- ctreeucb_parameters_control_default(dt = dt.old,
                                                                   visitorReward ,
                                                                   learn_size = learn_size,
                                                                   alpha = 1,
                                                                   arm_for_learn = names(visitorReward)[1],
                                                                   is_reward_are_boolean = FALSE,
                                                                   ctree_control_val=ctree_control(
                                                                     mincriterion = 0.95,
                                                                     testtype = "Bonferroni",
                                                                     teststat = "quadratic",
                                                                     splitstat = c( "quadratic"))
)


my_ctree_ucb <- ctreeucbBanditObjectEvaluation(dt= dt.old,visitor_reward=visitorReward, ctree_parameters_control= ctreeucb_parameters_control, average = TRUE)
max(my_ctree_ucb$cum_reg_ctree)
###END CTREE UCB###


###Data format###
###Other algorithms require binary or continuous variables.
dt <- as.data.frame(transform_categorial_to_binary( listCategorial =listCategorial ,listInteger=listInteger, dt=dt))
colnames(dt) <- paste(rep("col",ncol(dt)),as.character(c(1:ncol(dt))) ,sep="")
first <- my_ctree_ucb$ctreeucb_bandit_alloc$first_train_element
last <- nrow(dt)
dt <- dt[first:last,]
dt.reward <- dt.old[first:last,]
visitorReward <- visitorReward[first:last,]
my_linucb_ucb <- LinucbBanditObjectEvaluation(dt=dt, visitor_reward=visitorReward,alpha = 1, average = TRUE, IsRewardAreBoolean = FALSE,dt.reward=dt.reward)
max(my_linucb_ucb$cum_reg_linucb)
### END Lin UCB ###


### Kernel UCB ###
kernel_ucb <-  kernelucbBanditObjectEvaluation(dt=dt, visitor_reward=visitorReward,alpha = 1, average = TRUE, IsRewardAreBoolean = FALSE,dt.reward=dt.reward)
max(kernel_ucb$cum_reg_kernelucb)
### END Kernel UCB ###

### Random ###
unif_alloc <- uniform_bandit_object_evaluation(visitor_reward=visitorReward,average = TRUE, IsRewardAreBoolean = FALSE,dt.reward=dt.reward)
max(unif_alloc$cum_reg_uniform_bandit_alloc)
### END RANDOM ###


### UCB ###
ucb_alloc <-  UcbBanditObjectEvaluation(visitor_reward=visitorReward,alpha = 1,average = TRUE, IsRewardAreBoolean = FALSE,dt.reward=dt.reward)
max(ucb_alloc$cum_reg_ucb_alloc)
###END UCB###


### PLOT  OF REGRET###

###PLOT WITH GGPLOT2 REGRET###
library(ggplot2)

comp_reg <- data.frame(cbind(my_ctree_ucb$cum_reg_ctree,
                             ucb_alloc$cum_reg_ucb_alloc,
                             my_linucb_ucb$cum_reg_linucb,
                             kernel_ucb$cum_reg_kernelucb,
                             unif_alloc$cum_reg_uniform_bandit_alloc))


ggplot(comp_reg, aes(c(1:nrow(comp_reg)), y = value, color = Algorithm)) +
  geom_line(linetype="dashed",aes(y = my_ctree_ucb$cum_reg_ctree, col = "Ctreeucb"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = ucb_alloc$cum_reg_ucb_alloc, col = "UCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = my_linucb_ucb$cum_reg_linucb, col = "LinUCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = kernel_ucb$cum_reg_kernelucb, col = "KernelUCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = unif_alloc$cum_reg_uniform_bandit_alloc, col = "Uniform"),size = 0.5) +
  scale_colour_manual(values =  c("UCB"="brown","LinUCB"="blue","KernelUCB"="purple","Ctreeucb"="red","Uniform"="black"))+
  xlab("Time") +
  ylab("Cumulative Regret")



## Demo UCB Rejection Sampling ##
