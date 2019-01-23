#please set the directory to this file location


#setwd("~/user_name_directory/ctree_ucb/code"))
# the following line is for getting the path of your current open file
current_path <-  getwd()

source(paste(current_path,'/install_package.R',sep=""))
source(paste(current_path,'/Formating data/R/transform_categorial_to_binary.R',sep=""))

#memory.limit(size = 2500)

####Configuration
#Conf_30/70
config <- "30_70"
df <- read.csv(paste(current_path,"/Data/ab_test_dataset_0_3",sep=""),sep=",")

#Conf_100/100
#config <- "100_100"
#df <- read.csv(paste(current_path,"/Data/ab_test_dataset_0_5",sep=""),sep=",")


###Data format###
df$X <- NULL
df$hastrans <- NULL
df$variationID.y <- NULL
df$langID <- as.factor(df$langID)
df$countryID <- as.factor(df$countryID)


listCategorial =c("countryID","langID","name","device","userAgent")
listInteger  = c("latitude","longitude")

#Results for each variation
visitorReward <- df[,c("A","B")]

#Items caracteristics
dt <- df[, c(listCategorial,listInteger)]


set.seed(1234)


###File path



####CTREE UCB####

source("Bandit/R/ctree_object_evaluation.R")




dt.old <- dt

if(config  == "100_100" ) learn_size = 6216
if(config  ==  "30_70"  ) learn_size = 1865

####CTREEUCBPARAMETER
## - the size of the learning set is already calculated according to the selected configuration (learn_size)
#  - mincriterion parameter refers to 1 -risk error accepted  (0.99,0.95,0.90)
#  - alpha refers to the dynamic allocation parameter (U.C.B)
#  - arm_for_learn is the original varitation (names(visitorReward)[1] or names(visitorReward)[2])
#  testtype and teststat is refer to type of test to build the tree (see the paper for more details)
# and are not supposed to be modified#


ctree_parameters_control <- ctree_parameters_control_default(dt = dt.old,
                                                             visitorReward ,
                                                             learn_size = learn_size,
                                                             alpha = 1,
                                                             arm_for_learn = names(visitorReward)[1],
                                                             is_reward_are_boolean = TRUE,
                                                             ctree_control_val=ctree_control(
                                                               mincriterion = 0.95,
                                                               testtype = "Teststatistic",
                                                               teststat = "quadratic",
                                                               splitstat = c( "quadratic"))
                                                              )


my_ctree_ucb <- ctree_object_evaluation(dt= dt.old,visitorReward=visitorReward, ctree_parameters_control= ctree_parameters_control) 
max(my_ctree_ucb$cum_reg_ctree)
###END CTREE UCB###


###Data format###
###Other algorithms require binary or continuous variables.
dt <- transform_categorial_to_binary( listCategorial =listCategorial ,listInteger=listInteger, dt=dt)


### Lin UCB ###
source("Bandit/R/linucb_object_evaluation.R")
linucb_parameters_control_par =linucb_parameters_control_default(dt,visitorReward,alpha = 1)

my_linucb_ucb <-  linucb_object_evaluation(dt=dt[c(my_ctree_ucb$ctree_res$first_train_element:nrow(dt)),],
                                           visitorReward=visitorReward[c(my_ctree_ucb$ctree_res$first_train_element:nrow(visitorReward)),],
                                           linucb_parameters_control = linucb_parameters_control_par)
max(my_linucb_ucb$cum_reg_linucb)
### END Lin UCB ###

    
### Kernel UCB ###
source("Bandit/R/kernelucb_object_evaluation.R")

### Kernel UCB
#update_val parameter : Kernel UCB requires a very important calculation time. Updating results after X items reduces calculation time

kernel_ucb <- kernel_object_evaluation(dt=dt[c(my_ctree_ucb$ctree_res$first_train_element:nrow(dt)),],
                                       visitorReward=visitorReward[c(my_ctree_ucb$ctree_res$first_train_element:nrow(visitorReward)),],
                                       linucb_parameters_control=linucb_parameters_control_default(dt,visitorReward),
                                       update_val= 100, alpha=1)
max(kernel_ucb$cum_reg_kernelucb)
### END Kernel UCB ###



### Random ###
source("Bandit/R/random_bandit_object_evaluation.R")
random_alloc <- random_bandit_object_evaluation(visitorReward=visitorReward[c(my_ctree_ucb$ctree_res$first_train_element:nrow(visitorReward)),])
max(random_alloc$cum_reg_random_alloc)
### END RANDOM ###


### UCB ###
source("Bandit/R/ucb_bandit_object_evaluation.R")
ucb_alloc <- ucb_bandit_object_evaluation(visitorReward=visitorReward[c(my_ctree_ucb$ctree_res$first_train_element:nrow(visitorReward)),],alpha = 1)
max(ucb_alloc$cum_reg_ucb_alloc)
###END UCB###


### PLOT  OF REGRET###

plot(c(1:length(my_ctree_ucb$cum_reg_ctree)), my_ctree_ucb$cum_reg_ctree, type = "l")
lines(c(1:length(my_linucb_ucb$cum_reg_linucb)),my_linucb_ucb$cum_reg_linucb, col = "red")
lines(c(1:length(kernel_ucb$cum_reg_kernelucb)),kernel_ucb$cum_reg_kernelucb, col = "orange")
lines(c(1:length(random_alloc$cum_reg_random_alloc)),random_alloc$cum_reg_random_alloc, col = "violet")
lines(c(1:length(ucb_alloc$cum_reg_ucb_alloc)),ucb_alloc$cum_reg_ucb_alloc, col = "blue")



###PLOT WITH GGPLOT2 REGRET###
library(ggplot2)

comp_reg <- data.frame(cbind(my_ctree_ucb$cum_reg_ctree,
                             ucb_alloc$cum_reg_ucb_alloc,
                             my_linucb_ucb$cum_reg_linucb,
                             kernel_ucb$cum_reg_kernelucb,
                             random_alloc$cum_reg_random_alloc))



#pdf('../results/cumRegret.pdf')

ggplot(comp_reg, aes(c(1:nrow(comp_reg)), y = value, color = Algorithm)) + 
  geom_line(linetype="dashed",aes(y = my_ctree_ucb$cum_reg_ctree, col = "CTREE"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = ucb_alloc$cum_reg_ucb_alloc, col = "UCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = my_linucb_ucb$cum_reg_linucb, col = "LinUCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = kernel_ucb$cum_reg_kernelucb, col = "KernelUCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = random_alloc$cum_reg_random_alloc, col = "Random"),size = 0.5) +
  xlab("Time") +
  ylab("Regret")
  
#dev.off()

###plot cum regret for each subgroupe of ctreeucb ###
plot_cum_regret_for_each_subgroupe <- function(ctree_ucb_object, K=ncol(visitorReward)){
  
  
  dt <- ctree_ucb_object$ctree_res$data_context
  
  choice <- dt$choice
  visitorReward <- dt[,c((ncol(dt)-K+1):ncol(dt)),K]
  
  
  
  source("Bandit/R/regret.R")
  
  print(length(levels(as.factor(dt$groups))))
  for(i in levels(as.factor(dt$groups))){
    print(i)
    
    temp_choice  <- dt[dt$groups == i,]$choice
    
    
    temp_visitor_reward <- visitorReward[dt$groups == i,]
    temp_cum_reg <- cumulativeRegret(temp_choice,temp_visitor_reward)
    
    

    
 #   pdf(paste("../results/cumulative regret for subgroup ",i,".pdf",sep = ""))
    plot(temp_cum_reg, type ='l', ylab = paste("cumulative regret for subgroup ",i,sep = ""))
#    dev.off()
    
    
    temp_choice  <- NULL
    temp_visitor_reward   <- NULL
    temp_cum_reg   <- NULL
    
  }
  
  
  
}

plot_cum_regret_for_each_subgroupe(my_ctree_ucb, K=ncol(visitorReward))


#cumulative regret 
max(my_ctree_ucb$cum_reg_ctree)
max(my_linucb_ucb$cum_reg_linucb)
max(ucb_alloc$cum_reg_ucb_alloc)
max(kernel_ucb$cum_reg_kernelucb)
max(random_alloc$cum_reg_random_alloc)




