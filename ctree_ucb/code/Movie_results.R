#please set the directory to this file location


#setwd("~/user_name_directory/ctree_ucb/code")

# the following line is for getting the path of your current open file
current_path <-  getwd()


source(paste(current_path,'/install_package.R',sep=""))

#COLLECT DATA FROM MOVIELENS
source(paste(current_path,'/Formating data/R/Movie_formating.R',sep=""))



#multiply the dataset if Config 100_1OO
n <- 2
dt <- do.call("rbind", replicate(n, dt, simplify = FALSE))
visitorReward  <- do.call("rbind", replicate(n, visitorReward, simplify = FALSE))




###File path
#(WD <- getwd())
#WD <- paste(current_path , "/Bandit", sep = "")
#if (!is.null(WD)) setwd(WD)


####CTREE UCB####

source("Bandit/R/ctree_object_evaluation.R")

##Data formating for ctree_ucb
temp_dt  <- dt
for(i in 1:ncol(temp_dt  )) temp_dt[,i] <- as.factor(temp_dt[,i])

####CTREEUCBPARAMETER
## - the size of the learning set is a percent of the all dataset nrow(dt)*0.3 or nrow(dt)*0.5
#  - mincriterion parameter refers to 1 -risk error accepted  (0.99,0.95,0.90)
#  - alpha refers to the dynamic allocation parameter (U.C.B)
#  - arm_for_learn is the original varitation (names(visitorReward)[1] or names(visitorReward)[2] ...or  names(visitorReward)[5] )
#  testtype and teststat is refer to type of test to build the tree (see the paper for more details)
# and are not supposed to be modified#

#Do not multiply the dataset if Config 30_70
#Config 30_70
#  learn_size = nrow(dt)*0.3

#Config 100_100
  learn_size = nrow(dt)*0.5

ctree_parameter_def <- ctree_parameters_control_default(dt = temp_dt,
                                                        visitorReward ,
                                                        learn_size = learn_size,
                                                        alpha = 0.25,
                                                        arm_for_learn= names(visitorReward)[5],
                                                        ctree_control_val=ctree_control(
                                                                                       mincriterion = 0.95,
                                                                                       testtype = "Teststatistic",
                                                                                        teststat = "quadratic")
                                                        )


my_ctree_ucb <- ctree_object_evaluation(dt= temp_dt,visitorReward=visitorReward, ctree_parameters_control= ctree_parameter_def) 
max(my_ctree_ucb$cum_reg_ctree)

###END CTREE UCB###


### Lin UCB ###
source("Bandit/R/linucb_object_evaluation.R")
linucb_parameters_control_par =linucb_parameters_control_default(dt,visitorReward,alpha = 0.25)

my_linucb_ucb <-  linucb_object_evaluation(dt=dt[c(my_ctree_ucb$ctree_res$first_train_element:nrow(dt)),],
                                           visitorReward=visitorReward[c(my_ctree_ucb$ctree_res$first_train_element:nrow(visitorReward)),],
                                                                                      linucb_parameters_control = linucb_parameters_control_par)

max(my_linucb_ucb$cum_reg_linucb)
### END Lin UCB ###


### Kernel UCB ###

source("Bandit/R/kernelucb_object_evaluation.R")

#update_val parameter : Kernel UCB requires a very important calculation time. Updating results after X items reduces calculation time

         kernel_ucb <- kernel_object_evaluation(dt=dt[c(my_ctree_ucb$ctree_res$first_train_element:nrow(dt)),],
                                             visitorReward=visitorReward[c(my_ctree_ucb$ctree_res$first_train_element:nrow(visitorReward)),],
                                           linucb_parameters_control=linucb_parameters_control_default(dt,visitorReward),
                                            update_val= 100,alpha=0.25)
max(kernel_ucb$cum_reg_kernelucb)
### END Kernel UCB ###



### Random ###
source("Bandit/R/random_bandit_object_evaluation.R")
random_alloc <- random_bandit_object_evaluation(visitorReward=visitorReward[c(my_ctree_ucb$ctree_res$first_train_element:nrow(visitorReward)),])
max(random_alloc$cum_reg_random_alloc)
### END RANDOM ###

### UCB ###
source("Bandit/R/ucb_bandit_object_evaluation.R")
ucb_alloc <- ucb_bandit_object_evaluation(visitorReward=visitorReward[c(my_ctree_ucb$ctree_res$first_train_element:nrow(visitorReward)),],
                                          alpha = 0.25)

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
  
  print(length(levels(as.factor(dt$groups))))
  
  source("Bandit/R/regret.R")
  
  for(i in levels(as.factor(dt$groups))){
    
    
    temp_choice  <- dt[dt$groups == i,]$choice
    temp_visitor_reward <- visitorReward[dt$groups == i,]
    temp_cum_reg <- cumulativeRegret(temp_choice,temp_visitor_reward)
    
  #  pdf(paste("../results/cumulative regret for subgroup ",i,".pdf",sep = ""))
    plot(temp_cum_reg, type ='l', ylab = paste("cumulative regret for subgroup ",i,sep = ""))
 #   dev.off()
    
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

