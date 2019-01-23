#please set the directory to this file location


#setwd("~/user_name_directory/ctree_ucb/code"))

#setwd( getwd() )
# the following line is for getting the path of your current open file
current_path <-  getwd()


source(paste(current_path,'/install_package.R',sep=""))
source(paste(current_path,'/Formating data/R/transform_categorial_to_binary.R',sep=""))

memory.limit(size = 2500)


###Data format###
df  = read.table(paste(current_path,"/Data/adult.data",sep=""), sep = ",")  # read text file
df$V1 <- NULL

listCategorial =c("V2","V4","V6","V7","V8","V9")

listInteger  = c("V3","V5","V11","V12","V13")


visitorReward <- as.data.frame(transform_categorial_to_binary( listCategorial = c("V15"), dt=df))
for(i in 1:ncol(visitorReward)) visitorReward[,i] <- as.integer(visitorReward[,i])
###
dt <- df[, c(listCategorial,listInteger)]
dt$V15 <- NULL


#multiply the dataset (if Config 100_100)
n <- 2
dt <- do.call("rbind", replicate(n, dt, simplify = FALSE))
visitorReward  <- do.call("rbind", replicate(n, visitorReward, simplify = FALSE))



###File path
(WD <- getwd())

if (!is.null(WD)) setwd(WD)

####CTREE UCB####

source("Bandit/R/ctree_object_evaluation.R")

dt.old <- dt

set.seed(1234)


####CTREEUCBPARAMETER
## - the size of the learning set is a percent of the all dataset nrow(dt)*0.3 or nrow(dt)*0.5
#  - mincriterion parameter refers to 1 -risk error accepted  (0.99,0.95,0.90)
#  - alpha refers to the dynamic allocation parameter (U.C.B)
#  - arm_for_learn is the original varitation (names(visitorReward)[1] or names(visitorReward)[2] ...or  names(visitorReward)[5] )
#  testtype and teststat is refer to type of test to build the tree (see the paper for more details)
# and are not supposed to be modified#

#Do not multiply the dataset if Config 30_70
#Config 30_70
learn_size = nrow(dt.old)*0.30

#multiply the dataset if Config 100_100
#Config 100_100
learn_size = nrow(dt.old)*0.5

ctree_parameters_control <- ctree_parameters_control_default(dt = dt.old,
                                                             visitorReward ,
                                                             learn_size = learn_size,
                                                             alpha = 1,
                                                             arm_for_learn = names(visitorReward)[2],
                                                             is_reward_are_boolean = TRUE,
                                                             ctree_control_val=ctree_control(
                                                             mincriterion = 0.95,
                                                             testtype = "MonteCarlo",
                                                             teststat = "maximum"
                                                           )
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
#update_val parameter : Kernel UCB requires a very important calculation time. Updating results after X items reduces calculation time

source("Bandit/R/kernelucb_object_evaluation.R")
kernel_ucb <- kernel_object_evaluation(dt=dt[c(my_ctree_ucb$ctree_res$first_train_element:nrow(dt)),],
                                      visitorReward=visitorReward[c(my_ctree_ucb$ctree_res$first_train_element:nrow(visitorReward)),],
                                       linucb_parameters_control=linucb_parameters_control_default(dt,visitorReward),
                                       update_val= 100,
                                      alpha=1)
max(kernel_ucb$cum_reg_kernelucb)

### END Kernel UCB ###



### Random ###
source("Bandit/R/random_bandit_object_evaluation.R")
random_alloc <- random_bandit_object_evaluation(visitorReward=visitorReward[c(my_ctree_ucb$ctree_res$first_train_element:nrow(visitorReward)),])

### END Random ###

### UCB ###
source("Bandit/R/ucb_bandit_object_evaluation.R")
ucb_alloc <- ucb_bandit_object_evaluation(visitorReward=visitorReward[c(my_ctree_ucb$ctree_res$first_train_element:nrow(visitorReward)),],alpha = 1)
max(ucb_alloc$cum_reg_ucb_alloc)

#END UCB

### PLOT  OF REGRET###
plot(c(1:length(my_ctree_ucb$cum_reg_ctree)), my_ctree_ucb$cum_reg_ctree, type = "l")
lines(c(1:length(my_linucb_ucb$cum_reg_linucb)),my_linucb_ucb$cum_reg_linucb, col = "red")
lines(c(1:length(kernel_ucb$cum_reg_kernelucb)),kernel_ucb$cum_reg_kernelucb, col = "orange")
lines(c(1:length(random_alloc$cum_reg_random_alloc)),random_alloc$cum_reg_random_alloc, col = "violet")
lines(c(1:length(ucb_alloc$cum_reg_ucb_alloc)),ucb_alloc$cum_reg_ucb_alloc, col = "blue")





max(ucb_alloc$cum_reg_ucb_alloc)
max(my_ctree_ucb$cum_reg_ctree)
max(ucb_alloc$cum_reg_ucb_alloc) - max(my_ctree_ucb$cum_reg_ctree)

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





plot_cum_regret_for_each_subgroupe <- function(ctree_ucb_object, K=ncol(visitorReward)){
  
  
  dt <- ctree_ucb_object$ctree_res$data_context
  
  choice <- dt$choice
  visitorReward <- dt[,c((ncol(dt)-K+1):ncol(dt)),K]
  
  
  
  source("Bandit/R/regret.R")
  print(length(levels(as.factor(dt$groups))))
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

###########STANDARD DEVIATION  ###########
##If you want observe the sd of results 
## Only for MonteCarlo option

##ctree_parameters_control <- ctree_parameters_control_default(dt = dt.old,
#                                                             visitorReward ,
#                                                             learn_size = learn_size,
#                                                             alpha = 1,
#                                                             arm_for_learn = names(visitorReward)[1],
#                                                             is_reward_are_boolean = TRUE,
#                                                             ctree_control_val=ctree_control(
#                                                               mincriterion = 0.99,
#                                                               testtype = "MonteCarlo",
#                                                               teststat = "maximum",
#                                                               minprob = 0.05
#                                                            )
#)

#res <- c()
#for(i in 1:25){
#  print(i)
#  my_ctree_ucb <- ctree_object_evaluation(dt= dt.old,visitorReward=visitorReward, ctree_parameters_control= ctree_parameters_control) 
  
#  res[i]<- max(my_ctree_ucb$cum_reg_ctree)
  
#  rm(my_ctree_ucb)
  
#}


#var(res) 
#sd(res)
#mean(res)

#########"
