#Test with Adult UCI database
###Data format###
temp <- paste(getwd(),"/data/adult.data",sep="")
df <- read.table(temp)

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
                                                                   is_reward_are_boolean = TRUE,
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
dt <- transform_categorial_to_binary( listCategorial =listCategorial ,listInteger=listInteger, dt=dt)
#colnames(dt) <- paste(rep("col",74),as.character(c(1:74)) ,sep="")
first <- my_ctree_ucb$ctreeucb_bandit_alloc$first_train_element
last <- nrow(dt)
dt <- dt[first:last,]
dt.reward <- dt.old[first:last,]
visitorReward <- visitorReward[first:last,]
my_linucb_ucb <- LinucbBanditObjectEvaluation(dt=dt, visitor_reward=visitorReward,average = TRUE, IsRewardAreBoolean = TRUE,dt.reward=dt.reward)
max(my_linucb_ucb$cum_reg_linucb)
### END Lin UCB ###


### Kernel UCB ###
kernel_ucb <-  kernelucbBanditObjectEvaluation(dt=dt, visitor_reward=visitorReward,average = TRUE, IsRewardAreBoolean = TRUE,dt.reward=dt.reward)
max(kernel_ucb$cum_reg_kernelucb)
### END Kernel UCB ###

### Random ###
unif_alloc <- uniform_bandit_object_evaluation(visitor_reward=visitorReward,average = TRUE, IsRewardAreBoolean = TRUE,dt.reward=dt.reward)
max(unif_alloc$cum_reg_uniform_bandit_alloc)
### END RANDOM ###


### UCB ###
ucb_alloc <-  UcbBanditObjectEvaluation(visitor_reward=visitorReward,alpha = 1,average = TRUE, IsRewardAreBoolean = TRUE,dt.reward=dt.reward)
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
  geom_line(linetype="dashed",aes(y = unif_alloc$cum_reg_uniform_bandit_alloc, col = "Uniformm"),size = 0.5) +
  xlab("Time") +
  ylab("Regret")




plot_cum_regret_for_each_subgroupe(my_ctree_ucb, average = TRUE)


#cumulative regret
max(my_ctree_ucb$cum_reg_ctree)
max(my_linucb_ucb$cum_reg_linucb)
max(ucb_alloc$cum_reg_ucb_alloc)
max(kernel_ucb$cum_reg_kernelucb)
max(unif_alloc$cum_reg_uniform_bandit_alloc)

### PLOT  OF REWARD###
my_ctree_ucb.reward <- reward_cumulative(my_ctree_ucb$ctreeucb_bandit_alloc$choice, visitor_reward = visitorReward)
ucb_alloc.reward <- reward_cumulative(ucb_alloc$ucb_alloc$choice, visitor_reward = visitorReward)
my_linucb_ucb.reward <- reward_cumulative(my_linucb_ucb$linucb_bandit_alloc$choice, visitor_reward = visitorReward)
kernel_ucb.reward <- reward_cumulative(kernel_ucb$kernelucb_bandit_alloc$choice, visitor_reward = visitorReward)
unif_alloc.reward <- reward_cumulative(unif_alloc$uniform_bandit_alloc$choice, visitor_reward = visitorReward)

###PLOT WITH GGPLOT2 REWARD###
library(ggplot2)

comp_reward <- data.frame(cbind(my_ctree_ucb.reward,
                                ucb_alloc.reward,
                                my_linucb_ucb.reward,
                                kernel_ucb.reward,
                                unif_alloc.reward))


ggplot(comp_reg, aes(c(1:nrow(comp_reward)), y = value, color = Algorithm)) +
  geom_line(linetype="dashed",aes(y = my_ctree_ucb.reward, col = "Ctreeucb"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = ucb_alloc.reward, col = "UCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = my_linucb_ucb.reward, col = "LinUCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = kernel_ucb.reward, col = "KernelUCB"),size = 0.5) +
  geom_line(linetype="dashed",aes(y = unif_alloc.reward, col = "Uniformm"),size = 0.5) +
  xlab("Time") +
  ylab("Reward")

