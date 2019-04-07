df  =  read.csv2("/home/manue/Documents/manue/GitHub/R-CTree-UCB/bandit4abtest/data/movielens.data",sep=";",header = TRUE)
df$X = NULL
dt <- df[,1:19]
visitorReward <- df[,c(20:24)]

#multiply the dataset if Config 100_1OO
n <- 2
dt <- do.call("rbind", replicate(n, dt, simplify = FALSE))
visitorReward  <- do.call("rbind", replicate(n, visitorReward, simplify = FALSE))

###Statistics
temp <-as.data.frame( c(visitorReward[,1] ,visitorReward[,2] ,visitorReward[,3] ,visitorReward[,4], visitorReward[,5] ) )
temp$user <- "0"
temp$user[1:9125] <- "A"
temp$user[9125: 18250] <- "B"
temp$user[18250:27375] <- "C"
temp$user[27375:36500] <- "D"
temp$user[36500:45625] <- "E"

colnames(temp) <- c("valeur","variation")

ajuste <- lm(temp$valeur ~ temp$variation)
summary(ajuste)
anova(ajuste)

a1 <- aov(temp$valeur ~ temp$variation)
posthoc <- TukeyHSD(x=a1, 'temp$variation', conf.level=0.95)
posthoc

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
#learn_size = nrow(dt.old)*0.5


####CTREEUCBPARAMETER
## - the size of the learning set is already calculated according to the selected configuration (learn_size)
#  - mincriterion parameter refers to 1 -risk error accepted  (0.99,0.95,0.90)
#  - alpha refers to the dynamic allocation parameter (U.C.B)
#  - arm_for_learn is the original varitation (names(visitorReward)[1] or names(visitorReward)[2])
#  testtype and teststat is refer to type of test to build the tree (see the paper for more details)
# and are not supposed to be modified#
library(partykit)

ctreeucb_parameters_control <- ctreeucb_parameters_control_default(dt = dt.old,
                                                                   visitorReward ,
                                                                   learn_size = learn_size,
                                                                   alpha = 1,
                                                                   arm_for_learn = names(visitorReward)[1],
                                                                   is_reward_are_boolean = FALSE,
                                                                   ctree_control_val=ctree_control(
                                                                     mincriterion = 0.95,
                                                                     testtype = "Teststatistic",
                                                                     teststat = "quadratic"
                                                                     )
)


my_ctree_ucb <- ctreeucbBanditObjectEvaluation(dt= dt.old,visitor_reward=visitorReward, ctree_parameters_control= ctreeucb_parameters_control)
max(my_ctree_ucb$cum_reg_ctree)
###END CTREE UCB###


first <- my_ctree_ucb$ctreeucb_bandit_alloc$first_train_element
last <- nrow(dt)
dt <- dt[first:last,]
visitorReward <- visitorReward[first:last,]
my_linucb_ucb <- LinucbBanditObjectEvaluation(dt=dt, visitor_reward=visitorReward)
max(my_linucb_ucb$cum_reg_linucb)
### END Lin UCB ###


### Kernel UCB ###
dt <- sapply(dt,as.numeric)
kernel_ucb <-  kernelucbBanditObjectEvaluation(dt=dt, visitor_reward=visitorReward)
max(kernel_ucb$cum_reg_kernelucb)
### END Kernel UCB ###

### Random ###
unif_alloc <- uniform_bandit_object_evaluation(visitor_reward=visitorReward)
max(unif_alloc$cum_reg_uniform_bandit_alloc)
### END RANDOM ###


### UCB ###
ucb_alloc <-  UcbBanditObjectEvaluation(visitor_reward=visitorReward,alpha = 1)
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




plot_cum_regret_for_each_subgroupe(my_ctree_ucb)


#cumulative regret
max(my_ctree_ucb$cum_reg_ctree)
max(my_linucb_ucb$cum_reg_linucb)
max(ucb_alloc$cum_reg_ucb_alloc)
max(kernel_ucb$cum_reg_kernelucb)
max(unif_alloc$cum_reg_uniform_bandit_alloc)



