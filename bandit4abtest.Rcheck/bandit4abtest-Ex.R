pkgname <- "bandit4abtest"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('bandit4abtest')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("LINUCB")
### * LINUCB

flush(stderr()); flush(stdout())

### Name: LINUCB
### Title: LINUCB algorithm
### Aliases: LINUCB

### ** Examples

size.tot = 1000
set.seed(4649)                          # this makes the example exactly reproducible
x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
x2 = runif(size.tot, min=0, max=10)
x3 = runif(size.tot, min=0, max=10)
x4 = runif(size.tot, min=0, max=10)
dt = cbind(x1,x2,x3,x4)
#arm reward
arm_1 <-  as.vector(c(-1,9,-8,4))
K1 = crossprod(t(dt),arm_1)
arm_2 <-  as.vector(c(-1,2,1,0))
K2 = crossprod(t(dt),arm_2)
arm_3 <-  as.vector(c(-1,-5,1,10))
K3 = crossprod(t(dt),arm_3)
visitorReward <-  data.frame(K1,K2,K3)
dt <- as.data.frame(dt)
LINUCB(dt,visitorReward)



cleanEx()
nameEx("LOGITUCB")
### * LOGITUCB

flush(stderr()); flush(stdout())

### Name: LOGITUCB
### Title: LogitUCB algorithm
### Aliases: LOGITUCB

### ** Examples

size.tot = 1000
set.seed(4649)                          # this makes the example exactly reproducible
x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
x2 = runif(size.tot, min=0, max=10)
x3 = runif(size.tot, min=0, max=10)
x4 = runif(size.tot, min=0, max=10)
dt = cbind(x1,x2,x3,x4)
#arm reward
arm_1 <-  as.vector(c(-1,9,-8,4))
K1 = 1/(1+exp(- crossprod(t(dt),arm_1))) # inverse logit transform of linear predictor
K1 = vapply(K1, function(x) rbinom(1, 1, x), as.integer(1L))
arm_2 <-  as.vector(c(-1,2,1,0))
K2 = 1/(1+exp(- crossprod(t(dt),arm_2))) # inverse logit transform of linear predictor
K2 = vapply(K2, function(x) rbinom(1, 1, x), as.integer(1L))
arm_3 <-  as.vector(c(-1,-5,1,10))
K3 = 1/(1+exp(- crossprod(t(dt),arm_3)))
K3 = vapply(K3, function(x) rbinom(1, 1, x), as.integer(1L))
visitorReward <-  data.frame(K1,K2,K3)
dt <- as.data.frame(dt)
LOGITUCB(dt,visitorReward)



cleanEx()
nameEx("UCB")
### * UCB

flush(stderr()); flush(stdout())

### Name: UCB
### Title: UCB algorithm
### Aliases: UCB

### ** Examples

## Generates 10000 numbers from 2 binomial  distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2) )
#Run UCB algorithm
ucb_alloc  <- UCB(visitorReward,alpha = 10)
ucb_alloc$S
barplot(table(ucb_alloc$choice),main = "Histogram of choices",xlab="arm")
#Upper bound for arm 2 according iterations (red line is the real mean)
plot(x=c(1:length(ucb_alloc$choice[ucb_alloc$choice==2])),
  ucb_alloc$proba[ucb_alloc$choice==2],
  type='l',xlab = 'Time',ylab = 'Upper bound of arm 2')
  lines(c(1:length(ucb_alloc$choice[ucb_alloc$choice==2])),rep(mean(K2),length(ucb_alloc$choice[ucb_alloc$choice==2])),col="red")
ucb_alloc$time
ucb_alloc$theta_hat
ucb_alloc$theta



cleanEx()
nameEx("abtest1")
### * abtest1

flush(stderr()); flush(stdout())

### Name: abtest1
### Title: abtest1
### Aliases: abtest1
### Keywords: datasets

### ** Examples

 try(data(package = "bandit4abtest") )
 load(abtest1)



cleanEx()
nameEx("abtest2")
### * abtest2

flush(stderr()); flush(stdout())

### Name: abtest2
### Title: abtest2
### Aliases: abtest2
### Keywords: datasets

### ** Examples

 try(data(package = "bandit4abtest") )
 load(abtest1)



cleanEx()
nameEx("bandit_reward_control")
### * bandit_reward_control

flush(stderr()); flush(stdout())

### Name: bandit_reward_control
### Title: bandit_reward_control
### Aliases: bandit_reward_control

### ** Examples

## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rnorm(100, 30, .05)
K2 <- rnorm(100, 21, .05)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2) )
## Control
bandit_reward_control(visitorReward,K=3)
## Control
bandit_reward_control(visitorReward,K=2)
visitorReward[1,1]= NA
## Control
bandit_reward_control(visitorReward)




cleanEx()
nameEx("change_data_type_for_ctree")
### * change_data_type_for_ctree

flush(stderr()); flush(stdout())

### Name: change_data_type_for_ctree
### Title: change_data_type_for_ctree
### Aliases: change_data_type_for_ctree

### ** Examples

## Generates 1000 numbers from 2 uniform distributions
c1 <- rep("Test",100)
dt <- as.data.frame(c1)
typeof(change_data_type_for_ctree(dt=dt,visitorReward=dt)$visitorReward[,1])
K1 <- sample(c(0,1),replace=TRUE,size= 100)
visitorReward <- as.data.frame(K1)
typeof(change_data_type_for_ctree(dt=dt,visitorReward=dt,is_reward_are_boolean=FALSE)$visitorReward[,1])



cleanEx()
nameEx("change_type")
### * change_type

flush(stderr()); flush(stdout())

### Name: change_type
### Title: Change all colomns of a dataframe into numerical type
### Aliases: change_type

### ** Examples

## Generates 1000 numbers from 2 binomial distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame( cbind(as.character(K1),as.character(K2)) )
typeof(visitorReward[,1])
## Change type
visitorReward <- change_type(visitorReward)
typeof(visitorReward[,1])



cleanEx()
nameEx("condition_For_UCB")
### * condition_For_UCB

flush(stderr()); flush(stdout())

### Name: condition_For_UCB
### Title: Returns the arm with the highest bound
### Aliases: condition_For_UCB

### ** Examples

## Generates 1000 numbers from 2 binomial distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame( cbind(K1,K2) )
## Number of arms
K=2
## Init the S Matrix
S <- generate_Matrix_S(K)
S
## play arms uniformly
for(i in 1:nrow(visitorReward)){
S <- play_arm(i,arm=(i%%K+1),S,visitorReward)
}
## Results
S
proba_max_For_UCB(S=S,iter=i+1)
condition_For_UCB(S=S,iter=i+1)



cleanEx()
nameEx("condition_For_epsilonGreedy")
### * condition_For_epsilonGreedy

flush(stderr()); flush(stdout())

### Name: condition_For_epsilonGreedy
### Title: condition_For_epsilonGreedy
### Aliases: condition_For_epsilonGreedy

### ** Examples

## Generates 1000 numbers from 2 binomial distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame( cbind(K1,K2) )
## Number of arms
K=2
## Init the S Matrix
S <- generate_Matrix_S(K)
S
## play arms uniformly
for(i in 1:nrow(visitorReward)){
S <- play_arm(i,arm=(i%%K+1),S,visitorReward)
}
## Results
S
condition_For_epsilonGreedy(S=S)



cleanEx()
nameEx("condition_For_thompson_sampling")
### * condition_For_thompson_sampling

flush(stderr()); flush(stdout())

### Name: condition_For_thompson_sampling
### Title: condition_For_thompson_sampling
### Aliases: condition_For_thompson_sampling

### ** Examples

## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame( cbind(K1,K2) )
## Number of arms
K=2
## Init the S Matrix
S <- generate_Matrix_S(K)
S
## play arms uniformly
for(i in 1:nrow(visitorReward)){
S <- play_arm(i,arm=(i%%K+1),S,visitorReward)
}
## Results
S
## Choose next arm with thompson sampling policy
condition_For_thompson_sampling(S)
#Density
plot(density( rbeta(100, 1 +  S[1,1]*S[2,1], 1 + S[2,1] - S[1,1]*S[2,1])))
plot(density( rbeta(100, 1 +  S[1,2]*S[2,2], 1 + S[2,2] - S[1,2]*S[2,2])))



cleanEx()
nameEx("control_data_missing")
### * control_data_missing

flush(stderr()); flush(stdout())

### Name: control_data_missing
### Title: control_data_missing
### Aliases: control_data_missing

### ** Examples

## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rnorm(100, 30, .05)
K2 <- rnorm(100, 21, .05)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2) )
## Control
control_data_missing(visitorReward)
visitorReward[1,1]= NA
## Control
control_data_missing(visitorReward)




cleanEx()
nameEx("cumulativeRegret")
### * cumulativeRegret

flush(stderr()); flush(stdout())

### Name: cumulativeRegret
### Title: Return list of cumulative regret
### Aliases: cumulativeRegret

### ** Examples

## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rnorm(100, 30, .05)
K2 <- rnorm(100, 21, .05)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2) )
#Random choices
choice <- sample(c(1,2), 100, replace = TRUE)
cumulativeRegret(choice=choice,visitorReward=visitorReward)




cleanEx()
nameEx("data_control_K")
### * data_control_K

flush(stderr()); flush(stdout())

### Name: data_control_K
### Title: data_control_K
### Aliases: data_control_K

### ** Examples

## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rnorm(100, 30, .05)
## Define a dataframe of rewards
visitorReward <- as.data.frame(K1)
## Control
data_control_K(visitorReward)
K2 <- rnorm(100, 21, .05)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2) )
## Control
data_control_K(visitorReward,K=3)
## Control
data_control_K(visitorReward,K=2)




cleanEx()
nameEx("data_control_context_reward")
### * data_control_context_reward

flush(stderr()); flush(stdout())

### Name: data_control_context_reward
### Title: data_control_context_reward
### Aliases: data_control_context_reward

### ** Examples

## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rnorm(100, 30, .05)
K2 <- rnorm(100, 35, .05)
## Define a dataframe of rewards
visitorReward <- as.data.frame(K1)
## Define a dataframe of context
c1 <- rnorm(50, 35, .05)
dt <- as.data.frame(c1)
## Control
data_control_context_reward(dt=dt,visitorReward=visitorReward)
c1 <- rnorm(100, 30, .05)
dt <- as.data.frame(c1)
data_control_context_reward(dt=dt,visitorReward=visitorReward)




cleanEx()
nameEx("epsilonGreedy")
### * epsilonGreedy

flush(stderr()); flush(stdout())

### Name: epsilonGreedy
### Title: epsilonGreedy algorithm
### Aliases: epsilonGreedy

### ** Examples

## Generates 10000 numbers from 2 binomial  distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2) )
#Run epsilon Greedy algorithm
epsilonGreedy_alloc  <- epsilonGreedy(visitorReward,epsilon  = 0.25)
epsilonGreedy_alloc$S
barplot(table(epsilonGreedy_alloc$choice),main = "Histogram of choices",xlab="arm")
epsilonGreedy_alloc$time
epsilonGreedy_alloc$theta_hat
epsilonGreedy_alloc$theta



cleanEx()
nameEx("epsilonGreedy_bandit_object_evaluation")
### * epsilonGreedy_bandit_object_evaluation

flush(stderr()); flush(stdout())

### Name: epsilonGreedy_bandit_object_evaluation
### Title: epsilonGreedy_bandit_object_evaluation
### Aliases: epsilonGreedy_bandit_object_evaluation

### ** Examples

## Generates 1000 numbers from 2 binomial distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2) )
#Run epsilonGreedy algorithm with policy evaluation
epsilonGreedy_bandit_object_evaluation(visitorReward,epsilon = 0.25)




cleanEx()
nameEx("generate_Matrix_S")
### * generate_Matrix_S

flush(stderr()); flush(stdout())

### Name: generate_Matrix_S
### Title: Generate a S Matrix
### Aliases: generate_Matrix_S

### ** Examples

K = 2
generate_Matrix_S(K)




cleanEx()
nameEx("is_reward_are_boolean")
### * is_reward_are_boolean

flush(stderr()); flush(stdout())

### Name: is_reward_are_boolean
### Title: is_reward_are_boolean
### Aliases: is_reward_are_boolean

### ** Examples

## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rnorm(100, 30, .05)
K2 <- rnorm(100, 35, .05)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2))
is_reward_are_boolean(visitorReward)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
visitorReward <- as.data.frame(cbind(K1,K2))
is_reward_are_boolean(visitorReward)




cleanEx()
nameEx("linucb_bandit_object_evaluation")
### * linucb_bandit_object_evaluation

flush(stderr()); flush(stdout())

### Name: linucb_bandit_object_evaluation
### Title: linucb_bandit_object_evaluation
### Aliases: linucb_bandit_object_evaluation

### ** Examples

size.tot = 1000
set.seed(4649)                          # this makes the example exactly reproducible
x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
x2 = runif(size.tot, min=0, max=10)
x3 = runif(size.tot, min=0, max=10)
x4 = runif(size.tot, min=0, max=10)
dt = cbind(x1,x2,x3,x4)
#arm reward
arm_1 <-  as.vector(c(-1,9,-8,4))
K1 = 1/(1+exp(- crossprod(t(dt),arm_1))) # inverse logit transform of linear predictor
K1 = vapply(K1, function(x) rbinom(1, 1, x), as.integer(1L))
arm_2 <-  as.vector(c(-1,2,1,0))
K2 = 1/(1+exp(- crossprod(t(dt),arm_2))) # inverse logit transform of linear predictor
K2 = vapply(K2, function(x) rbinom(1, 1, x), as.integer(1L))
arm_3 <-  as.vector(c(-1,-5,1,10))
K3 = 1/(1+exp(- crossprod(t(dt),arm_3)))
K3 = vapply(K3, function(x) rbinom(1, 1, x), as.integer(1L))
visitorReward <-  data.frame(K1,K2,K3)
dt <- as.data.frame(dt)
linucb_bandit_object_evaluation(dt,visitorReward)



cleanEx()
nameEx("logitucb_bandit_object_evaluation")
### * logitucb_bandit_object_evaluation

flush(stderr()); flush(stdout())

### Name: logitucb_bandit_object_evaluation
### Title: logitucb_bandit_object_evaluation
### Aliases: logitucb_bandit_object_evaluation

### ** Examples

size.tot = 1000
set.seed(4649)                          # this makes the example exactly reproducible
x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
x2 = runif(size.tot, min=0, max=10)
x3 = runif(size.tot, min=0, max=10)
x4 = runif(size.tot, min=0, max=10)
dt = cbind(x1,x2,x3,x4)
#arm reward
arm_1 <-  as.vector(c(-1,9,-8,4))
K1 = 1/(1+exp(- crossprod(t(dt),arm_1))) # inverse logit transform of linear predictor
K1 = vapply(K1, function(x) rbinom(1, 1, x), as.integer(1L))
arm_2 <-  as.vector(c(-1,2,1,0))
K2 = 1/(1+exp(- crossprod(t(dt),arm_2))) # inverse logit transform of linear predictor
K2 = vapply(K2, function(x) rbinom(1, 1, x), as.integer(1L))
arm_3 <-  as.vector(c(-1,-5,1,10))
K3 = 1/(1+exp(- crossprod(t(dt),arm_3)))
K3 = vapply(K3, function(x) rbinom(1, 1, x), as.integer(1L))
visitorReward <-  data.frame(K1,K2,K3)
dt <- as.data.frame(dt)
logitucb_bandit_object_evaluation(dt=dt,visitorReward)



cleanEx()
nameEx("play_arm")
### * play_arm

flush(stderr()); flush(stdout())

### Name: play_arm
### Title: Assign an arm to an item
### Aliases: play_arm

### ** Examples

## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame( cbind(K1,K2) )
## Number of arms
K=2
## Init the S Matrix
S <- generate_Matrix_S(K)
S
## play arms uniformly
for(i in 1:nrow(visitorReward)){
S <- play_arm(i,arm=(i%%K+1),S,visitorReward)
}
## Results
S



cleanEx()
nameEx("proba_max_For_UCB")
### * proba_max_For_UCB

flush(stderr()); flush(stdout())

### Name: proba_max_For_UCB
### Title: Calculates, for each colonne of S (selected arm) an upper bound
###   according to the Hoeffding inequality (dependent on the iter
###   iteration). It is possible to adjust this bound via an alpha
###   parameter (default alpha = 1). Returns a vector of calculated upper
###   bounds
### Aliases: proba_max_For_UCB

### ** Examples

## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame( cbind(K1,K2) )
## Number of arms
K=2
## Init the S Matrix
S <- generate_Matrix_S(K)
S
## play arms uniformly
for(i in 1:nrow(visitorReward)){
S <- play_arm(i,arm=(i%%K+1),S,visitorReward)
}
## Results
S
proba_max_For_UCB(S=S,iter=i+1)



cleanEx()
nameEx("regretValue")
### * regretValue

flush(stderr()); flush(stdout())

### Name: regretValue
### Title: Return regret of chosen arm
### Aliases: regretValue

### ** Examples

## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rnorm(100, 30, .05)
K2 <- rnorm(100, 21, .05)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2) )
#regret of arm 1 for the fist item
regretValue(1,visitorReward[1,])
#'#regret of arm 1 for the fist item
regretValue(2,visitorReward[1,])



cleanEx()
nameEx("return_real_theta")
### * return_real_theta

flush(stderr()); flush(stdout())

### Name: return_real_theta
### Title: return_real_theta
### Aliases: return_real_theta

### ** Examples

size.tot = 1000
set.seed(4649)                          # this makes the example exactly reproducible
x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
x2 = runif(size.tot, min=0, max=10)
x3 = runif(size.tot, min=0, max=10)
x4 = runif(size.tot, min=0, max=10)
dt = cbind(x1,x2,x3,x4)
#arm reward
arm_1 <-  as.vector(c(-1,9,-8,4))
K1 = crossprod(t(dt),arm_1)
arm_2 <-  as.vector(c(-1,2,1,0))
K2 = crossprod(t(dt),arm_2)
arm_3 <-  as.vector(c(-1,-5,1,10))
K3 = crossprod(t(dt),arm_3)
visitorReward <-  data.frame(K1,K2,K3)
dt <- as.data.frame(dt)
return_real_theta(dt,visitorReward)



cleanEx()
nameEx("simpleRegret")
### * simpleRegret

flush(stderr()); flush(stdout())

### Name: simpleRegret
### Title: Return list of regret
### Aliases: simpleRegret

### ** Examples

## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rnorm(100, 30, .05)
K2 <- rnorm(100, 21, .05)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2) )
#Random choices
choice <- sample(c(1,2), 100, replace = TRUE)
simpleRegret(choice=choice,visitorReward=visitorReward)




cleanEx()
nameEx("thompson_sampling")
### * thompson_sampling

flush(stderr()); flush(stdout())

### Name: thompson_sampling
### Title: thompson_sampling
### Aliases: thompson_sampling

### ** Examples

## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame( cbind(K1,K2) )
thompson_sampling(visitorReward)



cleanEx()
nameEx("thompson_sampling_bandit_object_evaluation")
### * thompson_sampling_bandit_object_evaluation

flush(stderr()); flush(stdout())

### Name: thompson_sampling_bandit_object_evaluation
### Title: thompson_sampling_bandit_object_evaluation
### Aliases: thompson_sampling_bandit_object_evaluation

### ** Examples

## Generates 1000 numbers from 2 binomial distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2) )
#Run Thompson Sampling algorithm with policy evaluation
thompson_sampling_bandit_object_evaluation(visitorReward,alpha = 1, beta = 1 )




cleanEx()
nameEx("ucb_bandit_object_evaluation")
### * ucb_bandit_object_evaluation

flush(stderr()); flush(stdout())

### Name: ucb_bandit_object_evaluation
### Title: ucb_bandit_object_evaluation
### Aliases: ucb_bandit_object_evaluation

### ** Examples

## Generates 1000 numbers from 2 binomial distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2) )
#Run UCB algorithm with policy evaluation
ucb_bandit_object_evaluation(visitorReward,alpha = 1)




cleanEx()
nameEx("uniform_bandit")
### * uniform_bandit

flush(stderr()); flush(stdout())

### Name: uniform_bandit
### Title: Uniform algorithm
### Aliases: uniform_bandit

### ** Examples

## Generates 10000 numbers from 2 binomial  distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2) )
#Run Uniform algorithm
uniform_bandit_alloc  <- uniform_bandit(visitorReward)
uniform_bandit_alloc$S
uniform_bandit_alloc$time



cleanEx()
nameEx("uniform_bandit_object_evaluation")
### * uniform_bandit_object_evaluation

flush(stderr()); flush(stdout())

### Name: uniform_bandit_object_evaluation
### Title: uniform_bandit_object_evaluation
### Aliases: uniform_bandit_object_evaluation

### ** Examples

## Generates 1000 numbers from 2 binomial distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitorReward <- as.data.frame(cbind(K1,K2) )
#Run uniform bandit allocation with policy evaluation
uniform_bandit_object_evaluation(visitorReward)




### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
