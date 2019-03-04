
source("Bandit/R/RANDOM.R")
source("Bandit/R/regret.R")

random_bandit_object_evaluation <- function(visitorReward=visitorReward){

  random_alloc  <- random_bandit(visitorReward)

  cum_reg_random_alloc  <- cumulativeRegret(random_alloc$choice,visitorReward)


  return(list('random_alloc'=random_alloc ,'cum_reg_random_alloc'=cum_reg_random_alloc))
}
