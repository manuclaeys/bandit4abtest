source("Bandit/R/RANDOM.R")
source("Bandit/R/regret.R")

RandomBanditObjectEvaluation <- function(visitorReward = visitorReward) {
  randomAlloc <- RandomBandit(visitorReward)
  cumRegRandomAlloc <- CumulativeRegret(random_alloc$choice, visitorReward)
  return(list('randomAlloc' = randomAlloc, 'cumRegRandomAlloc' = cumRegRandomAlloc))
}
