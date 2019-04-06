#'plot_cum_regret_for_each_subgroupe
#'
#'plot cum regret for each subgroupe of ctreeucb object. See \code{\link{ctree}} using visitor_reward and dt values.
#'
#'@param ctree_ucb_object Dataframe of integer or numeric values
#'
#'@return
#'
#'@examples
## Generates 1000 numbers from 2 uniform distributions
#'size.tot = 1000
#'set.seed(4649)                          # this makes the example exactly reproducible
#'x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
#'x2 = runif(size.tot, min=0, max=10)
#'x3 = runif(size.tot, min=0, max=10)
#'x4 = runif(size.tot, min=0, max=10)
#'dt = cbind(x1,x2,x3,x4)
#arm reward
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = crossprod(t(dt),arm_1)
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = crossprod(t(dt),arm_2)
#'arm_3 <-  as.vector(c(-1,-5,1,10))
#'K3 = crossprod(t(dt),arm_3)
#'visitor_reward <-  data.frame(K1,K2,K3)
#'dt <- as.data.frame(dt)
#'temp <- ctreeucb(dt,visitor_reward)
#'plot_cum_regret_for_each_subgroupe(temp)
#'@export
#plot_cum_regret_for_each_subgroupe
plot_cum_regret_for_each_subgroupe <- function(ctree_ucb_object){


  dt <- ctree_ucb_object$ctreeucb_bandit_alloc$data_context

  choice <- ctree_ucb_object$ctreeucb_bandit_alloc$choice
  visitorReward <-  ctree_ucb_object$ctreeucb_bandit_alloc$data_reward

  print(length(levels(as.factor(dt$groups))))
  for(i in levels(as.factor(dt$groups))){
    print(i)

    temp_choice  <- dt[dt$groups == i,]$choice

    temp_visitor_reward <- visitorReward[dt$groups == i,]
    temp_cum_reg <- CumulativeRegret(temp_choice,temp_visitor_reward)
    plot(temp_cum_reg, type ='l', ylab = paste("cumulative regret for subgroup ",i,sep = ""))
    temp_choice  <- NULL
    temp_visitor_reward   <- NULL
    temp_cum_reg   <- NULL

  }
}
