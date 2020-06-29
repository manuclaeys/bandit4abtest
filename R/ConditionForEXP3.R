ConditionForEXP3 <- function(S, gamma=0.2, K=ncol(S), prob) {
  
  return(sample(1:K, size=1, replace=TRUE, prob = prob))
  
} 


