ConditionForKLUCB <- function(kl, S, d, upperbound, precision, max_iteration, visitor_reward){
  
  # Bisection method
  
  upperbound = upperbound
  reward = S[1,]
  value = reward
  count_iteration = 0
  kl_vec <- Vectorize(kl)
  
  while (count_iteration < max_iteration && upperbound - value > precision){
    m = (value + upperbound) / 2
    
    upperbound <- ifelse(kl_vec(reward, m) > d, m, upperbound)
    value <- ifelse(kl_vec(reward, m) <= d, m, value)
    
    count_iteration = count_iteration + 1
  }
  
  return ((value + upperbound) / 2)
  
}


# Kullback-Leibler divergence for Bernoulli distribution

kl_bernoulli <- function(p, q){
    
  epsilon <- 1e-16
  p = min(max(p, epsilon), 1 - epsilon)
  q = min(max(q, epsilon), 1 - epsilon)
  
  return(p * log(p/q) + (1 - p) * log((1 - p)/(1 - q)))
  
}

kl_ucb_bernoulli <- function(x, d, precision, S, max_iteration, visitor_reward){
  
  upperbound = min(1, kl_ucb_gaussian(x, d))
  
  return (ConditionForKLUCB(kl=kl_bernoulli, S=S, d=d, precision=precision, upperbound = upperbound,
                            max_iteration = max_iteration, visitor_reward = visitor_reward[j, ]
                            ))
}



# Kullback-Leibler divergence for Gaussian distribution
kl_ucb_gaussian <- function(x, d, sigma2=1){
  
  return ( x + sqrt(2 * sigma2 * d) )
  
}




