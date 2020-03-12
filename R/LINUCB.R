#'LINUCB algorithm
#'
#'Control data in visitor_reward with \code{\link{BanditRewardControl}}
#'Stop if something is wrong.
#' \itemize{ At each iteration
#'  \item Calculates the arm probabilities according to a linear regression of context in dt dataframe
#'  \item Choose the arm with the maximum upper bound (with alpha parameter)
#'  \item Receives a reward in visitor_reward for the arm and associated iteration
#'  \item Updates the results matrix S.
#'  }
#'Returns the calculation time.
#'Review the estimated, actual coefficient for each arm.
#'See also  \code{\link{ReturnRealTheta}},
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param dt Dataframe of integer or numeric values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item choice: choices of UCB,
#'  \item proba: probability of the chosen arms,
#'  \item time: time of cumputation,
#'  \item theta_hat: coefficients estimated of each arm
#'  \item theta: real coefficients of each arm
#'  }
#'
#'@examples
#'size.tot = 1000
#'set.seed(4649)                          # this makes the example exactly reproducible
#'x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
#'x2 = runif(size.tot, min=0, max=10)
#'x3 = runif(size.tot, min=0, max=10)
#'x4 = runif(size.tot, min=0, max=10)
#'dt = cbind(x1,x2,x3,x4)
#'#arm reward
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = crossprod(t(dt),arm_1)
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = crossprod(t(dt),arm_2)
#'arm_3 <-  as.vector(c(-1,-5,1,10))
#'K3 = crossprod(t(dt),arm_3)
#'visitor_reward <-  data.frame(K1,K2,K3)
#'dt <- as.data.frame(dt)
#'LINUCB(dt,visitor_reward)
#'@import tictoc
#'@export
#LINUCB
LINUCB <- function(dt, visitor_reward, alpha=1, K=ncol(visitor_reward),IsRewardAreBoolean = FALSE) {

  #control data
  DataControlK(visitor_reward, K = K)
  DataControlContextReward(dt, visitor_reward)

  #data formating
  visitor_reward <- as.matrix(visitor_reward)

  #Context matrix
  D <- as.matrix(dt)
  n <- nrow(dt)
  n_f <- ncol(D)

  #Keep the past choice for regression
  choices = list(rep.int(0,n))
  rewards = list(rep.int(0,n))
  proba = list(rep.int(0,n))

  #parameters to modelize
  th_hat = array(0, c(K,n_f))
  colnames(th_hat) <- colnames(dt)
  rownames(th_hat) <- colnames(visitor_reward)

  #regression variable
  b <- matrix(0, K, n_f)
  A <- array(0, c(n_f, n_f, K))

  #tempory variable
  p = list(rep.int(0, K))

  #time keeper
  tic()

  #initialization
  for (j in 1:K) {
    A[,,j]= diag(n_f)
  }


  for (i in 1:n) {
    x_i = D[i,]
    for (j in 1:K) {
      A_inv      = solve(A[,,j])
      th_hat[j,] = A_inv %*% b[j,]
      ta         = t(x_i) %*% A_inv %*%  x_i
      a_upper_ci = alpha * sqrt(ta)             # upper part of variance interval
      a_mean     = th_hat[j,] %*% x_i              # current estimate of mean
      p[j]       = a_mean + a_upper_ci         # top CI
    }

    # choose the highest,
    choices[i] = which.max(p)

    #save probability
    proba[i] = max(unlist(p))

    # see what kind of result we get
    rewards[i] = visitor_reward[i,as.integer(choices[i])]

    # update the input vector
    A[,,as.integer(choices[i])] = A[,,as.integer(choices[i])]  + x_i %*% t(x_i) #covariance matrix
    b[as.integer(choices[i]),] = b[as.integer(choices[i]),] +  x_i * as.numeric(rewards[i])


  }
  time <- toc()

  #return real theta from a rigide regression
 if(IsRewardAreBoolean == FALSE) th <- ReturnRealTheta(dt=dt,visitor_reward=visitor_reward, option = "linear")


  #return real theta from a logit regression
  if(IsRewardAreBoolean == TRUE) th <- ReturnRealTheta(dt=dt,visitor_reward=visitor_reward, option = "linear")

  #return  data , models, groups and results
  return (list('proba' = unlist(proba),'theta_hat'=th_hat,'theta'=th,'choice'=unlist(choices),'time'=(time$toc - time$tic)))

}
