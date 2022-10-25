#'LINUCB_rejection_sampling
#'
#'LINUCB algorithme with rejection sampling method.
#'Exclud any choices which not corresponds to real exepriments in dataset
#'Stop if something is wrong.
#'Generate a matrix to save the results (S).
#' \itemize{ At each iteration,
#'  \item Calculates the arm probabilities,
#'  \item Choose the arm with the maximum upper bound (with alpha parameter)
#'  \item Receives a reward in visitor_reward for the arm and associated iteration
#'  \item Updates the results matrix S.
#'  }
#'Returns the calculation time.
#'Review the estimated, actual averages and number of choices for each arm.
#'See also \code{\link{ConditionForUCB}}, \code{\link{GenerateMatrixS}},
#'\code{\link{ProbaMaxForUCB}} and \code{\link{PlayArm}}.
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
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
#'  \item theta_hat: mean estimated of each arm
#'  \item theta: real mean of each arm
#'  }
#'
#'@examples
#'## Generates 10000 numbers from 2 binomial  distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#remove data
#'temp_list <- sample(1:nrow(visitor_reward), 500, replace = FALSE, prob = NULL)
#'visitor_reward$K1[temp_list] <- NA
#'visitor_reward$K2[-temp_list] <- NA
#'#run ucb on missing data
#'ucb_rejection_sampling_alloc  <- UCB_rejection_sampling(visitor_reward,alpha = 10)
#'@import tictoc
#'@export
LINUCB_rejection_sampling <- function(dt, visitor_reward, alpha=1, K=ncol(visitor_reward),IsRewardAreBoolean = FALSE) {

  #control data
  DataControlContextReward(dt, visitor_reward)

  #data formating
  visitorReward <- as.matrix(visitor_reward)

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
  temp_i <- 0

  library(matlib)

  cat("début",'\n')
  #time keeper
  library(tictoc)
  tic()
  print(i)
  #initialization
  for (j in 1:K) {
    A[,,j]= diag(n_f)
  }
  cat("A=",A,'\n')
  for (i in 1:n) {
    x_i = D[i,]
    cat("x_i",x_i,'\n')
    for (j in 1:K) {
      tryCatch({
        A_inv      =  inv(t(A[,,j])) #solve(A[,,j])
      },
      error = function(e){
        message("Warning ! Error in solve.default(A[, , j]) ")
        A_inv      = A_inv}
      )

      th_hat[j,] = A_inv %*% b[j,]
      ta         = t(x_i) %*% A_inv %*%  x_i
      a_upper_ci = alpha * sqrt(abs(ta))             # upper part of variance interval
      a_mean     = th_hat[j,] %*% x_i              # current estimate of mean
      p[j]       = a_mean + a_upper_ci         # top CI

      # cat("Arm",j,'\n')
      # cat("A_inv=",A_inv,'\n')
      # cat("th_hat=",as.character(th_hat[j,]),'\n')
      # cat("ta",ta,'\n')
      #  cat("a_upper_ci",a_upper_ci,'\n')
      #  cat("a_mean",a_mean,'\n')
      # cat("prob",as.character( p[j] ),'\n')
    }

    #try each arm
    for(k in 1:K){
      if((k  %in% choices)==FALSE){
        cat('test',k,'\n')
        choices[i] = k
      }else{
        # choose the highest,
        choices[i] = which.max(p)
      }
    }


    cat("choice",as.character(choices[i]),'\n')

    #save probability
    proba[i] = max(unlist(p))
    #  cat("proba",as.character(p),'\n')

    ####Rejection sampling

    ### None empty reward ###
    if(is.na(visitorReward[i,as.integer(choices[i])])==FALSE){
      cat("None empty reward",'\n')
      temp_i = temp_i +1
      #  cat("temp_i",temp_i,'\n')
      # see what kind of result we get
      rewards[i] = visitorReward[i,as.integer(choices[i])]
      #  cat("rewards",as.character(rewards[i]),'\n')

      # update the input vector
      A[,,as.integer(choices[i])] = A[,,as.integer(choices[i])]  + x_i %*% t(x_i)
      # cat("update the input vector A",A[,,as.integer(choices[i])],'\n')
      b[as.integer(choices[i]),] = b[as.integer(choices[i]),] +  x_i * as.numeric(rewards[i])
      #  cat("b ",b[as.integer(choices[i]),],'\n')
    }

    if(is.na(visitorReward[i,as.integer(choices[i])])==TRUE){
      cat("Empty reward",'\n')
      choices[i] = NA
      proba[i] = NA
    }
  }

  time <- toc()

  #return real theta from a rigide regression
  if(IsRewardAreBoolean == FALSE) th <- ReturnRealTheta(dt=dt,visitor_reward=visitor_reward, option = "linear")


  #return real theta from a logit regression
  if(IsRewardAreBoolean == TRUE) th <- ReturnRealTheta(dt=dt,visitor_reward=visitor_reward, option = "linear")

  #return  data , models, groups and results
  return (list('proba' = unlist(proba),'theta_hat'=th_hat,'theta'=th,'choice'=unlist(choices),'time'=(time$toc - time$tic)))


}

