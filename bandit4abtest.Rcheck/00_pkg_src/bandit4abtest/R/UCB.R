#'UCB algorithm
#'
#'Control data in visitorReward with \code{\link{bandit_reward_control}}
#'Stop if something is wrong.
#'Generate a matrix to save the results (S).
#' \itemize{ At each iteration
#'  \item Calculates the arm probabilities
#'  \item Choose the arm with the maximum upper bound (with alpha parameter)
#'  \item Receives a reward in visitorReward for the arm and associated iteration
#'  \item Updates the results matrix S.
#'  }
#'Returns the calculation time.
#'Review the estimated, actual averages and number of choices for each arm.
#'See also \code{\link{condition_For_UCB}}, \code{\link{generate_Matrix_SK}},
#'\code{\link{proba_max_For_UCB}} and \code{\link{play_arm}}.
#'Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'
#'@param visitorReward Dataframe of integer or numeric values
#'@param K Integer value (optional)
#'@param alpha Numeric value (optional)
#'
#'@return
#' \itemize{ List of element:
#'  \item S:numerical matrix of results ,
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
#'visitorReward <- as.data.frame(cbind(K1,K2) )
#'#Run UCB algorithm
#'ucb_alloc  <- UCB(visitorReward,alpha = 10)
#'ucb_alloc$S
#'barplot(table(ucb_alloc$choice),main = "Histogram of choices",xlab="arm")
#'#Upper bound for arm 2 according iterations (red line is the real mean)
#'plot(x=c(1:length(ucb_alloc$choice[ucb_alloc$choice==2])),
#'   ucb_alloc$proba[ucb_alloc$choice==2],
#'   type='l',xlab = 'Time',ylab = 'Upper bound of arm 2')
#'   lines(c(1:length(ucb_alloc$choice[ucb_alloc$choice==2])),rep(mean(K2),length(ucb_alloc$choice[ucb_alloc$choice==2])),col="red")
#'ucb_alloc$time
#'ucb_alloc$theta_hat
#'ucb_alloc$theta
#'@import tictoc
#'@export
#UCB
UCB <- function(visitorReward, K=ncol(visitorReward) , alpha = 1){
  bandit_reward_control(visitorReward = visitorReward, K= K)

  #data formating
  visitorReward <- as.matrix(visitorReward)

  #keep list of choice
  choice <- c()
  proba <- c()
  S <-  generate_Matrix_S(K)

  tic()

  if(K >= nrow(visitorReward)){
    warning(" more arm than visitors !")

    for(j in 1:nrow(visitorReward)){
      proba[j] <-  max(proba_max_For_UCB(S=S,iter=j,alpha=alpha,K))
      S <- play_arm(j,j,S,visitorReward)        #handle case where there is more arm than visitors
    }
    choice[1:nrow(visitorReward)] <- c(1:nrow(visitorReward))

    if(K>nrow(visitorReward)){
      S[,c(j:K) ] <- 0
    }

    return(list('S'=S,'choice'= choice))

  }else{

    ###initialisation
    for(j in 1:K){
      proba[j] <- max(proba_max_For_UCB(S=S,iter=j,alpha,K))
      S <- play_arm(iter=j,arm=j,S=S,visitorReward)         #check if an arm have already been played
      # message(S)
      choice[j] <- j
      }

    for(i in (K+1):nrow(visitorReward)){
      #    message(S)
      choice[i] <- condition_For_UCB(S,iter=i,alpha=alpha,K)
      #   message( choice[i])
      proba[i] <-  max(proba_max_For_UCB(S=S,iter=i,alpha=alpha,K))
      S <- play_arm(iter=i,arm=choice[i] ,S,visitorReward)

    }

    time <- toc()

    #coef estimate
    th_hat=S[1,]

    #real coef
    th = colMeans(visitorReward)

    message("th_hat")
    message(th_hat)
    message("th real")
    message(th)

    return(list('S'=S,'choice'= choice,'proba' = proba,'time'=(time$toc - time$tic),'theta_hat'=th_hat,'theta'=th))

  }
}


