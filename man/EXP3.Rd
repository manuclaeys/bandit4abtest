% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EXP3.R
\name{EXP3}
\alias{EXP3}
\title{EXP3 algorithm}
\usage{
EXP3(visitor_reward, K = ncol(visitor_reward), gamma = 0.05)
}
\arguments{
\item{visitor_reward}{Dataframe of integer or numeric values}

\item{K}{Integer value (optional)}

\item{gamma}{Numeric value (optional)}
}
\value{
\itemize{ List of element:
 \item S:numerical matrix of results ,
 \item choice: choices of EXP3,
 \item proba: probability of the chosen arms,
 \item time: time of cumputation,
 \item theta_hat: mean estimated of each arm
 \item theta: real mean of each arm
 \item weight : weight coefficient of each arm
 }
}
\description{
Exponential  Weights  for  Exploration  and  Exploitation (EXP3) bandit strategy. Uses a list of weigths which evolve according
to arm's reward. The gamma parameter is a coefficient for balancing between exploitation and exploration.
Control data in visitor_reward with \code{\link{IsRewardAreBoolean}}
Stop if something is wrong.
Generate a matrix to save the results (S).
\itemize{ At each iteration
 \item Update weight parameter for each arm
 \item Choose randomly an arm according to the distribution of proba
 \item Receives a reward in visitor_reward for the arm and associated iteration
 \item Updates the results matrix S.
 }
Returns the calculation time.
Review the estimated, actual averages and number of choices for each arm.
See also  \code{\link{ConditionForEXP3}}, \code{\link{GenerateMatrixS}}, and \code{\link{PlayArm}}.
Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
}
\examples{
## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitor_reward <- as.data.frame( cbind(K1,K2) )
EXP3_alloc <- EXP3(visitor_reward)
EXP3_alloc$S
EXP3_alloc$time
EXP3_alloc$theta
EXP3_alloc$theta_hat
}
