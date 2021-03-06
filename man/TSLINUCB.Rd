% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TSLINUCB.R
\name{TSLINUCB}
\alias{TSLINUCB}
\title{TSLINUCB algorithm}
\usage{
TSLINUCB(dt, visitor_reward, alpha = 1, K = ncol(visitor_reward), iter = 10)
}
\arguments{
\item{dt}{Dataframe of integer or numeric values}

\item{visitor_reward}{Dataframe of integer or numeric values}

\item{K}{Integer value (optional)}

\item{iter}{Integer value (optional)}
}
\value{
\itemize{ List of element:
 \item choice: choices of TSLINUCB,
 \item proba: probability of the chosen arms,
 \item time: time of cumputation,
 \item theta_hat: coefficients estimated of each arm
 \item theta: real coefficients of each arm
 }
}
\description{
Control data in visitor_reward with \code{\link{BanditRewardControl}}
Stop if something is wrong.
\itemize{ At each iteration
 \item Sample a reward  from multivariate distribution (with covariance and means) for each arm f
 \item Choose the arm with the highest expected reward
 \item Receives a reward in visitor_reward for the arm and associated iteration
 \item Updates the results matrix S.
 }
Returns the calculation time.
Review the estimated, actual coefficient for each arm.
See also  \code{\link{ReturnRealTheta}},
Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library and  \code{\link{mvrnorm}} from MASS library.
}
\examples{
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
visitor_reward <-  data.frame(K1,K2,K3)
dt <- as.data.frame(dt)
TSLINUCB(dt,visitor_reward)
}
