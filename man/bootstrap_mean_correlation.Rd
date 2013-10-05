\name{bootstrap_mean_correlation}
\alias{bootstrap_mean_correlation}
\title{Bootstrap difference in average correlation of two sets of variables}
\usage{
  bootstrap_mean_correlation(set1, set2, data,
    iterations = 10000, ci = 0.95)
}
\arguments{
  \item{set1}{character string of variable names in set 1
  of data}

  \item{set2}{chracter string of variable names in set 2 of
  data}

  \item{data}{data.frame or matrix containing variables
  set1 and set2}

  \item{iterations}{positive integer indicating number of
  bootstrap iterations to run}

  \item{ci}{number between 0 and 1 indicating size of
  bootstrap confidence interval}
}
\description{
  The function gets average correlation for set1 and set2
  and obtains the difference Bootstrapping is used to get
  confidence intervals on this difference
}
\examples{
library(MASS)
r <- matrix(NA, ncol=6, nrow=6)
r[1:3, 1:3] <- .1 # set 1 population mean r
r[4:6, 4:6] <- .5 # set population mean r
r[1:3, 4:6] <- .3
r[4:6, 1:3] <- .3
diag(r) <- 1
colnames(r) <- c('a1', 'a2', 'a3', 'b1', 'b2', 'b3')
x <- 1:3
simulated_data <- mvrnorm(n=50, mu=rep(0, 6), Sigma=r)
colnames(simulated_data) <- colnames(r)
v <- list()
v$set1 <-  c('a1', 'a2', 'a3')
v$set2 <- c('b1', 'b2', 'b3')
bootstrap_mean_correlation(v$set1, v$set2, simulated_data)
}

