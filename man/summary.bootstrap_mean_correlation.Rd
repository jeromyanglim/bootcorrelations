\name{summary.bootstrap_mean_correlation}
\alias{summary.bootstrap_mean_correlation}
\title{Summary output for bootstrap_mean_correlation object}
\usage{
  \method{summary}{bootstrap_mean_correlation} (x,
    digits = 3, verbose = FALSE)
}
\arguments{
  \item{x}{object of class bootstrap_mean_correlation}

  \item{digits}{positive intenger: number of decimal points
  to display. Numbers are passed to round(x, digits)}

  \item{verbose}{logical. Only print correlation matrices
  if TRUE.}
}
\description{
  Summary output including descriptive statistics and
  bootstrap results
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
fit <- bootstrap_mean_correlation(v$set1, v$set2, simulated_data)
summary(fit, verbose=TRUE)
}

