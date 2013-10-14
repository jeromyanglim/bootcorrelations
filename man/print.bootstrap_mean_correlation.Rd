\name{print.bootstrap_mean_correlation}
\alias{print.bootstrap_mean_correlation}
\title{Print output for bootstrap_mean_correlation object}
\usage{
  \method{print}{bootstrap_mean_correlation} (x,
    digits = 3, verbose = FALSE, ...)
}
\arguments{
  \item{x}{object of class bootstrap_mean_correlation}

  \item{digits}{positive intenger: number of decimal points
  to display. Numbers are passed to round(x, digits)}

  \item{verbose}{logical. Only print correlation matrices
  if TRUE.}

  \item{...}{further arguments passed to or from other
  methods (not currently used)}
}
\description{
  Print output including descriptive statistics and
  bootstrap results
}
\examples{
data(twosetsim)
fit <- bootstrap_mean_correlation(twosetsim$set1, twosetsim$set2, twosetsim$data, iterations=1000)
print(fit, verbose=TRUE)
}

