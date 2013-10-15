\name{print.boot_mean_cor_diff}
\alias{print.boot_mean_cor_diff}
\title{Print output for boot_mean_cor_diff object}
\usage{
  \method{print}{boot_mean_cor_diff} (x, digits = 3,
    verbose = TRUE, ...)
}
\arguments{
  \item{x}{object of class boot_mean_cor_diff}

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
fit <- boot_mean_cor_diff(twosetsim$set1, twosetsim$set2,
       twosetsim$data, iterations=1000)
print(fit, verbose=TRUE)
}

