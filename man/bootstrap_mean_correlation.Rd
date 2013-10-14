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
\value{
  an object of class bootstrap_mean_correlation
}
\description{
  The function gets average correlation for set1 and set2
  and obtains the difference Bootstrapping is used to get
  confidence intervals on this difference
}
\examples{
data(twosetsim)
bootstrap_mean_correlation(twosetsim$set1, twosetsim$set2, twosetsim$data, iterations=1000, ci=.95)
}
\seealso{
  \code{\link{print.bootstrap_mean_correlation}} for
  summaries of the returned object.
}

