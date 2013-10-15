\name{boot_mean_cor_diff}
\alias{boot_mean_cor_diff}
\title{Bootstrap difference in average correlation of two sets of variables}
\usage{
  boot_mean_cor_diff(set1, set2, data, iterations,
    ci = 0.95)
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
  bootstrap confidence interval. Default value is .95
  representing a 95\% confidence interval.}
}
\value{
  an object of class boot_mean_cor_diff
  \item{boot_results}{object of class boot resulting from
  bootstrapping analysis} \item{cor_matrix1}{numeric
  matrix: correlation matrix between set1 variables}
  \item{cor_matrix2}{numeric matrix: correlation matrix
  between set2 variables} \item{mean_cor1}{numeric scalar:
  average correlation between set1 variables}
  \item{mean_cor2}{numeric scalar: average correlation
  between set2 variables} \item{set1, set2, data,
  iterations, ci}{copy of corresponding arguments}
}
\description{
  The function gets average correlation for set1 and set2
  and obtains the difference Bootstrapping is used to get
  confidence intervals on this difference
}
\examples{
data(twosetsim)
boot_mean_cor_diff(twosetsim$set1, twosetsim$set2,
twosetsim$data, iterations=1000, ci=.95)
}
\seealso{
  \code{\link{print.boot_mean_cor_diff}} for summaries of
  the returned object.
}

