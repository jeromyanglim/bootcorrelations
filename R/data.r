#' Simulated data with six variables divided into two sets
#'
#' A list with main element "data" that is simulated data containing six variables.
#' The data is drawn from multivariate standard normal distribution:
#' The first three variables a1, a2, a3 have average intercorrelation r=.1
#' The second three variables b1, b2, b3 have average intercorrelation r=.5
#' And across the two sets variables have intercorrelations r=.3
#' 
#' The elements in the list are:
#' \describe{
#' \item{set1}{character vector of variable names in set 1}
#' \item{set2}{character vector of variable names in set 2}
#' \item{data}{data frame of 100 cases by 6 variables}
#' }
#' @name twosetsim
#' @docType data
#' @keywords data
#' @examples 
#' data(twosetsim)
#' cor(twosetsim$data)
#' mean(cor(twosetsim$data[twosetsim$set1]))
#' mean(cor(twosetsim$data[twosetsim$set2]))
NULL