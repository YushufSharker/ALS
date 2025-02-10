
# function for calculating mean and variance in log scale from the original scale
# Assumed the data in the original scale is log-normally distributed
# Author: Yushuf Sharker, MyAIbou 3.0 assisted code

#' Convert to Lognormal Distribution Parameters
#'
#' This function converts the given mean and variance of a normal distribution
#' to the parameters of the corresponding lognormal distribution.
#'
#' To be used for sample size calculation
#'
#' @param meanx A numeric value representing the mean of the original scale data.
#' @param varx A numeric value representing the variance of the original scale data.
#'
#' @return A named vector with the following elements:
#' \item{meanx}{Mean of the original scale data}
#' \item{varx}{Variance of the original scale data}
#' \item{mu}{Mean of the log-transformed data}
#' \item{sigma2}{Variance of the log-transformed data}
#' \item{sigma}{Standard deviation of the log-transformed data}
#' \item{GM}{Geometric mean of the original scale data}
#' \item{GMSD}{Geometric standard deviation of the original scale data}
#'
#' @examples
#' meanx <- 37.0
#' varx <- 29.5
#' convert_to_lognormal(meanx, varx)
#'
#'Check the finding with SAS version privided in
#'https://blogs.sas.com/content/iml/2014/06/04/simulate-lognormal-data-with-specified-mean-and-variance.html

convert_to_lognormal <- function(meanx, varx) {
  varlog <- round(log(varx / meanx ^ 2 + 1), 4)
  meanlog <- round(log(meanx) - (varlog / 2), 4)
  sigmalog <- round(sqrt(varlog), 4)
  gm <- round(exp(meanlog), 4)
  gmsd <- round((exp(sigmalog)), 4)
  gmvar <- round((exp(varlog)), 4)
  return(
    c(
      meanx = meanx,
      varx = varx,
      mu = meanlog,
      sigma2 = varlog,
      sigma = sigmalog,
      GM = gm,
      GMVar = gmvar,
      GMSD = gmsd
    )
  )
}
