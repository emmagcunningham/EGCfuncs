

#' Bayes Factor R Value Threshold Calculation
#'
#' This function calculates the Bayes Factor test statistic thresholds (null and alternative) for R values.
#' It takes a dataframe and a method name as inputs and returns the thresholds for null acceptance and rejection regions.
#'
#' @param d A dataframe containing the data to calculate thresholds. The number of rows (\code{nrow(d)}) is used as the sample size.
#' @param method A character string indicating the method used for calculation. Defaults to \code{"Default Method"}.
#'
#' @details
#' The function calculates two thresholds:
#' \itemize{
#'   \item \code{alt_thresh}: The R value threshold where the Bayes Factor indicates strong evidence for the alternative hypothesis (\code{BF > 3}).
#'   \item \code{null_thresh}: The R value threshold where the Bayes Factor indicates strong evidence for the null hypothesis (\code{BF < 1/3}).
#' }
#'
#' These thresholds are computed by optimizing the R value to minimize the squared difference between the Bayes Factor and the target values (3 for \code{alt_thresh} and 1/3 for \code{null_thresh}).
#'
#' For more information on the Bayes Factor calculations, see \code{\link[BayesFactor]{ttest.tstat}}.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{alt_thresh}}{The R value threshold for null rejection (\code{|r| > alt_thresh}).}
#'   \item{\code{null_thresh}}{The R value threshold for null acceptance (\code{|r| < null_thresh}).}
#' }
#'
#' @examples
#' # Example usage:
#' # Create a dataframe with 100 rows:
#' df <- data.frame(x = rnorm(100))
#' result <- bayesThreshold(df, method = "Example Method")
#' print(result)
#'
#' @importFrom psych r2t
#' @importFrom BayesFactor ttest.tstat
#' @export
bayesThreshold <- function(d, method = "Default Method") {
  library(psych)
  library(BayesFactor)
  main <- ""
  if (nchar(main) == 0) {
    alt_thresh <- suppressMessages({
      suppressWarnings({
        optimise(function(r_val) {
          (3 - BayesFactor::ttest.tstat(psych::r2t(r_val,
                                                   nrow(d)), nrow(d), simple = TRUE))^2
        }, c(1e-06, 0.99999))$minimum
      })
    })
    null_thresh <- suppressMessages({
      suppressWarnings({
        optimise(function(r_val) {
          (0.333333333333 - BayesFactor::ttest.tstat(psych::r2t(r_val,
                                                                nrow(d)), nrow(d), simple = TRUE))^2
        }, c(1e-06, 0.99999))$minimum
      })
    })
    main <- paste(
      "Method:", method,
      "-- Approximate null acceptance region |r|<",
      round(null_thresh, 2),
      "; null rejection region |r|>",
      round(alt_thresh, 2), sep = " "
    )
    print(main)
  }

  # Return the thresholds as a list
  return(list(
    alt_thresh = alt_thresh,
    null_thresh = null_thresh
  ))
}
