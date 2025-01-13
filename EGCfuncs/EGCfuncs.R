## Emma G. Cunningham General Functions Library ##
#############################################
# Purpose: Store custom functions written   #
# for many purposes by Emma Cunningham      #
#############################################

# _________________________________________________________________
## Bayes Factor R Value Threshold Calculation
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
# _________________________________________________________________

